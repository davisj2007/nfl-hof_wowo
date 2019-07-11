###Summary---------------------
#Quantifying the relative impact of HOF QBs using with or without you
#Jacob Davis
#07-11-2019


###load packages and get rid of scientific notation
options(scipen=999) #No scientific notation
library(tidyverse) #data science tools
library(nflscrapR) #DATA!!!!!
library(scales) #this makes plotting easier
library(tidymodels) #easy(ier) statistical models
library(ggbeeswarm) #cool scatterplots
library(ggrepel) #better annotations
library(prediction) #simple wrapper for predictions from models

###change up the ggplot theme to my new weird one!-----------------------------------------
##extrafont::font_import()
extrafont::loadfonts()

theme_jake <- theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Permanent Marker"),
        plot.title  = element_text(family = "Permanent Marker", size = 20),
        plot.subtitle = element_text(family = "Permanent Marker"),
        axis.text = element_text(family = "Permanent Marker", color = "#54585A"),
  )

theme_set(theme_jake)

####load data------------------------------------------
#reg_pbp_2009 <- season_play_by_play(2009)
#reg_pbp_2010 <- season_play_by_play(2010)
#reg_pbp_2011 <- season_play_by_play(2011)
#reg_pbp_2012 <- season_play_by_play(2012)
#reg_pbp_2013 <- season_play_by_play(2013)
#reg_pbp_2014 <- season_play_by_play(2014)
#reg_pbp_2015 <- season_play_by_play(2015)
#reg_pbp_2016 <- season_play_by_play(2016)
#reg_pbp_2017 <- season_play_by_play(2017)
#reg_pbp_2018 <- season_play_by_play(2018)

#create master file
#data_complete <- bind_rows(reg_pbp_2009,reg_pbp_2010,reg_pbp_2011,reg_pbp_2012,reg_pbp_2013,reg_pbp_2014,
#reg_pbp_2015,reg_pbp_2016,reg_pbp_2017,reg_pbp_2018)

##so I don't have to wait on the API anymore
data_complete <- vroom::vroom("~/Desktop/R Projects/NFL/Data/pbp_data_20092018.csv")

###We need a few datasets
#first up, isolate quarterbacks
data_qb <- data_complete %>% 
  filter(PassAttempt == 1 & PlayType != "No Play" & !is.na(Passer) & !is.na(EPA) & !is.na(WPA)) %>%
  mutate(pre.prob = ifelse(posteam == HomeTeam, 
                           Home_WP_pre, 
                           Away_WP_pre),
         PassOutcome = case_when(
           PassOutcome == "Incomplete Pass" ~ "Incomplete",
           PassOutcome == "Complete" ~ "Complete"
         )) %>%
  select(-(sp:PuntResult),-(Rusher:RunGap), 
         -(ReturnResult:DefTeamScore), -(Timeout_Indicator:ExpPts))

####Let's check out how receivers fare with and without hall of fame QBs
#Let's make a list of "HOF"ers
hof.qbs = c("T.Brady", "B.Roethlisberger", "P.Manning", 
            "E.Manning", "A.Rodgers", "P.Rivers", 
            "M.Ryan","R.Wilson", "D.Brees", "T.Romo")

#filter our original dataset to just these guys, fDor later
data_qb_hof <- data_qb %>%
  filter(Passer %in% hof.qbs)

#create an indicator variable of HOF in original dataset as well
data_qb_flag <- data_qb %>%
  mutate(hof = ifelse(Passer %in% hof.qbs, 1, 0))

###find all receivers that were thrown 20 or more passes by at least two QBs
combos <- data_qb %>% 
  filter(!is.na(Passer) & !is.na(Receiver)) %>%
  group_by(Receiver_ID, Passer_ID, Passer) %>% 
  summarize(n = n()) %>% 
  filter(n > 20) %>% group_by(Receiver_ID) %>% 
  mutate(passers = n()) %>% 
  ungroup() %>%
  filter(passers > 2) %>% select(-passers)

###Find all the receivers that fit the above criteria AND 
###one of those passers was in our HOF list
tag <- combos %>% filter(Passer %in% hof.qbs) %>% 
  group_by(Receiver_ID,Passer_ID , Passer) %>%
  select(Receiver_ID,Passer_ID , Passer) %>%
  unite(combo_ID, Receiver_ID, Passer_ID, sep = "_", remove = FALSE)

###Limit our original combos dataset to just those receivers 
###but keep the other QBs too
desired <- combos %>% 
  mutate(wowo = ifelse(
    Receiver_ID %in% tag$Receiver_ID,1,0)) %>%
  filter(wowo == 1) %>%
  select(-wowo) %>%
  unite(combo_ID, Receiver_ID, Passer_ID, sep = "_", remove = FALSE)

###Identify each receivers with and without status for each QB
#There is probably a better way to do this, but I don't know what that is

#first, isolate all receivers based on whether they played with a HOF
ben <- tag %>%
  filter(Passer_ID == "00-0022924")

brady <- tag %>%
  filter(Passer_ID == "00-0019596")

peyton <- tag %>%
  filter(Passer_ID == "00-0010346")

eli <- tag %>%
  filter(Passer_ID == "00-0022803")

rodgers <- tag %>%
  filter(Passer_ID == "00-0023459")

rivers <- tag %>%
  filter(Passer_ID == "00-0022942")

romo <- tag %>%
  filter(Passer_ID == "00-0021678")

ryan <- tag %>%
  filter(Passer_ID == "00-0026143")

brees <- tag %>%
  filter(Passer_ID == "00-0020531")

wilson <- tag %>%
  filter(Passer_ID == "00-0029263")

#create new columns of receivers with, without, and NA and then gather to long
withorwithout <- desired %>%
  mutate(Roethlisberger = case_when(
    Passer_ID == "00-0022924" ~ "With",
    Passer_ID != "00-0022924" & Receiver_ID %in% ben$Receiver_ID ~ "Without"),
    Brady = case_when(
      Passer_ID == "00-0019596" ~ "With",
      Passer_ID != "00-0019596" & Receiver_ID %in% brady$Receiver_ID ~ "Without"),
    Brees = case_when(
      Passer_ID == "00-0020531" ~ "With",
      Passer_ID != "00-0020531" & Receiver_ID %in% brees$Receiver_ID ~ "Without"),
    E.Manning = case_when(
      Passer_ID == "00-0022803" ~ "With",
      Passer_ID != "00-0022803" & Receiver_ID %in% eli$Receiver_ID ~ "Without"),
    P.Manning = case_when(
      Passer_ID == "00-0010346" ~ "With",
      Passer_ID != "00-0010346" & Receiver_ID %in% peyton$Receiver_ID ~ "Without"),
    Ryan = case_when(
      Passer_ID == "00-0026143" ~ "With",
      Passer_ID != "00-0026143" & Receiver_ID %in% ryan$Receiver_ID ~ "Without"),
    Wilson = case_when(
      Passer_ID == "00-0029263" ~ "With",
      Passer_ID != "00-0029263" & Receiver_ID %in% wilson$Receiver_ID ~ "Without"),
    Romo = case_when(
      Passer_ID == "00-0021678" ~ "With",
      Passer_ID != "00-0021678" & Receiver_ID %in% romo$Receiver_ID ~ "Without"),
    Rivers = case_when(
      Passer_ID == "00-0022942" ~ "With",
      Passer_ID != "00-0022942" & Receiver_ID %in% rivers$Receiver_ID ~ "Without"),
    Rodgers = case_when(
      Passer_ID == "00-0023459" ~ "With",
      Passer_ID != "00-0023459" & Receiver_ID %in% rodgers$Receiver_ID ~ "Without")
  ) %>%
  gather(Roethlisberger:Rodgers, key = "hof", value = "wowo")

#remove all NA values
withorwithout2 <- withorwithout %>% na.omit()

#plot the number of receivers in the analysis
withorwithout2 %>% 
  filter(wowo == "With") %>% 
  group_by(hof) %>% 
  count() %>%
  ggplot(aes(reorder(hof, n), n)) + 
  #geom_col(aes(fill = -n)) +
  geom_linerange(aes(ymin = 0, ymax = n), size = 1, color = "blue" )+
  geom_point(size = 5, color = "blue") + 
  coord_flip() + 
  labs(x = element_blank(),
       y = element_blank(),
       title = "Brady's rotating cast of pass-catchers",
       subtitle = "# of receivers that played with the HOF and at least one other QB, min 20 targets",
       caption = "SOURCE: nflscrapR")+
  theme(legend.position = "none")

ggsave("no_receivers.jpeg")


#Join back to the original play by play QB data, but only keep relevant WRs
#Cast variables to factors and relevel to make regression easier to interpret
data_mod <- right_join(data_qb, withorwithout2, 
                       by = c("Receiver_ID" = "Receiver_ID", "Passer_ID" = "Passer_ID")) %>%
  mutate(PassOutcome = as.factor(PassOutcome),
         PassOutcome = fct_relevel(PassOutcome, "Incomplete", "Complete"),
         wowo = as.factor(wowo),
         wowo = fct_relevel(wowo, "Without", "With")) %>%
  filter(!is.na(EPA)) %>%
  select(-(combo_ID:n))

#note that data_mod only keeps HOF pbp data BUT it repeats "without"
#plays for recievers who played with more than one HOF QB

#summary table for each HOF QB
wowo.table <- data_mod %>%
  group_by(hof, wowo) %>%
  summarize(EPA.per.attempt = mean(EPA, na.rm = TRUE),
            EPA.per.completion = mean(ifelse(PassOutcome == "Complete", EPA, 0), na.rm = TRUE),
            WPA.per.attempt = mean(WPA, na.rm = TRUE),
            WPA.per.completion = mean(ifelse(PassOutcome == "Complete", WPA, 0), na.rm = TRUE)) %>%
  na.omit()


####Let's do some EDA on our new dataset----------------------------------------------------

#;plot average EPA trend
data_qb %>% 
  ggplot(aes(Season, EPA)) + 
  geom_smooth(color = "purple", method = "gam") + 
  expand_limits(y = c(0, .3)) + 
  labs(x = element_blank(),
       y = "EPA per attempt\n",
       title = "QBs are getting better",
       subtitle = "Linear trend of EPA for all passing attempts",
       caption = "SOURCE: nflscrapR")

ggsave("EPA_over_time.jpeg")

#break out the plot by HOF vs Non-HOF
data_qb_flag %>% 
  ggplot(aes(x = Season, y = EPA)) + 
  geom_smooth(aes(color = as_factor(hof)), method = "gam") + 
  scale_color_discrete(labels = c("Non-HOF", "HOF")) + 
  labs(x = element_blank(),
       y = "EPA per attempt\n",
       color = element_blank(),
       title = "But the gains are mostly in non-HOFers...",
       subtitle = "Linear trend of EPA for all passing attempts, by HOF tier",
       caption = "SOURCE: nflscrapR") + 
  expand_limits(y = c(0, .3)) + 
  theme(legend.position = "bottom")

ggsave("EPA_over_time?_byHOF.jpeg")


#Plot the average average by HOF vs non-HOF
data_qb_flag %>% 
  group_by(hof) %>%
  summarize(EPA = round(mean(EPA, na.rm = TRUE),2)) %>%
  ggplot(aes(as_factor(hof), EPA)) +
  geom_col() +
  geom_text(aes(label = EPA), 
            nudge_y = .01, family = "Permanent Marker") + 
  scale_x_discrete(labels = c("Non-HOF", "HOF")) + 
  expand_limits(y = c(0, .3)) + 
  labs(x = element_blank(),
       y = "EPA per attempt\n",
       caption = "SOURCE: nflscrapR",
       title = "HOF QBs produce 188% more value per attempt",
       subtitle = "Average expected points added per dropback by HOF tier, 2009-2018")

ggsave("EPA_by_HOFclass.jpeg")


#Density plot for EPA by HOF vs non-HOF
ggplot(data_qb_flag, aes(x = EPA)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_density(aes(fill = as_factor(hof), color = as_factor(hof)), alpha = .1) +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(labels = c("Non-HOF", "HOF")) + 
  scale_color_discrete(labels = c("Non-HOF", "HOF")) +
  theme(legend.position = "bottom")+
  #guides(color = "none") + 
  labs(x = "EPA per attempt",
       y = "Density\n",
       fill = element_blank(),
       color = element_blank(),
       title = "53% of all passes are worth 0 or worse in EPA, \nbut only 49% of HOFers are non-positive",
       subtitle = "Distribution of EPA by HOF status",
       caption = "SOURCE: nflscrapR") + 
  coord_cartesian(x = c(-5,5))

ggsave("epa_distribution.jpeg")


#Gather our summary by HOF data to a tidy format for easy plotting
wowo.graph <- wowo.table %>% 
  select(-(EPA.per.completion:WPA.per.completion)) %>%
  spread(wowo, EPA.per.attempt) %>% 
  mutate(rel.diff = With / Without - 1) %>%
  gather(key = metric, value = value, Without:With) 


#Plot slope charts for each HOF QB with or without
wowo.graph %>%
  ggplot(aes(fct_rev(metric), value)) +
  geom_point(aes(color = hof), size = 2) +
  geom_line(aes(group = hof, color = hof)) + 
  geom_text_repel(data = subset(wowo.graph, metric == "Without"),
                  force = 2,
                  segment.color = "grey",
                  aes(label = hof,
                      color = hof),
                  nudge_x = -.2,
                  direction = "both",
                  hjust = 0,
                  family = "Permanent Marker") + 
  geom_text_repel(data = subset(wowo.graph, metric == "With"),
                  force = 2,
                  segment.color = "grey",
                  aes(label = hof,
                      color = hof),
                  nudge_x = .2,
                  direction = "both",
                  hjust = 1,
                  family = "Permanent Marker") + 
  expand_limits(y = c(0, .5)) + 
  theme(legend.position = "none") + 
  labs(x = element_blank(),
       y = "EPA per attempt\n",
       caption = "SOURCE: nflscrapR",
       title = "Receivers enjoyed playing with HOFers...unless they got Eli",
       subtitle = "EPA per attempt to all receivers that played with a HOF and without him")

ggsave("average_epa_wowo_hof.jpeg")


#Calculate mean EPA for each HOF for annotation of next plot
data_qb_mean <- data_qb_hof %>%
  group_by(Passer) %>%
  summarize(EPA.mean = mean(EPA, na.rm = TRUE))

#Examine in HOF QBs scatter plot of expected points
ggplot(data_qb_hof, aes(fct_reorder(Passer, EPA, .fun = mean), EPA)) + 
  geom_hline(yintercept = 0) + 
  geom_quasirandom(aes(color = Passer), 
                   alpha = 1/20, method = "smiley") + 
  geom_text(data = data_qb_mean, 
            aes(label = round(EPA.mean, 3), 
                x = Passer, y = EPA.mean),
            family = "Permanent Marker") + 
  coord_flip(ylim = c(-5,5)) + 
  labs(y = element_blank(),
       x = element_blank(),
       caption = "SOURCE: nflscrapR",
       title = "Rodgers is the EPA king, but even the best produce a ton of negative plays",
       subtitle = "EPA distribution of all passes, 2009-2018, with average by Passer") +
  theme(legend.position = "none")

ggsave("hof_epa_distr.jpeg")


#Calculate mean EPA for each HOF WoWo for annotation of next plot
data_mod_hof <- data_mod %>%
  filter(wowo == "With") %>%
  group_by(hof) %>%
  summarize(EPA.mean = mean(EPA, na.rm = TRUE))

#Examine in HOF QBs scatter plot of expected points for the WoWo subset
data_mod %>%
  filter(wowo == "With") %>%
  select(EPA, hof) %>%
ggplot(aes(fct_reorder(hof, EPA, .fun = mean), EPA)) + 
  geom_hline(yintercept = 0) + 
  geom_quasirandom(aes(color = hof), 
                   alpha = 1/5, method = "frowney") + 
  geom_text(data = data_mod_hof, 
            aes(label = round(EPA.mean, 3), 
                x = hof, y = EPA.mean),
            family = "Permanent Marker") + 
  coord_flip(ylim = c(-5,5)) + 
  labs(y = element_blank(),
       x = element_blank(),
       caption = "SOURCE: nflscrapR",
       title = "Our sample of 'With' attempts is slightly better than the overall sample",
       subtitle = "EPA distribution of all passes to 'With' Receivers, 2009-2018, with average by Passer") +
  theme(legend.position = "none")

ggsave("hof_epa_WITH_distr.jpeg")

#Mean EPA plot for WoWo split
data_mod %>% 
  group_by(wowo) %>%
  summarize(EPA = round(mean(EPA, na.rm = TRUE),2)) %>%
  ggplot(aes(as_factor(wowo), EPA)) +
  geom_col(fill = "#00BFC4") +
  geom_text(aes(label = EPA), 
            nudge_y = .01, family = "Permanent Marker") + 
  scale_x_discrete(labels = c("Without", "With")) + 
  expand_limits(y = c(0, .3)) + 
  labs(x = element_blank(),
       y = "EPA per attempt\n",
       caption = "SOURCE: nflscrapR",
       title = "Our WOWO receivers tended to be a little better than the overall data",
       subtitle = "Average expected points added per dropback by WOWO receivers, 2009-2018")

ggsave("average_wowo_receivers.jpeg")


#let's do some basic modeling to get a better understanding of impact of HOF QB-------------
#remove  outliers
data_outliers <- boxplot(data_mod$EPA, plot=FALSE)$out
data_mod_prune <- data_mod %>%
  filter(EPA %in% data_outliers)

#bivariate regression for wowo (same as taking the averages, but with intervals)
mod_hof <- lm(EPA ~ wowo,
              data = data_mod)

#model fit
summary(mod_hof)
#model fit metrics
glance(mod_hof)

#Create a tidy dataset of model coeficients and confidence intervals.  Clean some data
hof_impact <- bind_cols(tidy(mod_hof), 
                        confint_tidy(mod_hof)) %>%
  mutate(estimate = ifelse(is.na(estimate + lag(estimate)), estimate, estimate + lag(estimate)),
         conf.low = ifelse(is.na(conf.low + lag(conf.low)), conf.low, conf.low + lag(conf.low)),
         conf.high = ifelse(is.na(conf.high + lag(conf.high)), conf.high, conf.high + lag(conf.high)),
         term = case_when(
           term == "(Intercept)" ~ "Without",
           term == "wowoWith" ~ "With"
         ))

#create an easy plot for with / without using regression coefficients
hof_impact %>%
  ggplot(aes(fct_rev(term), estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = term), size = 1.5) + 
  geom_curve(aes(x = hof_impact$term[1], 
                 y = hof_impact$estimate[1], 
                 xend = hof_impact$term[2], 
                 yend = hof_impact$estimate[2]),
             curvature = -.2,
             color = "grey", size = 1) + 
  expand_limits(y = c(.1, .4)) + 
  theme(legend.position = "none") + 
  labs(x = element_blank(),
       y = "Expected EPA per attempt\n",
       caption = "SOURCE: nflscrapR",
       title = "The model suggests that the same receivers produced 92% more EPA with a HOF QB",
       subtitle = "Bivariate regression results, with 95% confidence intervals")

ggsave("model_epa_wowo.jpeg")

#find the epxected average lift of HOF with versus without
hof_relative_impact <- hof_impact %>% 
  select(estimate) %>% 
  mutate(estimate = estimate/lag(estimate)-1) %>% na.omit()


####create a win probability model to quantify EPA increases------------------------------
#summarize the data to get total passing EPA per game by each team
game_data <- data_qb_flag %>%
  group_by(GameID, HomeTeam, AwayTeam, Season) %>%
  summarize(home_EPA = sum(ifelse(HomeTeam == posteam, EPA, 0), na.rm = TRUE),
            away_EPA = sum(ifelse(AwayTeam == posteam, EPA, 0), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(GameID = as.character(GameID))

#load the game result data (commented out because I saved the combined file already)
#sg_09 <- scrape_game_ids(2009, type = "reg")
#sg_10 <- scrape_game_ids(2010, type = "reg")
#sg_11 <- scrape_game_ids(2011, type = "reg")
#sg_12 <- scrape_game_ids(2012, type = "reg")
#sg_13 <- scrape_game_ids(2013, type = "reg")
#sg_14 <- scrape_game_ids(2014, type = "reg")
#sg_15 <- scrape_game_ids(2015, type = "reg")
#sg_16 <- scrape_game_ids(2016, type = "reg")
#sg_17 <- scrape_game_ids(2017, type = "reg")
#sg_18 <- scrape_game_ids(2018, type = "reg")

#game_outcomes <- as_tibble(bind_rows(sg_09, sg_10, sg_11, sg_12, sg_13, sg_14, sg_15,sg_16, sg_17,  sg_18))
  
##load dataset so I don't need to wait on APIs
game_outcomes <- read_csv("~/Desktop/R Projects/NFL/Data/game_results.csv", 
                         col_types = cols(game_id = col_character()))

#Join our datasets                      
game_data_join <- left_join(game_data, game_outcomes, by = c("GameID" = "game_id")) 

#create the win indicator
#first for the home teams
game_model_home <- game_data_join %>%
  filter(home_score != away_score) %>%
  mutate(win = case_when(
    home_score > away_score ~ 1,
    away_score > home_score ~ 0)
  ) %>%
  select(GameID, week, Season, home_team, away_team, home_EPA, away_EPA, win) %>%
  rename("team1" = home_team, "team2" = away_team, "epa1" = home_EPA, "epa2" = away_EPA)

#then away teams so we have the fullest range of total passing EPA and outcomes
game_model_away <- game_data_join %>%
  filter(home_score != away_score) %>%
  mutate(win = case_when(
    away_score > home_score ~ 1,
    home_score > away_score ~ 0)
  ) %>%
  select(GameID, week, Season, away_team, home_team, away_EPA, home_EPA, win) %>%
  rename("team1" = away_team, "team2" = home_team, "epa1" = away_EPA, "epa2" = home_EPA)

#join the home and away datasets
game_model <- bind_rows(game_model_home, game_model_away)

#create a win probability model based just on total passing EPA per game
set.seed(123)
#training and testing sets
data_split <- rsample::initial_split(game_model, prop = .6, strata = "win")
data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)



#logistic regression
epa_win_mod <- glm(win ~ epa1,
                   family = "binomial",
                   data = data_train)
#summary of logit model
summary(epa_win_mod)
#model statistics
glance(epa_win_mod)

#check fit on test dataset
mod_test <- as_tibble(prediction(epa_win_mod, data_test))
metrics(mod_test, truth = win, estimate = fitted)

#simulate a dataset for better plotting
data_sim <- tibble(epa1 = seq(from = -40,to = 40, by = .1))

#add predictions to simulated set
epa_model_prediction <- as_tibble(prediction(epa_win_mod, data_sim, calculate_se = TRUE)) %>%
  mutate(low = fitted - 1.96 * se.fitted,
         high = fitted + 1.96 * se.fitted)

#find the average total passing EPA in games
overall_mean <- mean(game_model$epa1)

#Plot the logit curve with a few annotations
ggplot(epa_model_prediction, aes(epa1, fitted)) + 
  geom_hline(yintercept = .5, linetype = 1) + 
  geom_vline(xintercept = overall_mean, linetype = 2, color = "purple", size = 1) + 
  geom_vline(xintercept = overall_mean * (1 + hof_relative_impact$estimate[1]), linetype = 2, color = "red", size = 1) + 
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 1/5) + 
  geom_text(aes(x = 4, y = .75), label = "Average QB",
            family = "Permanent Marker",
            color = "purple", size = 6, hjust = 1) + 
  geom_text(aes(x = 10, y = .25), label = "Hall of Faame QB",
            family = "Permanent Marker",
            color = "red", size = 6, hjust = 0) + 
  geom_line(group = 1, size = 1, color= "blue")+
  scale_y_continuous(labels = percent_format())+
  labs(x = "Total Passing EPA",
       y = "Win Prodbability",
       title = "All else held equal, the average HOF level QB increases \nthe odds of winning by about 22%",
       subtitle = "Win probability estimated by total passing EPA,\nrandom sample of all games played 2009-2018")

ggsave("win_prob_hof.jpeg")

#simulate what a HOF would theoretically add at each total EPA marker, all else held equal
data_sim_hof <- epa_model_prediction %>%
  rename("original" = "epa1", "pred1" = "fitted", "pred.error" = "se.fitted") %>%
  mutate(epa1 = original + 0.151633 * 32)

#add predictions
hof_win_impact <- as_tibble(prediction(epa_win_mod, data_sim_hof, calculate_se = TRUE)) %>%
  mutate(diff = fitted / pred1 - 1)
  
#plot the relative impact of replacing your QB with a HOFer, all else held equal
hof_win_impact %>%
  ggplot(aes(original, diff)) + 
  geom_vline(xintercept = overall_mean, linetype = 2, size = 1) + 
  geom_hline(yintercept = 0.2371171, linetype = 2, size = 1) + 
  geom_text(aes(x = overall_mean + 2, y = .40), label = "The impact of replacing an\naverage QB with a Hall of Famer",
            family = "Permanent Marker",
            color = "red", size = 5, hjust = 0) + 
  geom_curve(xend = overall_mean+.01,
             yend = 0.24,
             x = overall_mean + 9,
             y = .38,
             color = "red",
             curvature = -.3,
             arrow = arrow(ends = "last", type = "closed", length = unit(0.25, "cm"))) + 
  geom_area(fill = "red", alpha = 1/2, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  labs(x = "\nTotal Passing EPA (base rate)",
       y = "Expected win probability increase \nat each base Passing EPA per game\n",
       caption = "SOURCE: nflscrapR",
       title = "Unsurprisingly, replacing a terrible QB with a HOF QB is good idea",
       subtitle = "Relative increase in chances of winning with the same receivers from expected increase in total Passing EPA")

ggsave("relative_win_impact.jpeg")  


#convert the relative impact plot into an incremental wins plot
hof_win_impact %>%
  mutate(initial.win = pred1 * 16,
         new.win = fitted * 16,
         delta = new.win - initial.win) %>%
  ggplot(aes(initial.win, new.win)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 5) + 
  geom_area(fill = "#629cff", alpha = 1/2, color = "black") +
  scale_x_continuous(breaks = seq(0,16, 4)) + 
  scale_y_continuous(breaks = seq(0,16, 4)) + 
  labs(x = "\nWins ff you have a non-HOF QB...",
       y = "Wins with the same team,\nbut now with a HOFer...\n",
       subtitle = "Expected wins for getting a Hall of Fame QB relative to current strength, \nall else held equal")

ggsave("wins_increase.jpeg")

##Create a table to find out Antonio Brown's current versus future ranking
data_qb %>%
  group_by(Receiver_ID, Receiver) %>% 
  summarize(EPA.mean = mean(EPA, na.rn = TRUE),
            n = n(),
            EPA.total = sum(EPA, na.rm = TRUE)) %>%
  filter(n > 200 & Receiver_ID != "None") %>%
  ungroup() %>%
  mutate(rank = min_rank(desc(EPA.mean)),
         new_epa = ifelse(Receiver == "A.Brown" | Receiver ==  "L.Bell", EPA.mean - 0.1516, EPA.mean),
         new.rank = min_rank(desc(new_epa))) %>%
  arrange(rank) %>%
  filter(Receiver == "A.Brown" | Receiver ==  "L.Bell") 


