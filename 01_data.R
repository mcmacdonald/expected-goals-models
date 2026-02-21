


# this .r script prepares the data for an xG model

# required packages
require(hockeyR); require(tidyverse); require(ggplot2); require(sportyR); require(pROC)



# first, load in nhl rtss play-by-play events ... hockeyR scrapes NHL's RTSS play-by-play data, which is what EH and Moneypuck use

# don't run
# install.packages("remotes")
# remotes::install_github("danmorse314/hockeyR") # hockeyR documentation, for reference: https://github.com/danmorse314/hockeyR

# load play-by-play from hockeyR repository
rtss <- hockeyR::load_pbp( # https://github.com/danmorse314/hockeyR-data/tree/main/data
  season = c(
    '2010-11',
    '2011-12',
    '2012-13',
    '2013-14',
    '2014-15', 
    '2015-16',
    '2016-17',
    '2017-18',
    '2018-19',
    '2019-20',
    '2020-21',
    '2021-22',
    '2022-23'
    # don't run
    # this season does not load
    # '2023-24'
    )
  )

# don't run
# this function also works ti load play-by-play from hockeyR repository
# pbp <- hockeyR::load_pbp( # https://github.com/danmorse314/hockeyR-data/tree/main/data
# season = 2010:2023
# )



# leg of the nhl season i.e., regular season or playoffs
table(rtss$season_type, useNA = "always")

# retain shots during regular season and playoff games
rtss <- rtss %>% dplyr::filter(season_type == "R" | season_type == "P")

# drop shootouts during regular season, but retain extra playoffs
rtss <- rtss %>% dplyr::filter(season_type == "R" & period < 5 | season_type == "P" & period < Inf)

# recode season labels
rtss <- rtss %>% dplyr::mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 7, 8)))

# rename column that indicates shot type
rtss <- rtss %>% dplyr::rename(shot_type = secondary_type)



# recode
rtss$strength[rtss$strength == "Short Handed"] <- "Shorthanded"

# rename
rtss <- rtss %>% dplyr::rename(event_description = description)

# rename shot coordinates
rtss <- rtss %>% dplyr::rename(coords_x = x_fixed); rtss <- rtss %>% dplyr::rename(coords_y = y_fixed)


# columns that list the home team and away team
rtss <- rtss %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    home_team = event_team[event_team_type == "home"][1],
    away_team = event_team[event_team_type == "away"][1]
    ) %>%
  dplyr::ungroup()

# rename variable in hockeyR's rtss scrape
rtss <- rtss %>% dplyr::rename(shot_distance_hr = shot_distance)

# rename variable in hockeyR's rtss scrape
rtss <- rtss %>% dplyr::rename(shot_angle_hr = shot_angle)





# ##### all syntax above this line is generic code common to the cleaning for all the replication models





# define shot types used in model
fenwick <- c("SHOT", "MISSED_SHOT", "GOAL")

# all types of shot cases
rtss <- rtss %>% dplyr::filter(event_type %in% fenwick)





# function to create dummy for goal
is_goal <- function(df){
  df$is_goal <- ifelse(df$event_type == "GOAL", 1 , 0)
  return(df)
}
rtss <- is_goal(rtss)



# function to create dummy for home team
is_home <- function(df){
  df$is_home <- ifelse(df$event_team_type == "home", 1 , 0)
  return(df)
}
rtss <- is_home(rtss)







table(is.na(rtss$event_goalie_name), useNA = "always")


rtss$event_goalie_name[is.na(rtss$event_goalie_name)] <- "empty net"

rtss$event_goalie_name <- relevel(factor(rtss$event_goalie_name), ref = "empty net")

rtss$event_player_1_name <- relevel(factor(rtss$event_player_1_name), ref = "Alex.Ovechkin")


rtss$shooting_team <- ifelse(rtss$event_team == rtss$home_team, rtss$home_team, rtss$away_team)
rtss$defending_team <- ifelse(rtss$event_team == rtss$home_team, rtss$away_team, rtss$home_team)



rtss$shooting_team <- relevel(factor(rtss$shooting_team), ref = "Florida Panthers")
rtss$defending_team <- relevel(factor(rtss$defending_team), ref = "Buffalo Sabres")



# set regular season as the reference group
rtss$season_type <- relevel(factor(rtss$season_type), ref = "R")


# set wrist shot as the reference group
rtss$shot_type <- relevel(factor(rtss$shot_type), ref = "Wrist Shot")

# set most recent season in the dataset as the reference group
rtss$season <- relevel(factor(rtss$season), ref = "2022-23")



# calculate shot distancce
rtss <- rtss %>%
  dplyr::mutate(
    shot_distance = ifelse(abs(coords_x) > 89,
      sqrt((abs(coords_x) - 89)^2 + coords_y^2),
        sqrt((89 - abs(coords_x))^2 + coords_y^2)
        )
    )

# calculate the shot angle
rtss <- rtss %>%
  dplyr::mutate(
    shot_angle = abs(atan2(coords_y, 89 - abs(coords_x)) * (180 / pi))
    )


# control for shots with man advantage
rtss$manadv <- rtss$home_skaters - rtss$away_skaters
# top code at +/-3 differential because 6v3 is the only realistic scenario 
rtss$manadv[rtss$manadv <= -3] <- -3
rtss$manadv[rtss$manadv >=  3] <-  3


# top code period ... top code the 5th period for extra over-time in the playoffs
rtss$period[rtss$period >= 5] <- 5


# control for last thing that happened before the shot



# function to create dummy for a shot off a rebound
is_rebound <- function(df){
  df$is_rebound <- ifelse(df$event_type == "GOAL", 1 , 0)
  return(df)
}
rtss <- is_rebound(rtss)







# end .R script








# calculate the baseline probability of a goal for any shot taken at random
null <- stats::glm(
  goal ~ 1 + factor(season), # seasonality
  family = stats::binomial(), 
  data = mp_shots
)

# function to calculate the predicted probabilty
inv_logit <- function(b){
  exp(b)/(1+exp(b))
}
inv_logit(null$coefficients[[1]])







# basic Bayesian hierarchical xG model
xG <- brms::brm(
  formula = is_goal ~ 
    # fixed effects
    poly(shot_distance, degree = 2, raw = TRUE) + 
    poly(shot_angle, degree = 2, raw = TRUE) + 
    as.factor(shot_type) +
    as.factor(is_rebound) + 
    manadv +
    as.factor(period) +
    as.factor(season_type) +
    as.factor(season),
  data   = rtss,
  family = brms::bernoulli(link = "logit"),
  chains = 4,
  cores  = 4,
  iter   = 250,
  warmup = 100,
  prior  = c(
    brms::prior(normal(0, 1), class = b),
    brms::prior(normal(0, 1), class = Intercept),
    brms::prior(exponential(1), class = sd)
  )
)
summary(xG)






# random effects
(1 | event_goalie_name) +
  (1 | event_player_1_name) +
  (1 | shooting_team) +
  (1 | defending_team)


# estimate xG model ---------------------------------------------------------------------------
xGmodel_mb <- stats::glm(
  is_goal ~ poly(distance, 3, raw = TRUE) + 
    poly(shot_angle, 3, raw = TRUE) + 
    as.factor(shot_type) + 
    as.factor(strength) +
    is_rebound + is_rush,
  data = rtss, 
  family = binomial(link = 'logit')
)
summary(xGmodel_mb)





# get posterior predicted probabilities
pred_probs <- brms::posterior_epred(xg_bayes, newdata = rtss)

# average across draws to get mean predicted probability per observation
mean_probs <- colMeans(pred_probs)

# compute ROC and AUC
roc_bayes <- pROC::roc(rtss$is_goal, mean_probs)

# plot
plot(roc_bayes)

pROC::auc(roc_bayes)


#################


# attach predicted probability of goal to the dataset
pbp$xG <- stats::predict(xGmodel, pbp, type = "response")



xG_pbp <- stats::predict(xGmodel, pbp, type = "response")
y_pbp <- pbp$is_goal

# receiver operating characteristic (ROC) curve for the test data
roc_pbp <- pROC::roc(y_pbp, xG_pbp)

# calculate area under curve
auc_pbp <- pROC::auc(roc_pbp); cat("Test AUC:", round(roc_pbp, 2), "\n")








#########





# ── Feature engineering ──────────────────────────────────────────────────────
shots_eh <- shots %>%
  filter(period != 5) %>%                          # no shootouts
  filter(homeEmptyNet == 0, awayEmptyNet == 0) %>% # no empty nets
  filter(homeSkatersOnIce == awaySkatersOnIce) %>%  # even strength only
  mutate(
    # Score state from shooter's perspective, capped at ±3
    score_diff_raw = ifelse(isHomeTeam == 1,
                            homeTeamGoals - awayTeamGoals,
                            awayTeamGoals - homeTeamGoals),
    score_state = case_when(
      score_diff_raw <= -3 ~ -3L,
      score_diff_raw >= 3  ~  3L,
      TRUE                 ~ as.integer(score_diff_raw)
    ),
    
    # Shot type: collapse rare categories
    shotType_clean = case_when(
      shotType %in% c("WRIST", "SNAP", "SLAP", "BACK", "TIP", "WRAP", "DEFL") ~ shotType,
      TRUE ~ "OTHER"
    ),
    shotType_int = as.integer(factor(shotType_clean)),
    
    # Last event category: collapse rare
    lastEvent_clean = case_when(
      lastEventCategory %in% c("FAC", "HIT", "SHOT", "BLOCK", "MISS", "GIVE", "TAKE") ~ lastEventCategory,
      TRUE ~ "OTHER"
    ),
    lastEvent_int = as.integer(factor(lastEvent_clean))
  )

# ── Define features (EH feature set) ────────────────────────────────────────
eh_features <- c(
  "arenaAdjustedShotDistance",   # shot distance
  "shotAngleAdjusted",           # shot angle
  "time",                        # game seconds
  "period",                      # period
  "xCordAdjusted",               # shot x coord
  "yCordAdjusted",               # shot y coord
  "lastEventxCord_adjusted",     # last event x
  "lastEventyCord_adjusted",     # last event y
  "distanceFromLastEvent",       # distance from last event
  "timeSinceLastEvent",          # seconds since last event
  "shotRebound",                 # rebound flag
  "shotRush",                    # rush flag
  "score_state",                 # score differential (binned)
  "isHomeTeam",                  # home/away
  "shotType_int",                # shot type
  "lastEvent_int"                # last event type
)

# ── Train/test split ─────────────────────────────────────────────────────────
train <- shots_eh %>% filter(season %in% 2007:2014)
test  <- shots_eh %>% filter(season == 2015)

# ── Build DMatrix objects ────────────────────────────────────────────────────
dtrain <- xgb.DMatrix(
  data  = as.matrix(train[, eh_features]),
  label = train$goal
)
dtest <- xgb.DMatrix(
  data  = as.matrix(test[, eh_features]),
  label = test$goal
)

# ── Hyperparameters (EH used modified random search / 5-fold CV) ─────────────
params <- list(
  objective        = "binary:logistic",
  eval_metric      = "logloss",
  eta              = 0.05,
  max_depth        = 5,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 10
)

# ── Train with early stopping ────────────────────────────────────────────────
set.seed(42)
eh_model <- xgb.train(
  params               = params,
  data                 = dtrain,
  nrounds              = 500,
  watchlist            = list(train = dtrain, test = dtest),
  early_stopping_rounds = 25,
  verbose              = 1
)

# ── Predict and evaluate ─────────────────────────────────────────────────────
test$xg_eh_es    <- predict(eh_model, dtest)               # your EH replication
# Moneypuck's own xG on the same even-strength test shots
# (already in the data as xGoal)

roc_eh <- roc(test$goal, test$xg_eh_es)
roc_mp <- roc(test$goal, test$xGoal)         # MP xG on ES shots only

cat("EH replication AUC (ES):", round(auc(roc_eh), 3), "\n")
cat("Moneypuck AUC (ES shots):", round(auc(roc_mp), 3), "\n")




