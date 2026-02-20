


# this .r script replicates Moneypuck's expected goals (xG) model

# Moneypuck's documentation used to replicate their xG model: https://moneypuck.com/about.htm



# required packages
require(tidyverse); require(magrittr); require(utils); require(stats); require(xgboost); require(pROC)

# call pipe to workspace
`%>%` <- magrittr::`%>%`



# first, download and unzip data
url <- "https://peter-tanner.com/moneypuck/downloads/shots_2007-2024.zip"
utils::download.file(url, destfile = "shots_2007-2024.zip")
utils::unzip("shots_2007-2024.zip")

# pull from zip file
mp_shots <- readr::read_csv("shots_2007-2024.csv")

# dictionary for Mobeypuck's dataset, for reference
mp_dictionary <- readr::read_csv("https://peter-tanner.com/moneypuck/downloads/MoneyPuck_Shot_Data_Dictionary.csv")

# clean up the dictionary
mp_dictionary <- mp_dictionary[1:124, 1:2]



# clean Moneypuck's shot data -------------------------------------------------------------------------------

# first, keep only variables needed to re-construct Moneypuck's xG model
model_vars <- c(
  
  # game and player information
  "season", 
  "shotID", 
  "isPlayoffGame", 
  "period", 
  "team",
  "homeSkatersOnIce", 
  "awaySkatersOnIce",
  
  # outcome to predict
  "goal", 
  
  # Moneypuck's own predictions
  "xGoal",
  
  # model variables
  "arenaAdjustedShotDistance",   # 1. shot distance
  "timeSinceLastEvent",          # 2. time since last event
  "shotType",                    # 3. shot type
  "speedFromLastEvent",          # 4. speed from previous event
  "shotAngleAdjusted",           # 5. shot angle
  "lastEventyCord_adjusted",     # 6. E-W location of last event
  "shotAnglePlusReboundSpeed",   # 7. rebound angle / time
  "lastEventCategory",           # 8. type of last event i.e., shot, goal, hit, etc.
  "yCordAdjusted",               # 10. E-W location of shot
  "xCordAdjusted",               # 14. N-S location of shot
  "distanceFromLastEvent",       # 13. distance of shot from last event
  "shotOnEmptyNet",              # 15. empty net
  
  # penalty information
  "homePenalty1Length", 
  "homePenalty1TimeLeft",
  "awayPenalty1Length", 
  "awayPenalty1TimeLeft"
  )

# drop all other variables from Moneypuck's dataset
mp_shots <- mp_shots %>% dplyr::select(dplyr::all_of(model_vars))



# manually recode nominal-level variables


# frequency distribution for shot type
mp_shots %>% dplyr::count(shotType, sort = TRUE)
  
  # cross-tabulation for type of shot and shot on an empty net
  table(mp_shots$shotType, mp_shots$shotOnEmptyNet, useNA = "always")
  
  # replace missing values with wristers given that it is by far the most common shot type 
  mp_shots$shotType[is.na(mp_shots$shotType)]<- "WRIST"


# frequency distribution for last event prior to shot
mp_shots %>% dplyr::count(lastEventCategory, sort = TRUE)
  
  # recode missings
  mp_shots$lastEventCategory[is.na(mp_shots$lastEventCategory)] <- "FAC"
  
  # recode noisy stoppage categories to face-off
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "STOP"] <- "FAC"
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "GOAL"] <- "FAC"
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "PSRT"] <- "FAC"
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "PEND"] <- "FAC"
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "EISTR"] <- "FAC"
  mp_shots$lastEventCategory[mp_shots$lastEventCategory == "GEND"] <- "FAC"

  
  
# construct variables for man advantage/disadvantage situations for the shooting team
mp_shots <- mp_shots %>%
  dplyr::mutate(
    
    # number of skaters on ice for shooting team versus defending team
    shooterSkaters = if_else(team == "HOME", homeSkatersOnIce, awaySkatersOnIce),
    defenderSkaters = if_else(team == "HOME", awaySkatersOnIce, homeSkatersOnIce),
    manAdvantage = shooterSkaters - defenderSkaters,  # >0 = PP, 0 = EV, <0 = SH
    
    # time elapsed since power play (PP) started i.e., seconds into PP
    ppTimeElapsed = if_else(
      team == "HOME",
      # total length of penalty - time left in penalty = time elapsed in PP
      if_else(awayPenalty1Length > 0, awayPenalty1Length - awayPenalty1TimeLeft, 0),
      if_else(homePenalty1Length > 0, homePenalty1Length - homePenalty1TimeLeft, 0)
      )
    )





# set-up data -----------------------------------------------------------------------------------------------

# Moneypuck trained their xG model on shot data for the 2007/08 - 2014/15 regular season and playoffs
train <- mp_shots %>%
  dplyr::filter(
    season %in% 2007:2014,
    period != 5 # no shootouts
    )

# hold out 2015/16 season as test data, which is the test data that Moneypuck uses for cross-validation
test <- mp_shots %>%
  dplyr::filter(
    season == 2015,
    period != 5 # no shootouts
    )

# column names for the 15 variables in Moneypuck's xG model
variables <- c(
  "arenaAdjustedShotDistance",   # 1. Shot distance
  "timeSinceLastEvent",          # 2. Time since last event
  "shotType",                    # 3. Shot type (categorical)
  "speedFromLastEvent",          # 4. Speed from previous event
  "shotAngleAdjusted",           # 5. Shot angle (absolute value)
  "lastEventyCord_adjusted",     # 6. E-W location of last event
  "shotAnglePlusReboundSpeed",   # 7. Rebound angle change / time
  "lastEventCategory",           # 8. Type of last event (categorical)
  "defenderSkaters",             # 9. Defending team skaters on ice
  "yCordAdjusted",               # 10. E-W location of shot
  "manAdvantage",                # 11. Man advantage situation
  "ppTimeElapsed",               # 12. Time since PP started
  "distanceFromLastEvent",       # 13. Distance from previous event
  "xCordAdjusted",               # 14. N-S location of shot
  "shotOnEmptyNet"               # 15. Empty net indicator
  )

# encode categorical variables as integers... xgboost() requires all variables to be numerically coded
encode_vars <- function(df) {
  df %>%
    dplyr::mutate(
      shotType = as.integer(factor(shotType)),
      lastEventCategory = as.integer(factor(lastEventCategory))
      ) %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0)))
}

# encode categorical variables in training data
X_train <- encode_vars(train)

# ecode categorical variables in test data
X_test <- encode_vars(test)



# training data 
dtrain <- xgboost::xgb.DMatrix(
  data = as.matrix(X_train), 
  label = train$goal
  )

# test data 
dtest  <- xgboost::xgb.DMatrix(
  data = as.matrix(X_test),
  label = test$goal
  )

# model parameters
params <- list(
  objective        = "binary:logistic",
  eval_metric      = "logloss",
  eta              = 0.05,       # learning rate
  max_depth        = 5,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 10          # guards against overfitting on rare shots
  )





# estimate the model and predictive accuracy of the model ---------------------------------------------------

# set seed for replication
set.seed(123)

# gradient boosting method for the training data
xG_mp <- xgboost::xgb.train( # for reference: https://www.kaggle.com/code/rtatman/machine-learning-with-xgboost-in-r/notebook
  params    = params, # parameters
  data      = dtrain, # training data
  nrounds   = 500, # 500 rounds
  watchlist = list(
    train = dtrain, 
    test = dtest
    ),
  early_stopping_rounds = 25,
  verbose = 1
  )

# predicted goals for the test data
xG_mp_rep <- stats::predict(xG_mp, dtest)



# receiver operating characteristic (ROC) curve for the test data
roc_mp_rep <- pROC::roc(test$goal, xG_mp_rep)

# calculate area under curve
auc_mp_rep <- pROC::auc(roc_mp_rep); cat("Test AUC:", round(auc_mp_rep, 2), "\n")



# ROC for Moneypuck's own xGoal predictions on the same shots
roc_mp <- pROC::roc(test$goal, test$xGoal)

# calculate area under curve
auc_mp <- pROC::auc(roc_mp); cat("Test AUC:", round(auc_mp, 2), "\n")



# roc curve coordinates
roc_df_mp <- rbind(
  data.frame(
    fpr   = 1 - roc_mp_rep$specificities,
    tpr   = roc_mp_rep$sensitivities,
    model = paste0("Moneypuck replication (AUC = ", round(auc_mp_rep, 2), ")")
    ),
  data.frame(
    fpr   = 1 - roc_mp$specificities,
    tpr   = roc_mp$sensitivities,
    model = paste0("MoneyPuck (AUC = ", round(pROC::auc(roc_mp), 2), ")")
    )
  )





# end .R script


