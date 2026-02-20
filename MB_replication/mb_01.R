


# this .r script replications Matthew Barlowe's xG model

# documentation: https://rstudio-pubs-static.s3.amazonaws.com/311470_f6e88d4842da46e9941cc6547405a051.html



# don't run
# install.packages("remotes")
# remotes::install_github("danmorse314/hockeyR")



# required packages
require(hockeyR); require(tidyr); require(ggplot2)



# load play-by-play from hockeyR repo
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
# also works
# load play-by-play from hockeyR repo
# pbp <- hockeyR::load_pbp( # https://github.com/danmorse314/hockeyR-data/tree/main/data
# season = 2010:2023
  # )



# recode season labels
rtss <- rtss %>% dplyr::mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 7, 8)))



# leg of the nhl season i.e., regular season or playoffs
table(rtss$season_type, useNA = "always")

# retain shots during regular season and playoff games
rtss <- rtss %>% dplyr::filter(season_type == "R" | season_type == "P")

# drop shootouts during regular season
rtss <- rtss %>% dplyr::filter(season_type == "R" & period < 5 | season_type == "P" & period < Inf)



# shot type
table(rtss$secondary_type, useNA = "always")

# rename column that indicates shot type
rtss <- rtss %>% dplyr::rename(shot_type = secondary_type)



# drop penalty shots
rtss <- rtss %>% dplyr::filter(shot_type != "Penalty Shot")


# strength in number of skaters
table(rtss$strength, useNA = "always")

# recode typo
rtss$strength[rtss$strength == "Short Handed"] <- "Shorthanded"


# rename
rtss <- rtss %>% dplyr::rename(event_description = description)



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



# define shot types used in model
fenwick <- c("SHOT", "MISSED_SHOT", "GOAL")

# all types of shot cases
rtss <- rtss %>% dplyr::filter(event_type %in% fenwick)




# rename shot coordinates
rtss <- rtss %>% dplyr::rename(coords_x = x_fixed); rtss <- rtss %>% dplyr::rename(coords_y = y_fixed)



# function that indicates if shot was made by the home team
is_home <- function(df){
  df$is_home <- ifelse(df$event_team_type == "home", 1 , 0)
  return(df)
  }
rtss <- is_home(rtss)

# function that indicates if shot resulted in a goal
is_goal <- function(df){
  df$is_goal <- ifelse(df$event_type == "GOAL", 1, 0)
  return(df)
  }
rtss <- is_goal(rtss)



# identify shots scored on rebound

# calculate the time lag between shots to identify rebounds
rtss <- rtss %>% 
  # group by game
  dplyr::group_by(game_id) %>%
  # arrange shots in the order that they happened
  dplyr::arrange(event_id, .by_group = TRUE) %>%
  # time difference between shots 
  dplyr::mutate(time_diff = game_seconds - lag(game_seconds))

# recode missings
rtss$time_diff[is.na(rtss$time_diff)] <- 0

# recode missings
rtss$is_home[is.na(rtss$is_home)] <- 0

# code shots off of rebounds
rtss$is_rebound <- ifelse(rtss$time_diff < 3 & rtss$event_type %in% fenwick & rtss$event_team == lag(rtss$event_team), 1, 0)
rtss$is_rebound[is.na(rtss$is_rebound)] <- 0 # recode missings

# code shots off the rush
rtss$is_rush <- ifelse(rtss$time_diff < 4 & lag(abs(rtss$coords_x)) < 25 & rtss$event_type %in% fenwick, 1, 0)
rtss$is_rush[is.na(rtss$is_rush)] <- 0 # recode missings

# recode
rtss$is_rebound[is.na(rtss$time_diff)] <- 0



# recode type of shot
table(rtss$shot_type, useNA = "always")

# group into 'trick shot' category
rtss$shot_type[rtss$shot_type == "Cradle"] <- "Trick Shot"
rtss$shot_type[rtss$shot_type == "Between Legs"] <- "Trick Shot"



# drop missing shot coordinates 
rtss <- dplyr::filter(rtss, coords_x != 'NA' & coords_y != 'NA') 


# calculate radian
radian <- asin(12/sqrt(1440.20+144))

rtss$coords_y <- ifelse(rtss$coords_x < 0, -1 * rtss$coords_y, rtss$coords_y)

rtss$coords_x <- abs(rtss$coords_x)

rtss$shot_angle <- (asin(abs(rtss$coords_y)/sqrt((87.95 - abs(rtss$coords_x))^2 + rtss$coords_y^2))*180)/ 3.14

rtss$shot_angle <- ifelse(abs(rtss$coords_x) > 88, 90 + (180-(90 + rtss$shot_angle)), rtss$shot_angle)

rtss$distance <- sqrt((87.95 - abs(rtss$coords_x))^2 + rtss$coords_y^2)





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


# predict
xG_mb <- stats::predict(xGmodel_mb, rtss, type = "response")

# receiver operating characteristic (ROC) curve for the test data
roc_mb <- pROC::roc(rtss$is_goal, xG_mb)

# calculate area under curve
auc_mb <- pROC::auc(roc_mb); cat("Test AUC:", round(auc_mb, 2), "\n")



# roc curve coordinates
roc_df_mb <- rbind(
  data.frame(
    fpr   = 1 - roc_mb$specificities,
    tpr   = roc_mb$sensitivities,
    model = paste0("Barlowe replication (AUC = ", round(auc_mb, 2), ")")
    )
  )

# plot
ggplot(roc_df_mb, aes(x = fpr, y = tpr, colour = "firebrick1")) +
  geom_line(linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  labs(
    title   = "Replication of Matthew Barlowe's xG Model ROC Curve",
    x       = "False positive rate",
    y       = "True positive rate",
    colour  = NULL
    ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = c(0.7, 0.2))



