


# this .r script prepares the data for an xG model
# required packages
require(hockeyR); require(tidyverse); require(data.table)



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



# call pipe locally
`%>%` <- magrittr::`%>%`

# recode season labels
rtss <- rtss %>% dplyr::mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 7, 8)))

# drop lockout shortened and pandemic shortened seasons
rtss <- rtss %>% dplyr::filter(season != "2012-13" & season != "2019-20")

# set most recent season in the dataset as the reference group
rtss$season <- relevel(factor(rtss$season), ref = "2022-23")

# leg of the nhl season i.e., regular season or playoffs
table(rtss$season_type, useNA = "always")

# retain shots during regular season and playoff games
rtss <- rtss %>% dplyr::filter(season_type == "R" | season_type == "P")

# drop shootouts during regular season, but retain extra playoffs
rtss <- rtss %>% dplyr::filter(season_type == "R" & period < 5 | season_type == "P" & period < Inf)

# top code period ... 5th period for extra over-time in the playoffs
rtss$period[rtss$period >= 5] <- 5

# recode into factor
rtss$period[rtss$period == 1] <- "1st"
rtss$period[rtss$period == 2] <- "2nd"
rtss$period[rtss$period == 3] <- "3rd"
rtss$period[rtss$period == 4] <- "OT"
rtss$period[rtss$period == 5] <- "OT+"

# set regular season as the reference group
rtss$period <- relevel(factor(rtss$period), ref = "1st")

# set regular season as the reference group
rtss$season_type <- relevel(factor(rtss$season_type), ref = "R")



# rename case description of what happened
rtss <- rtss %>% dplyr::rename(event_description = description)



# code whether prior shot was a rebound
data.table::setDT(rtss)
data.table::setorder(rtss, game_id, period, period_seconds)

rtss[, `:=`(
  prior_event_type    = data.table::shift(event_type),
  prior_event_seconds = data.table::shift(period_seconds),
  prior_event_saved   = data.table::shift(grepl("saved", tolower(event_description))),
  seconds_since_last  = period_seconds - data.table::shift(period_seconds)
  ), 
  by = .(game_id, period)]

rtss[, is_rebound := as.integer(
  event_type == "SHOT" &
    prior_event_type == "SHOT" &
    prior_event_saved == TRUE &
    seconds_since_last <= 5
    )]

rtss[, time_since_rebound := data.table::fcase(
  is_rebound == 1 & seconds_since_last <= 1, "1 second",
  is_rebound == 1 & seconds_since_last <= 2, "2 seconds",
  is_rebound == 1 & seconds_since_last <= 3, "3 seconds",
  is_rebound == 1 & seconds_since_last <= 4, "4 seconds",
  is_rebound == 1 & seconds_since_last <= 5, "5 seconds",
  default = "no rebound"
  )]
data.table::setDF(rtss)


# set no rebound as the reference group
rtss$time_since_rebound <- relevel(factor(rtss$time_since_rebound), ref = "no rebound")





# define shot types used in model ----------------------------------------------
fenwick <- c("SHOT", "MISSED_SHOT", "GOAL")

# all types of shot cases
rtss <- rtss %>% dplyr::filter(event_type %in% fenwick)



# function to create dummy for goal
is_goal <- function(df){
  df$is_goal <- ifelse(df$event_type == "GOAL", 1 , 0)
  return(df)
}
rtss <- is_goal(rtss)



# rename column that indicates shot type
rtss <- rtss %>% dplyr::rename(shot_type = secondary_type)

# recode shot types
rtss$shot_type[rtss$shot_type == "Between Legs"] <- "Trick Shot"; rtss$shot_type[rtss$shot_type == "Cradle"] <- "Trick Shot"

# set wrist shot as the reference group
rtss$shot_type <- relevel(factor(rtss$shot_type), ref = "Wrist Shot")



# rename shot coordinates
rtss <- rtss %>% dplyr::rename(coords_x = x_fixed); rtss <- rtss %>% dplyr::rename(coords_y = y_fixed)

# don't run
# rename variable in hockeyR's rtss scrape
# rtss <- rtss %>% dplyr::rename(shot_distance_hr = shot_distance)

# calculate shot distancce
rtss <- rtss %>%
  dplyr::mutate(
    shot_distance = ifelse(abs(coords_x) > 89,
                           sqrt((abs(coords_x) - 89)^2 + coords_y^2),
                           sqrt((89 - abs(coords_x))^2 + coords_y^2)
                           )
    )

# don't run
# rename variable in hockeyR's rtss scrape
# rtss <- rtss %>% dplyr::rename(shot_angle_hr = shot_angle)

# calculate the shot angle
rtss <- rtss %>%
  dplyr::mutate(
    shot_angle = abs(atan2(coords_y, 89 - abs(coords_x)) * (180 / pi))
    )



# don't run
# shot at even strength, on power play, or shorthanded? 
# table(rtss$strength, useNA = "always")


# recode lone case 
rtss$strength[rtss$strength == "Short Handed"] <- "Shorthanded"

# print cases
table(rtss$event_goalie_name[rtss$event_type == "SHOT"], useNA = "always")

# empty net shots 
rtss$empty_net <- 0
rtss$empty_net[is.na(rtss$event_goalie_name) & rtss$event_type == "SHOT"] <- 1
rtss$empty_net[is.na(rtss$event_goalie_name) & rtss$event_type == "GOAL"] <- 1



# control for shots with man advantage
rtss$manadv <- rtss$home_skaters - rtss$away_skaters

# top code at +/-3 differential because 6v3 is the only realistic scenario 
rtss$manadv[rtss$manadv <= -3] <- -3
rtss$manadv[rtss$manadv >=  3] <-  3

# code as factor, with even strength as the reference
rtss$manadv <- relevel(factor(rtss$manadv), ref = "0")



# columns that list the home team and away team
rtss <- rtss %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    home_team = event_team[event_team_type == "home"][1],
    away_team = event_team[event_team_type == "away"][1]
  ) %>%
  dplyr::ungroup()

# function to create dummy for home team
is_home <- function(df){
  df$is_home <- ifelse(df$event_team_type == "home", 1 , 0)
  return(df)
}
rtss <- is_home(rtss)

# shooting team
rtss$shooting_team <- ifelse(rtss$event_team == rtss$home_team, rtss$home_team, rtss$away_team)
rtss$shooting_team <- relevel(factor(rtss$shooting_team), ref = "Florida Panthers")

# defending tea <- m
rtss$defending_team <- ifelse(rtss$event_team == rtss$home_team, rtss$away_team, rtss$home_team)
rtss$defending_team <- relevel(factor(rtss$defending_team), ref = "Buffalo Sabres")



# function to calculate the week of the season
week_of_season <- function(df){
    df <- df %>%
      dplyr::group_by(season) %>%
      dplyr::mutate(
        season_start   = min(as.Date(game_date)),
        week_of_season = as.integer(difftime(as.Date(game_date), season_start, units = "weeks")) + 1
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-season_start)
    return(df)
  }

rtss <- week_of_season(rtss)


# score differential for shooting team 
score_diff <- function(df){
  df$score_diff <- ifelse(
    df$shooting_team == df$home_team,
    df$home_score - df$away_score,
    df$away_score - df$home_score
    )
  return(df)
}
rtss <- score_diff(rtss)               
                            


# divsion matchup / rivalry
rtss$division_matchup <- 0
rtss$division_matchup[rtss$home_division_name == rtss$away_division_name] <- 1



# prior event before the shot
table(rtss$prior_event_type)

# don't run
# manually check observations 
# obs_to_check <- rtss %>% dplyr::filter(prior_event_type == "GAME_END" | prior_event_type == "GAME_SCHEDULED")

# recode last play to face off given that all these categories indicate play stoppage and faceoff must happen for play to start again
rtss$prior_event_type[rtss$prior_event_type == "GOAL"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "STOP"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "PENALTY"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "CHALLENGE"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "EARLY_INT_END"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "EARLY_INT_START"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "GAME_END"] <- "FACEOFF"
rtss$prior_event_type[rtss$prior_event_type == "GAME_SCHEDULED"] <- "FACEOFF"

# set shot on net as the reference group
rtss$prior_event_type <- relevel(factor(rtss$prior_event_type), ref = "SHOT")



# high danger scoring chance -- 20 feet from goal line is hash marks
rtss$high_danger <- as.integer(rtss$shot_distance <= 20 & rtss$shot_angle <= 45)




# don't run
# goalie effexts 
# table(is.na(rtss$event_goalie_name), useNA = "always")
# rtss$event_goalie_name[is.na(rtss$event_goalie_name)] <- "empty net"
# rtss$event_goalie_name <- relevel(factor(rtss$event_goalie_name), ref = "empty net")



# don't run
# player effects
# table(tss$event_player_1_name, useNA = "always")
# rtss$event_player_1_name <- relevel(factor(rtss$event_player_1_name), ref = "Alex.Ovechkin")





# close .R script 
