


# replication of Evolving Hockey's (EH's) expected goals model

# for reference:
# see documentation: https://rpubs.com/evolvingwild/395136/
# original code: https://github.com/evolvingwild/hockey-all/tree/master/xG



# things to note for the replication:

# 1) I use NHL RTSS play-by-play data, which is the same data used by EH, but I pull it from hockeyR's repository

# 2) I keep EH's naming convention for functions, objects, etc., where possible 

# 3) some columm names in the datasets differ given their different sourcing, so I had to identify and change those

# 4) I've made some ammendments to EH's functions

# 5) EH's code wasn't fully or easily replicable from scratch, so I've made simple changes to fix that issue




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
# this function also works to load play-by-play from hockeyR repository
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





# end .R script
