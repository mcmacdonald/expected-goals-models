


# this .r script pulls HockeyR's xG predictions and plots the ROC curves for each of the xG models



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



# define shot types used in model
fenwick <- c("SHOT", "MISSED_SHOT", "GOAL")

# all types of shot cases
rtss <- rtss %>% dplyr::filter(event_type %in% fenwick)


# normalize shot coordinates for plotting
shot_plot_df <- shots_eh %>%
  dplyr::mutate(
    coords_x = abs(coords_x),
    coords_y = ifelse(coords_x < 0, -coords_y, coords_y)
    )

# heatmap of all Fenwick unblocked shot attempts in the NHL
# https://www.stat.cmu.edu/cmsac/conference/2022/assets/pdf/Morse.pdf
sportyR::geom_hockey("nhl") +
  ggplot2::geom_hex(
    data = shot_plot_df,
    # plot shots at fixed coordinates for visualization
    ggplot2::aes(coords_x, coords_y), 
    alpha = .7, 
    binwidth = c(5,5), 
    show.legend = FALSE
    ) +
  ggplot2::scale_fill_gradient2(low = "white", mid = "#dff5f7", high = "darkred") +
  ggplot2::facet_wrap(~season) +
  ggplot2::labs(
    title  = "Heatmap of all unblocked shot attempts in the NHL, by season",
    caption = "Notes: NHL RTSS play-by-play data collected by hockeyR. 2012-13 lockout-shortened and 2019-20 pandemic-shortened seasons omitted.",
    colour = NULL
    ) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(size = 20),
    plot.caption = ggplot2::element_text(size = 10, hjust = 0)
    )



# pull HockeyR's xG predictions, and plot the comparative ROC curve

# test data for HockeyR's xG model
hr_test <- rtss %>% dplyr::filter(season == "2015-16")

# function that indicates if shot resulted in a goal
is_goal <- function(df){
  df$is_goal <- ifelse(df$event_type == "GOAL", 1, 0)
  return(df)
  }
hr_test <- is_goal(hr_test)

# ROC for hockeyR's xGoal predictions on the same shots
roc_hr <- pROC::roc(hr_test$is_goal, hr_test$xg)

# calculate area under curve
auc_hr <- pROC::auc(roc_hr); cat("Test AUC:", round(auc_hr, 2), "\n") 


# roc curve coordinates
roc_df_hr <-   data.frame(
  fpr   = 1 - roc_hr$specificities,
  tpr   = roc_hr$sensitivities,
  model = paste0("HockeyR (AUC = ", round(auc_hr, 2), ")")
  )

# append xG predictions together
roc_df <- rbind(
  roc_df_mp, 
  roc_df_mb, 
  roc_df_EH,
  roc_df_hr
  )



# plot receiver operating characteristic (ROC) curve
ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr, colour = model)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey", linetype = "dashed") +
  ggplot2::scale_colour_manual(
    values = c(
      "purple1", 
      "green3",
      "yellow1",
      "steelblue1",
      "firebrick1"
      )
    ) +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    x      = "False positive rate",
    y      = "True positive rate",
    title  = "xG models ROC curves",
    colour = NULL
    ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(size = 20),
    legend.text     = ggplot2::element_text(size = 10),
    axis.text       = ggplot2::element_text(size = 12),
    axis.title      = ggplot2::element_text(size = 12),
    legend.position = c(0.7, 0.2)
    )





# end .R script


