


# this .R script plots the predicted probabilities of an expected goal for a 'typical shot' for different NHL seasons

# required packages
require(brms); requrie(ggplot2); require(ddplyr)



# pull the coefficents from the model
betas <- as.data.frame(brms::fixef(xG)) %>% dplyr::mutate(term = rownames(.))

# rows for season fixed effects
seasons <- betas[77:86, ]

# recode labels
seasons$term <- c(
  "2010-11",
  "2011-12",
  "2013-14",
  "2014-15",
  "2015-16",
  "2016-17",
  "2017-18",
  "2018-19",
  "2020-21",
  "2021-22"
  )

# plot the log-odds and credible intervals
ggplot2::ggplot(seasons, ggplot2::aes(x = Estimate, y = forcats::fct_rev(term))) +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = `Q2.5`, xmax = `Q97.5`)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  ggplot2::labs(title = "Season \u00d7 Season Bias in an expected goal (xG) for an NHL season", x = "Log odds that shot results in goal (95% CI)", y = "NHL season") +
  ggplot2::theme_bw()





# plot the predcted probability of a goal for a typical shot -------------------------------------------------------------------------------

# define the conditions of a typical shot
typical_shot <- data.frame(
  shot_distance = mean(rtss$shot_distance, na.rm = TRUE), 
  shot_angle = mean(rtss$shot_angle, na.rm = TRUE)
  )

# conditional effects for NHL seasons
conditional_ef <- brms::conditional_effects(
  xG, # model
  effects = "season", # effect name
  conditions = typical_shot,
  re_formula =  NA, # this command ensures conditional effects are estimated for the "typical" case, ignoring random effect
  )

# data for plotting
plot_df <- as.data.frame(conditional_ef$season)

# rename colimns
plot_df$estimate <- plot_df$estimate__
plot_df$lower    <- plot_df$lower__
plot_df$upper    <- plot_df$upper__

# recode labels
plot_df$season <- c(
  "2010-11",
  "2011-12",
  "2013-14",
  "2014-15",
  "2015-16",
  "2016-17",
  "2017-18",
  "2018-19",
  "2020-21",
  "2021-22",
  "2022-23"
  )

# plot the expected goals season x season
fig_02 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = season, y = estimate, group = 1)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "firebrick1", alpha = 0.2) +
  ggplot2::geom_line(color = "firebrick", linwidth = 1) +
  ggplot2::geom_point(color = "firebrick3", size = 2, shape = 21, fill = "firebrick3") +
  ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 0.25)) +
  ggplot2::labs(
    title = "Season \u00d7 Season Bias in an Expected Goal (xG) for a 'Typical' Shot",
    y = "Predicted Probability of a Goal (xG)",
    x = "NHL Season",
    caption = stringr::str_wrap(paste0(
    "Figure notes: The predicted probabilities illustrate season-by-season differences for an expected goal (xG) conditioned on the average distance (", 
    round(typical_shot$shot_distance, 1), "ft) and angle (", round(typical_shot$shot_angle, 1), "°) of a shot, estimated from a Bayesian hierarchical model. The xG model controls for the type of shot taken (reference = wrist shot); 
    the last thing that happened before the shot was taken (reference = shot); if the shot is off a rebound, the number of seconds that have elapsed 
    since before the shot (reference = no rebound); if the shooting team has a man advantage/disadvantage (reference = even strength); 
    if the shot was taken on an empty net; interaction effects for man advantage/disadvantage and an empty net shot; the period of play 
    (reference = 1st period); the number of seconds remaining in the period; the score differential; if the game being played is a division match-up; 
    the week of the NHL season; and the NHL season (reference = 2022-23). The xG model also models the shooting team and the defending team as random 
    effects to control for unobserved differences in talent and skill."), 
    width = 190
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 12, face = "bold"),
    axis.title = ggplot2::element_text(size = 12),
    axis.text.y =  ggplot2::element_text(angle =  0, hjust = 1, colour = "black", size = 10),
    axis.text.x =  ggplot2::element_text(angle = 45, hjust = 1, colour = "black", size = 10),
    plot.caption = ggplot2::element_text(hjust =  0, face = "italic", color = "black", size = 8),
    plot.caption.position = "plot"
    )

ggplot2::ggsave(
  plot = fig_03, 
  filename = "xG_fig03.png",
  path = "~/Desktop/",
  width = 10, 
  height = 5, 
  dpi = 720
  )





# close .R script
