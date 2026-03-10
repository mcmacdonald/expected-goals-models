


# this .r script estimates an xG model by Bayesian hierachical regression

# required packages
require(lme4); require(brms); require(pROC)



# calculate the baseline probability of a goal for any shot taken at random
null <- stats::glm(
  is_goal ~ 1 + factor(season), # seasonality
  family = stats::binomial(), 
  data = rtss
  )

# function to calculate the predicted probabilty
inv_logit <- function(b){
  exp(b)/(1+exp(b))
}
inv_logit(null$coefficients[[1]])





# estimate xG model ------------------------------------------------------------
xG_glm <- lme4::glmer(
  formula = is_goal ~ 
    poly(shot_distance, degree = 2, raw = TRUE) + 
    poly(shot_angle, degree = 2, raw = TRUE) + 
    # high_danger +
    shot_type +
    prior_event_type +
    # is_rebound +
    time_since_rebound +
    manadv*empty_net +
    period +
    period_seconds_remaining +
    score_diff + # for the shooting team
    division_matchup +
    # season_type +
    as.factor(week_of_season) + 
    season +
    (1 | shooting_team) +  # proxy for shooter and offense
    (1 | defending_team) ,  # proxy for goalie and defensive play
  family = stats::binomial(),
  data   = rtss
  )
summary(xG_glm)

# receiver operating characteristic (ROC) curve for the test data
roc_xG_glm <- pROC::roc(xG_glm$y, xG_glm$fitted.values)

# calculate area under curve
pROC::auc(roc_xG_glm)

# don't run
# attach predicted probability of goal to the dataset
# rtss$xG <- stats::predict(xG_glm, rtss, type = "response")





# Bayesian hierarchical xG model
xG <- brms::brm(
  formula = is_goal ~ 
    # fixed effects
    poly(shot_distance, degree = 2, raw = TRUE) + 
    poly(shot_angle, degree = 2, raw = TRUE) + 
    # high_danger + 
    shot_type +
    prior_event_type +
    # is_rebound +
    time_since_rebound +
    manadv*empty_net +
    period +
    period_seconds_remaining +
    score_diff + # for the shooting team
    division_matchup +
    # season_type +
    as.factor(week_of_season) + 
    season +
    
    # random effect
    (1 | shooting_team) +  # proxy for shooter and offense
    (1 | defending_team) ,  # proxy for goalie and defensive play
  
  # specs
  data   = rtss,
  family = brms::bernoulli(link = "logit"),
  chains = 4,
  cores  = 4,
  iter   = 1000,
  warmup = 500,
  
  # priors
  prior  = c(
    brms::prior(normal(0, 1), class = b),
    brms::prior(normal(0, 1), class = Intercept),
    brms::prior(exponential(1), class = sd)
    )
  )
summary(xG)



# get posterior predicted probabilities
pred_probs <- brms::posterior_epred(xG, newdata = rtss)

# average across draws to get mean predicted probability per observation
mean_probs <- colMeans(pred_probs)

# receiver operating characteristic (ROC) curve for the test data
roc_xG <- pROC::roc(rtss$is_goal, mean_probs)

# calculate area under curve
pROC::auc(roc_xG)

# plot
plot(roc_xG)





# close .R script
