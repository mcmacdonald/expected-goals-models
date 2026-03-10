# expected goals (xG) models
R code for replications of expected goals (xG) models in the public domain


This repository includues R code to replicate three different xG models:

1) Moneypuck's xG model - gradient boosting (XGBoost)
2) Evolving Hockey's xG models for even strength, man advantage, short-handed, empty net shots - gradient boosting (XGBoost)
3) Matthew Barlowe's xG model - logistic regression

[I plot ROC curves that compare the predictions of the different models](https://github.com/mcmacdonald/expected-goals-models/blob/main/xG_02.png). I also include Dan Morse's (hockeyR) xG model predictions that use gradient boosting (XGBoost) for comparison. The rationale to doing so is to compare the performance of different xG models, which, together, provide clear benchmarks for further refinement of an xG model.

To test for bias in the NHL RTSS data and its use for predictions, I first estimate a Bayesian model that predict goals across [11 NHL seasons: 2010-11, 2011-12, 2013-14, 2014-15, 2015-16, 2016-17, 2017-18, 2018-19, 2020-21, 2021-22, and 2022-23](https://github.com/mcmacdonald/expected-goals-models/blob/main/xG_01.png). This model incorporate the best predictors from current models (shot distance from goal, shot angle, etc.), but also incorporates random effects for the shooting and defending teams that control for differences in team talent and skill, which have been omitted from prior models.

The findings of this sensitivity analysis suggests that xG models trained on one or more seasons of shot data likely produce biased predictions for expected goals for another season (i.e., the test data). Although the predicted probability of a goal for a 'typical shot' is fairly stable across seasons, [there is considerable variation in the credible intervals from season to season](https://github.com/mcmacdonald/expected-goals-models/blob/main/xG_03.png). This finding suggests that the predictions of expected goals from one season to be too unreliable to accurately predict expected goals in another, without calibrating or weighting the shot data to adjust for this uncertainty.

To replicate this analysis, run the .R files in this order: '00_data.R', '01_data.R', '02_models.R', '03_plots.R'.
