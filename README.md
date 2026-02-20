# expected goals (xG) models
R code for replications of expected goals (xG) models in the public domain


This repository includues R code to replicate three different xG models:

1) Moneypuck's xG model - gradient boosting (XGBoost)
2) Evolving Hockey's xG models for even strength, man advantage, short-handed, empty net shots - gradient boosting (XGBoost)
3) Matthew Barlowe's xG model - logistic regression

I evaluate the models on a common held-out test season to permit direct comparison of model performance. I plot ROC curves that compare the predictions of the different models. I also include Dan Morse's (hockeyR) xG model predictions that use gradient boosting (XGBoost) for comparison.

The rationale to doing so is to compare the performance of different xG models, which, together, provide clear benchmarks for further refinement of an xG model.

To this end, I also provide R code for an xG model that builds upon prior models. I construct models that incorporate the best predictors from current models (shot distance from goal, shot angle, etc.), but that also incorporate player effects, goalie effects, and team effects that have been omitted from prior models.

I calculate expected goals during the regular season and playoffs at even strength, on the power play, while shorthanded, and on an empty net.

