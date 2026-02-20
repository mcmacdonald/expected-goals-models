# expected-goals-models
R code for replications of expected goals (xG) models in the public domain


This repository includues R code for replications of different xG models:

1) Moneypuck's xG model - gradient boosting (XGBoost)
2) Evolving Hockey's xG models for even strength, man advantage, short-handed, empty net - gradient boosting (XGBoost)
3) hockeyR's (Dan Morse) xG model - gradient boosting (XGBoost)
4) Matthew Barlowe's xG model - logistic regression

I evaluate the models on a common held-out test season to permit direct comparison of model performance. I plot ROC curves that compare the predictions of the different models.

The rationale to doing so is to compare the performance of different xG models, which provide clear benchmarks for furhter refinement of an xG model.

To this end, I provide R code for an xG model that incorporates player effects, goalie effects, and team effects omitted from prior models.
