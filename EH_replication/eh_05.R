


# this .r script computes an ROC curve and area under the curve (AUC) for EH's even strength xG model



# set seed for replication
set.seed(123)

# goals at even strength
y_EV <- as.numeric(pbp_prep_EV$event_type == "GOAL")

# predicted goals for the test data
p_xG_model_XGB_7_EV <- stats::predict(xG_model_XGB_7_EV, predictors_EV)

# receiver operating characteristic (ROC) curve for the test data
roc_xG_model_XGB_7_EV <- pROC::roc(y_EV, p_xG_model_XGB_7_EV)

# calculate area under curve
auc_xG_model_XGB_7_EV <- pROC::auc(roc_xG_model_XGB_7_EV); cat("Test AUC:", round(auc_xG_model_XGB_7_EV, 2), "\n")



# roc curve coordinates
roc_df_EH <- rbind(
  data.frame(
    fpr   = 1 - roc_xG_model_XGB_7_EV$specificities,
    tpr   = roc_xG_model_XGB_7_EV$sensitivities,
    model = paste0("Evolving Hockey's even strength xG replication (AUC = ", round(auc_xG_model_XGB_7_EV, 2), ")")
    )
  )

# plot receiver operating characteristic (ROC) curve
ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr, colour = "firebrick1")) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey", linetype = "dashed") +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    x      = "False Positive Rate",
    y      = "True Positive Rate",
    title  = "Evolving Hockey's xG Model ROC Curves",
    colour = NULL
    ) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.7, 0.2))





# end .R script
