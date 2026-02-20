


# this .r script contains the function to pull the predicted expected goals for even strength, man advantage, shorthanded, and empty net



# Full function for xG values --------------------------------------------------------------------------------------------------------
fun.pbp_full_add <- function(data, model_EV, model_UE, model_SH, model_EN) { 
  
  ### Function should be run using unprocessed scraped pbp data. Returns a list of 3 data frames: 
  ## [[1]] - full pbp data with extras and xG probabilities for both EV and UE situations
  ## [[2]] - EV prep data frame for xG model evaluation
  ## [[3]] - UE prep data frame for xG model evaluation
  ## [[4]] - SH prep data frame for xG model evaluation
  ## [[5]] - EN prep data frame for xG model evaluation
  
  # Initial prep of scraped pbp data.
  pbp_part <- fun.pbp_expand(data)
  pbp_part <- fun.pbp_index(pbp_part)
  
  
  # Convert for use with XGBoost and predict goal probability - EV
  print("predict_EV", quote = F)
  pbp_prep_EV <- fun.pbp_prep(pbp_part, "EV")
  model_prep_EV <- fun.model_prep(pbp_prep_EV, "EV")
  
  model_prep_EV <- Matrix::Matrix(model_prep_EV, sparse = TRUE)
  pred_matrix_EV <- model_prep_EV[, 2:ncol(model_prep_EV)]
  xgb_matrix_EV <- xgb.DMatrix(data = pred_matrix_EV)
  
  pbp_prep_EV$pred_XGB_7 <- predict(object = model_EV, xgb_matrix_EV)
  pred_goal_EV <- select(pbp_prep_EV, game_id, event_id, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - UE
  print("predict_UE", quote = F)
  pbp_prep_UE <- fun.pbp_prep(pbp_part, "UE")
  model_prep_UE <- fun.model_prep(pbp_prep_UE, "UE")
  
  model_prep_UE <- Matrix::Matrix(model_prep_UE, sparse = TRUE)
  pred_matrix_UE <- model_prep_UE[, 2:ncol(model_prep_UE)]
  xgb_matrix_UE <- xgb.DMatrix(data = pred_matrix_UE)
  
  pbp_prep_UE$pred_XGB_7 <- predict(object = model_UE, xgb_matrix_UE)
  pred_goal_UE <- select(pbp_prep_UE, game_id, event_id, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - SH
  print("predict_SH", quote = F)
  pbp_prep_SH <- fun.pbp_prep(pbp_part, "SH")
  model_prep_SH <- fun.model_prep(pbp_prep_SH, "SH")
  
  model_prep_SH <- Matrix::Matrix(model_prep_SH, sparse = TRUE)
  pred_matrix_SH <- model_prep_SH[, 2:ncol(model_prep_SH)]
  xgb_matrix_SH <- xgb.DMatrix(data = pred_matrix_SH)
  
  pbp_prep_SH$pred_XGB_7 <- predict(object = model_SH, xgb_matrix_SH)
  pred_goal_SH <- select(pbp_prep_SH, game_id, event_id, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - EN
  print("predict_EN", quote = F)
  pbp_prep_EN <- fun.pbp_prep(pbp_part, "EN")
  model_prep_EN <- fun.model_prep(pbp_prep_EN, "EN")
  
  model_prep_EN <- Matrix::Matrix(model_prep_EN, sparse = TRUE)
  pred_matrix_EN <- model_prep_EN[, 2:ncol(model_prep_EN)]
  xgb_matrix_EN <- xgb.DMatrix(data = pred_matrix_EN)
  
  pbp_prep_EN$pred_XGB_7 <- predict(object = model_EN, xgb_matrix_EN)
  pred_goal_EN <- select(pbp_prep_EN, game_id, event_id, pred_XGB_7)
  
  
  
  # Join / Make List
  print("final_join", quote = F)
  
  hold <- rbind(pred_goal_EV, 
                pred_goal_UE, 
                pred_goal_SH, 
                pred_goal_EN
  )
  
  pbp_return <- pbp_part %>% 
    left_join(., hold, by = c("game_id", "event_id")) #%>% 
  #select(-c(face_index:pen_index))
  
  pbp_return$shift_ID <- as.character(pbp_return$shift_ID)
  pbp_prep_EV$shift_ID <- as.character(pbp_prep_EV$shift_ID)
  pbp_prep_UE$shift_ID <- as.character(pbp_prep_UE$shift_ID)
  pbp_prep_SH$shift_ID <- as.character(pbp_prep_SH$shift_ID)
  pbp_prep_EN$shift_ID <- as.character(pbp_prep_EN$shift_ID)
  
  list_data <- list(pbp_full = pbp_return, 
                    prep_EV = pbp_prep_EV, 
                    prep_UE = pbp_prep_UE, 
                    prep_SH = pbp_prep_SH, 
                    prep_EN = pbp_prep_EN
  )
  
}

# Run
pbp_full_list <- fun.pbp_full_add(data = rtss, 
                                  model_EV = xG_model_XGB_7_EV, 
                                  model_UE = xG_model_XGB_7_UE, 
                                  model_SH = xG_model_XGB_10_SH, 
                                  model_EN = xG_model_XGB_10_EN
)


# Return data from function
pbp_df <-   pbp_full_list$pbp_full
model_EV <- pbp_full_list$prep_EV
model_UE <- pbp_full_list$prep_UE
model_SH <- pbp_full_list$prep_SH
model_EN <- pbp_full_list$prep_EN





# end .R script
