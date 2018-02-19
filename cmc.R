###########################################################
### COERCED MULTICLASS CLASSIFICATION #####################
### ADDS NEW CLASSIFICATION CATEGORY FOR INCONCLUSIVE CASES
###########################################################

require(caret)
require(pROC)

cmc = function(df_probs
               , df_preds
               , df_target
               , target_level_1
               , target_level_2
               , threshold
               , return_outcome){

  testing_data_frame = data.frame('preds' = df_preds
                            , 'probs' = df_probs
                            , 'target' = df_target)

  testing_data_frame$cmc = ifelse(testing_data_frame$probs > (0.5-threshold) & testing_data_frame$probs < (0.5+threshold), 'inconclusive', testing_data_frame$pred)

  testing_data_frame$cmc[testing_data_frame$cmc == '1'] = target_level_1
  testing_data_frame$cmc[testing_data_frame$cmc == '2'] = target_level_2

  #levels(testing_data_frame$cmc) = c(target_level_1, target_level_2, 'inconclusive')

  testing_data_new = testing_data_frame[testing_data_frame$cmc != 'inconclusive', ]
  testing_data_new = droplevels(testing_data_new)

  if(nrow(testing_data_new) == 0 | nlevels(testing_data_new$target) < 2){
    accuracy = NA
    lower_ci = NA
    upper_ci = NA
    roc_auc = NA
    roc_lower = NA
    roc_upper = NA
  } else {
    confmat_1 = caret::confusionMatrix(testing_data_new$cmc, testing_data_new$target)
    confmat_2 = caret::confusionMatrix(testing_data_frame$pred, testing_data_frame$target)

    accuracy = unname(confmat_1$overall[1])
    lower_ci = unname(confmat_1$overall[3])
    upper_ci = unname(confmat_1$overall[4])
    roc = roc(response = testing_data_new$target
              , predictor = testing_data_new$probs
              , ci=T)
    roc_auc = roc$auc[1]
    roc_lower = roc$ci[1]
    roc_upper = roc$ci[3]



  }
  sample_loss = 1 - (nrow(testing_data_new)/nrow(testing_data_frame))


  if(return_outcome == 'acc'){
    return(accuracy)
  } else if (return_outcome == 'lower'){
    return(lower_ci)
  } else if (return_outcome == 'upper'){
    return(upper_ci)
  } else if(return_outcome == 'loss'){
    return(sample_loss)
  } else if(return_outcome == 'auc'){
    return(roc_auc)
  } else if(return_outcome == 'auc_lower'){
    return(roc_lower)
  } else if(return_outcome == 'auc_upper'){
    return(roc_upper)
  } else if(return_outcome == 'df'){
    return(testing_data_new)
  }
}


#usage example
# cmc(df_probs = testing_data$probs[,1] # --> must contain class probs from prediction
#     , df_preds = testing_data$pred # --> must contain the predictions from a classifier
#     , df_target = testing_data$veracity # --> outcome variable
#     , threshold = .2 # --> threshold
#     , return_outcome = 'acc')

#TODO
# - set multiclass parameter

