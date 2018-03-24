# SCORE TEST DATSET


# Manually set best parameters

    catboost_fit_params_home <- list(iterations = 1000,
                                     thread_count = 96,
                                     loss_function = 'Logloss',
                                     border_count = 32,
                                     depth = 12,
                                     l2_leaf_reg = 10,
                                     use_best_model = FALSE,
                                     learning_rate = 0.03)
    catboost_fit_params_work <- list(iterations = 1000,
                                     thread_count = 96,
                                     loss_function = 'Logloss',
                                     border_count = 32,
                                     depth = 12,
                                     use_best_model = FALSE,
                                     learning_rate = 0.03)

# Build models on full train data
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  # catboost.save_model(catboost_model_home, "models/catboost_model_home")
  # catboost.save_model(catboost_model_work, "models/catboost_model_work")

# Score Test data

  result_test$score_home <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  

# Order by score

  result_test_ordered_home <- result_test[order(customer_id,-score_home),]
  result_test_ordered_work <- result_test[order(customer_id,-score_work),]
  


# Make predictions
  
  averaging = F
  
  if (averaging == F)
  {
  
    pred_test_home <- result_test_ordered_home[,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
    pred_test_work <- result_test_ordered_work[,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
    
  }
  
  if (averaging == T)
  {
  
    # ToDO (Optional): make unique points
    
    pred_test_home <- get_averaged_prediction(result_test_ordered_home, 0.03)
    pred_test_work <- get_averaged_prediction(result_test_ordered_work, 0.03)
  
  }
  
  colnames(pred_test_home) <- c("customer_id","home_lat","home_lon")
  colnames(pred_test_work) <- c("customer_id","work_lat","work_lon")








# Create output dataset

  pred <- merge(pred_test_work, pred_test_home, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
  
  # Check number of customers
  length(unique(pred$customer_id))
  
  write.csv(pred, "output/pred.csv", sep=",", row.names = F, col.names = T, quote=F)



  
  
  
  # Averaging the same algorithm
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  result_test$score_home1 <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work1 <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  
  write.csv(result_test, "output/result_test_1.csv", sep=";", row.names=F, col.names=T)
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  result_test$score_home2 <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work2 <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  
  write.csv(result_test, "output/result_test_2.csv", sep=";", row.names=F, col.names=T)
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  result_test$score_home3 <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work3 <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  
  write.csv(result_test, "output/result_test_3.csv", sep=";", row.names=F, col.names=T)
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  result_test$score_home4 <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work4 <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  
  write.csv(result_test, "output/result_test_4.csv", sep=";", row.names=F, col.names=T)
  
  catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
  catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
  
  result_test$score_home5 <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
  result_test$score_work5 <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
  
  write.csv(result_test, "output/result_test_5.csv", sep=";", row.names=F, col.names=T)
  
  result_test$score_home <- (result_test$score_home1 + result_test$score_home2 + result_test$score_home3 + result_test$score_home4 + result_test$score_home5) / 5
  result_test$score_work <- (result_test$score_work1 + result_test$score_work2 + result_test$score_work3 + result_test$score_work4 + result_test$score_work5) / 5
  
  write.csv(result_test[,.(customer_id,pos_atm_orig_lat,pos_atm_orig_lon,score_home,score_work)], "output/result_test.csv",sep=";",row.names=F,col.names=T)
  
  result_test_ordered_home <- result_test[order(customer_id,-score_home),]
  result_test_ordered_work <- result_test[order(customer_id,-score_work),]
  
  pred_test_home <- get_averaged_prediction(result_test_ordered_home, 0.03)
  pred_test_work <- get_averaged_prediction(result_test_ordered_work, 0.03)
  
  pred_test_home <- result_test_ordered_home[,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
  pred_test_work <- result_test_ordered_work[,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
  
  pred <- merge(pred_test_work, pred_test_home, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
  
  # Check number of customers
  length(unique(pred$customer_id))
  
  write.csv(pred, "output/pred_no_avg.csv", sep=",", row.names = F, col.names = T, quote=F)
  
  




# Other algorithms for Stacking

result_train_home$catboost_score_home <- catboost.predict(catboost_model_home, catboost_train_home, prediction_type = "Probability")
result_train_work$catboost_score_work <- catboost.predict(catboost_model_work, catboost_train_work, prediction_type = "Probability")

xgb_model_home <- xgboost(
  data=data.matrix(result_train_home[,c(predictors),with=F]), label=as.numeric(result_train_home$target_home)-1,
  max_depth=10,
  subsample=1.0,
  nrounds=1000, 
  objective="binary:logistic", 
  eval_metric="auc")

result_train_home$xgb_score_home <- predict(xgb.model_home, result_train_home, ntree=100)

# Fit RandomForest for Home on Train
xgb_model_work <- xgboost(
  data=data.matrix(result_train_home[,c(predictors),with=F]), label=as.numeric(result_train_home$target_home)-1,
  max_depth=10,
  subsample=1.0,
  nrounds=1000, 
  objective="binary:logistic", 
  eval_metric="auc")

result_train_work$xgb_score_work <- predict(xgb_model_work, result_train_work, ntree=100)

write.csv(result_train_home, "output/result_train.csv", sep=";", row.names=F)
write.csv(result_train_work, "output/result_work.csv", sep=";", row.names=F)

system("gzip output/result_train_home.csv")
system("gzip output/result_train_work.csv")








