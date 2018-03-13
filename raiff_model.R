library(data.table)

# result = fread("result.csv", sep=";")



# library(xgboost)







predictors <- colnames(result)[substring(colnames(result),1,3) == "top"]
predictors <- predictors[5:length(predictors)]
predictors <- c(predictors, c("is_moscow","is_piter","is_other","center.dist","size_x","size_y",
                              "pos_amount",
                              "pos_amount_avg",
                              "pos_amount_cnt",
                              "pos_amount_min",
                              "pos_amount_max",
                              "pos_weekend_trn_cnt",
                              "pos_weekday_trn_cnt",
                              "pos_dow_cnt",
                              "cust_amount_avg",
                              "cust_amount_cnt",
                              "cust_amount_max",
                              "cust_weekend_trn_cnt",
                              "cust_weekend_trn_rate",
                              "cluster_rank_by_size",
                              "cluster_rank_by_max_diss",
                              "cluster_rank_by_av_diss",
                              "cluster_rank_by_diameter",
                              "cluster_rank_by_separation",
                              "cluster_dist",
                              "cluster_diss_to_max_diss",
                              "diameter",
                              "separation",
                              "cluster_cnt",
                              "max_diss",
                              "av_diss",
                              "mcc",
                              "is_atm",
                              "size.x",
                              "size.y",
                              "cluster_mean_lat",
                              "cluster_mean_lon"
                             
))



result$is_atm <- ifelse(result$is_atm == "Y", 1, 0)

result$mcc <-factor(result$mcc)


# Define base datasets

  result_train = result[result$df=="train",]
  result_test = result[result$df == "test",]
  
  # Work dataset is half size of home dataset
  result_train_home = result_train
  result_train_work = result_train[!is.na(result_train$work_lat) & !is.na(result_train$work_lon),]
  
  # Correct target variables
  result_train_home$target_home <- factor(ifelse(result_train_home$target_home == 1 | result_train_home$home_dist <= 0.02, 1, 0))
  result_train_work$target_work <- factor(ifelse(result_train_work$target_work == 1 | result_train_work$work_dist <= 0.02, 1, 0))
  
  # Divide Home & Work datasets into train/validate by customers
  train_home_customers <- unique(result_train_home$customer_id)
  train_work_customers <- unique(result_train_work$customer_id)
  train_train_home_customers = sample(train_home_customers, round(0.75*length(train_home_customers)), FALSE)
  train_train_work_customers = sample(train_work_customers, round(0.75*length(train_work_customers)), FALSE)
  train_validate_home_customers = setdiff(train_home_customers, train_train_home_customers)
  train_validate_work_customers = setdiff(train_work_customers, train_train_work_customers)
  
  result_train_train_home = result_train_home[result_train_home$customer_id %in% train_train_home_customers,]
  result_train_validate_home = result_train_home[result_train_home$customer_id %in%train_validate_home_customers,]
  
  result_train_train_work = result_train_work[result_train_work$customer_id %in% train_train_work_customers,]
  result_train_validate_work = result_train_work[result_train_work$customer_id %in%train_validate_work_customers,]
  
  print("Stats for datasets...")
  print(paste(
    "Train=", nrow(result_train),
    " Test=", nrow(result_test),
    " Train(Home)=", nrow(result_train_home),
    " Train(Work)=", nrow(result_train_work),
    " Train-Train(Home)", nrow(result_train_train_home),
    " Train-Validate(Home)", nrow(result_train_validate_home),
    " Train-Train(Work)=", nrow(result_train_train_work),
    " Train-Validate(Work)=",nrow(result_train_validate_work),sep=""))
  




# Define catboost datasets

  catboost_train_home <- catboost.load_pool(data=result_train_home[,predictors,with=F], 
                                            label = as.numeric(result_train_home$target_home)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=c(62),
                                            thread_count = -1)
  catboost_test_home <- catboost.load_pool(data=result_test[,predictors,with=F], 
                                           label = as.numeric(result_test$target_home)-1,
                                           feature_names = as.list(predictors),
                                           cat_features=c(62),
                                           thread_count = -1)
  
  catboost_train_work <- catboost.load_pool(data=result_train_work[,predictors,with=F], 
                                            label = as.numeric(result_train_work$target_work)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=c(62),
                                            thread_count = -1)
  catboost_test_work <- catboost.load_pool(data=result_test[,predictors,with=F], 
                                           label = NULL,
                                           feature_names = as.list(predictors),
                                           cat_features=c(62),
                                           thread_count = -1)
  
  catboost_train_train_home <- catboost.load_pool(data=result_train_train_home[,predictors,with=F], 
                                            label = as.numeric(result_train_train_home$target_home)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=c(62),
                                            thread_count = -1)
  catboost_train_validate_home <- catboost.load_pool(data=result_train_validate_home[,predictors,with=F], 
                                           label = as.numeric(result_train_validate_home$target_home)-1,
                                           feature_names = as.list(predictors),
                                           cat_features=c(62),
                                           thread_count = -1)
  
  catboost_train_train_work <- catboost.load_pool(data=result_train_train_work[,predictors,with=F], 
                                            label = as.numeric(result_train_train_work$target_home)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=c(62),
                                            thread_count = -1)
  catboost_train_validate_work <- catboost.load_pool(data=result_train_validate_work[,predictors,with=F], 
                                           label = as.numeric(result_train_validate_work$target_home)-1,
                                           feature_names = as.list(predictors),
                                           cat_features=c(62),
                                           thread_count = -1)

  




# Optimize hyperparameters
  
  # Alogirithm1: XGB

  xgb_param_grid = expand.grid(
    maxdepth=c(10,15), 
    nrounds=c(100,1000), 
    subsample=c(1.0)
  )
  xgb_param_grid$home_accuracy <- NA
  xgb_param_grid$work_accuracy <- NA
  
  
  
  for (i in 1:nrow(xgb_param_grid))
  {
  
    params = xgb_param_grid[i,]
    print(params)
    
    xgb_model_home_for_validation <- xgboost(
      data=data.matrix(result_train_train[,c(predictors),with=F]), 
      label=as.numeric(result_train_train$target_home)-1,
      nrounds=params$nrounds,
      max_depth=params$max_depth,
      subsample=params$subsample,
      verbose=0,
      objective="binary:logistic", 
      eval_metric="auc")
    
    xgb_model_work_for_validation <- xgboost(
      data=data.matrix(result_train_train_work[,c(predictors),with=F]), 
      label=as.numeric(result_train_train_work$target_work)-1,
      nrounds=params$nrounds,
      max_depth=params$max_depth,
      subsample=params$subsample,
      verbose=0,
      objective="binary:logistic", 
      eval_metric="auc")
    
        result_train_validate$score_home <- predict(xgb_model_home_for_validation, data.matrix(result_train_validate[,c(predictors),with=F]))
        result_train_validate_work$score_work <- predict(xgb_model_work_for_validation, data.matrix(result_train_validate_work[,c(predictors),with=F]))
  
        pred_home <- result_train_validate[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
        pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
  
        xgb_param_grid$home_accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
        xgb_param_grid$work_accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)
   
  }
  
  optimal_xgb_home_params <- param_grid[which.max(param_grid$home_accuracy),]
  optimal_xgb_work_params <- param_grid[which.max(param_grid$work_accuracy),]
  
  
  
  
  
  
  # Alogirithm2: CatBoost
  
  catboost_param_grid = expand.grid(
    depth=c(6,8,10), 
    iterations=c(100,1000), 
    eval_metric=c("Accuracy","Auc")
  )
  catboost_param_grid$home_accuracy <- NA
  catboost_param_grid$work_accuracy <- NA
  
  print("Starting optimize hyperparameters")
  print(catboost_param_grid)
  
  for (i in 1:nrow(catboost_param_grid))
  {
    
    params = catboost_param_grid[i,]
    print(params)
    
    catboost_fit_params <- list(iterations = params$iterations,
      thread_count = 16,
      eval_metric = params$eval_metric,
      border_count = 32,
      depth = params$depth,
      use_best_model = FALSE,
      learning_rate = 0.03)
    
    catboost_model_home_for_validation <- catboost.train(learn_pool = catboost_train_train_home, params = catboost_fit_params)
    catboost_model_work_for_validation <- catboost.train(learn_pool = catboost_train_train_work, params = catboost_fit_params)
    
    result_train_validate_home$score_catboost_home <- catboost.predict(catboost_model_home_for_validation, catboost_train_validate_home, prediction_type = "Probability")
    result_train_validate_work$score_catboost_work <- catboost.predict(catboost_model_work_for_validation, catboost_train_validate_work, prediction_type = "Probability")  
    
    pred_home <- result_train_validate[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
    pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
    
    catboost_param_grid$home_accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
    catboost_param_grid$work_accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)
    
  }
  
  optimal_catboost_home_params <- catboost_param_grid[which.max(catboost_param_grid$home_accuracy),]
  optimal_catboost_work_params <- catboost_param_grid[which.max(catboost_param_grid$work_accuracy),]









# SCORE TEST DATSET


  # Fit RandomForest for Home on Train
  xgb.model_home <- xgboost(
    data=data.matrix(result_train[,c(predictors),with=F]), label=as.numeric(result_train$target_home)-1,
    max_depth=10,
    subsample=1.0,
    nrounds=1000, 
    objective="binary:logistic", 
    eval_metric="auc")
  
  catboost_fit_params <- list(iterations = 1000,
                              thread_count = 16,
                              eval_metric = 'Accuracy',
                              border_count = 32,
                              depth = 10,
                              use_best_model = FALSE,
                              learning_rate = 0.03)
  
    
    
    catboost_model <- catboost.train(learn_pool = catboost_train_home, params = fit_params)
    
    result_test$score_home <- catboost.predict(catboost_model, catboost_test_home, prediction_type = "Probability")
    result_train$score_home <- catboost.predict(catboost_model, catboost_train_home, prediction_type = "Probability")
    
    # fi_home = data.frame(catboost_model$feature_importances)
    # fi$var = row.names(fi)
    # fi[order(-fi$catboost_model.feature_importances),]
    

    
    
    # Score Home on TEST
    # result_test$score_home <- predict(rf_model_home, result_test[,c(predictors),with=F], ntree=100)
    # result_test$score_home <- predict(glm_model_home, result_test[,c(predictors),with=F], type="response")
    # result_test$score_home <- predict(xgb.model_home, data.matrix(result_test[,c(predictors),with=F]))
    
    # Get Home Prediction for TEST
    pred_test_home <- result_test[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
    colnames(pred_test_home) <- c("customer_id","home_lat","home_lon")
  
  
  
  
  # Fit RandomForest for Work on Train
  xgb.model_work <- xgboost::xgboost(
    data=data.matrix(result_train[!is.na(work_lat) & !is.na(work_lon),c(predictors),with=F]), 
    label=as.numeric(result_train$target_work[!is.na(result_train$work_lat) & !is.na(result_train$work_lon)])-1, 
    nrounds=1000,
    subsample=1.0,
    max_depth=10,
    objective="binary:logistic", 
    eval_metric="auc")
  
  
  
  
  
  catboost_model <- catboost.train(learn_pool=catboost_train_work, params=fit_params)
  
  result_test$score_work <- catboost.predict(catboost_model, catboost_test_work, prediction_type = "Probability")
  result_train$score_work <- catboost.predict(catboost_model, catboost_train_work, prediction_type = "Probability")
  
  # Score Home on TEST
  # result_test$score_work <- predict(rf_model_work, result_test[,c(predictors),with=F], ntree=100)
  # result_test$score_work <- predict(glm_model_work, result_test[,c(predictors),with=F], type="response")
  result_test$score_work <- predict(xgb.model_work, data.matrix(result_test[,c(predictors),with=F]))
  
  # Get Home Prediction for TEST
  pred_test_work <- result_test[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
  colnames(pred_test_work) <- c("customer_id","work_lat","work_lon")
  
  
  pred <- merge(pred_test_work, pred_test_home, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
  
  pred <- rbind(pred, result_add)
  
  length(unique(pred$customer_id))
  
  write.csv(pred, "pred.csv", sep=",", row.names = F, col.names = T, quote=F)















rf_model_home <- randomForest(target_home~., data=result_train[,c(predictors,'target_home'),with=F], ntree=100)
result_test$score_home <- predict(rf_model_for_validation, home_validate_data[,c(predictors),with=F], ntree=100)



result_train_work <- result_train[is.na(result_train$work_lat)==FALSE & is.na(result_train$work_lon)==FALSE,]
model_work <- glm(target_work~., data=result_train_work[,c(predictors,'target_work'),with=F], family=binomial(logit))
result_train_work$score_work <- predict(model_work, result_train_work[,c(predictors),with=F], type="response")
AUC::auc(AUC::roc(result_train_work$score_home, result_train_work$target_home))

library(AUC)


pred_home <- result[order(customer_id,-score),][,head(.SD,1),by=.(customer_id)]
pred_work <- result[order(customer_id,-score),][,head(.SD,1),by=.(customer_id)]

predDist <- computeDist(pred_home$pos_atm_orig_lat, pred_home$pos_atm_orig_lon, pred_home$home_orig_lat, pred_home$home_orig_lon)
sum(predDist < 0.02) / nrow(pred_home[pred_home$target_home==1])



