# Optimize hyperparameters
  
  # Alogirithm1: XGB

  xgb_param_grid = expand.grid(
    max_depth=c(10,15), 
    nrounds=c(500),
    gamma=c(0.01,0.1,1.0),
    subsample=c(1.0)
  )
  xgb_param_grid$home_accuracy <- NA
  xgb_param_grid$work_accuracy <- NA
  xgb_param_grid$accuracy <- NA
  
  for (i in 1:nrow(xgb_param_grid))
  {
  
    params = xgb_param_grid[i,]
    print(params)
    
    xgb_model_home_for_validation <- xgboost(
      data=data.matrix(result_train_train_home[,predictors[-c(88,138)],with=F]), 
      label=as.numeric(result_train_train_home$target_home)-1,
      nrounds=params$nrounds,
      max_depth=params$max_depth,
      subsample=params$subsample,
      nthread=12,
      print_every=50,
      objective="binary:logistic")
    
    xgb_model_work_for_validation <- xgboost(
      data=data.matrix(result_train_train_work[,predictors[-c(88,138)],with=F]), 
      label=as.numeric(result_train_train_work$target_work)-1,
      nrounds=params$nrounds,
      max_depth=params$max_depth,
      subsample=params$subsample,
      print_every=50,
      nthread=12,
      objective="binary:logistic")
    
        result_train_validate_home$score_home <- predict(xgb_model_home_for_validation, data.matrix(result_train_validate_home[,c(predictors),with=F]))
        result_train_validate_work$score_work <- predict(xgb_model_work_for_validation, data.matrix(result_train_validate_work[,c(predictors),with=F]))
  
        pred_home <- result_train_validate_home[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
        pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
  
        xgb_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
        xgb_param_grid$work_accuracy[i] <- sum(pred_work$home_dist <= 0.02) / nrow(pred_work)
        xgb_param_grid$accuracy[i] <- sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02) / (nrow(pred_home) + nrow(pred_work))

        print(xgb_param_grid[i])
   
  }
  
  optimal_xgb_home_params <- xgb_param_grid[which.max(xgb_param_grid$home_accuracy),]
  optimal_xgb_work_params <- xgb_param_grid[which.max(xgb_param_grid$work_accuracy),]
  
  print(xgb_param_grid)


  # Alogirithm1: RandomForest

  require(randomForest)

  rf_param_grid = expand.grid(
    ntree=c(1), 
    maxnodes=c(100),
    nodesize=c(5),
    sampsize=c(1.0)
  )
  rf_param_grid$home_accuracy <- NA
  rf_param_grid$work_accuracy <- NA
  rf_param_grid$accuracy <- NA
  
  # RandomForest works with 52 levels factors
  
  rf_predictors <- c(predictors[-mcc_attribute_num], "mcc_group")

   require(doMC)

  n_threads = 12
  registerDoMC(n_threads)
  
  for (i in 1:nrow(rf_param_grid))
  {
    

    params = rf_param_grid[i,]

    print(params)
  
   p_time <- system.time(
  
    {  

      rf_model_home_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
      {
      
        randomForest(
          target_home~.,
          data=result_train_train_home[,c("target_home",rf_predictors),with=F],
          nodesize=300,
          ntree=1)

      }

    }

   )

    print(p_time)


    p_time <- system.time(
  
    {  

      rf_model_home_for_validation <- foreach(thread = 1:n_threads) %dopar%
      {
      
        randomForest(
          target_home~.,
          data=result_train_train_home[,c("target_home",rf_predictors[1:50]),with=F],
          nodesize=300,
          ntree=10)

      }

    }

   )

    print(p_time)

    rf_model_home_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
    {
    
      randomForest(
        target_home~.,
        data=result_train_train_home[,c("target_home",rf_predictors),with=F], 
        ntree=1)

    }
    
    rf_model_work_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
    {

      rf_model_work_for_validation <- randomForest(
        target_work~.,
        data=result_train_train_work[,c("target_work",rf_predictors),with=F], 
        ntree=100)

    }
    
        result_train_validate_home$score_home <- predict(rf_model_home_for_validation, result_train_validate_home[,c(rf_predictors),with=F], ntree=params$ntree)
        result_train_validate_work$score_work <- predict(rf_model_work_for_validation, result_train_validate_work[,c(rf_predictors),with=F], ntree=params$ntree)
  
        pred_home <- result_train_validate_home[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
        pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
  
        rf_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
        rf_param_grid$work_accuracy[i] <- sum(pred_work$home_dist <= 0.02) / nrow(pred_work)
        rf_param_grid$accuracy[i] <- sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02) / (nrow(pred_home) + nrow(pred_work))

        print(rf_param_grid[i])
   
  }
  
  optimal_xgb_home_params <- xgb_param_grid[which.max(xgb_param_grid$home_accuracy),]
  optimal_xgb_work_params <- xgb_param_grid[which.max(xgb_param_grid$work_accuracy),]
  
  print(xgb_param_grid)
  
  
  
  
  # Alogirithm2: CatBoost
  
  catboost_param_grid = expand.grid(
    depth=c(10,10), 
    iterations=c(2000),
    ignored_features=c(1000),
    l2_leaf_reg = c(10),
    # eval_metric=c("Accuracy","AUC"),
    loss_function=c("Logloss")
  )
  catboost_param_grid$home_accuracy <- NA
  catboost_param_grid$work_accuracy <- NA
  catboost_param_grid$accuracy <- NA  
  
  print("Starting optimize hyperparameters")
  print(catboost_param_grid)
  
  for (i in 1:nrow(catboost_param_grid))
  {
    
    params = catboost_param_grid[i,]
    print(params)
    
    catboost_fit_params <- list(iterations = params$iterations,
      #eval_metric = params$eval_metric,
      eval_metric="Accuracy",
      loss_function = params$loss_function,
      border_count = 32,
      depth = params$depth,
      use_best_model = TRUE,
      learning_rate = 0.03,
      metric_period=100,
      l2_leaf_reg = params$l2_leaf_reg,
      thread_count=12)
    
    catboost_model_home_for_validation <- catboost.train(learn_pool = catboost_train_train_home, test_pool=catboost_train_validate_home, params = catboost_fit_params)
    catboost_model_work_for_validation <- catboost.train(learn_pool = catboost_train_train_work, params = catboost_fit_params)
    
    result_train_validate_home$score_catboost_home <- catboost.predict(catboost_model_home_for_validation, catboost_train_validate_home, prediction_type = "Probability")
    result_train_validate_work$score_catboost_work <- catboost.predict(catboost_model_work_for_validation, catboost_train_validate_work, prediction_type = "Probability")  
    
    pred_home <- result_train_validate_home[order(customer_id,-score_catboost_home),][,head(.SD,1),by=.(customer_id)]
    pred_work <- result_train_validate_work[order(customer_id,-score_catboost_work),][,head(.SD,1),by=.(customer_id)]
    
    # catboost_param_grid$home_accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
    # catboost_param_grid$work_accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)

    catboost_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
    catboost_param_grid$work_accuracy[i] <- sum(pred_work$work_dist <= 0.02) / nrow(pred_work)
    catboost_param_grid$accuracy[i] <- (sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02)) / (nrow(pred_home) + nrow(pred_work))

    print(catboost_param_grid[i])
    
  }
  
  optimal_catboost_home_params <- catboost_param_grid[which.max(catboost_param_grid$home_accuracy),]
  optimal_catboost_work_params <- catboost_param_grid[which.max(catboost_param_grid$work_accuracy),]
  
  print(catboost_param_grid)

  
  

  # Alogirithm4: GBM

  require(gbm)
  
  gbm_param_grid = expand.grid(
    distribution=c("bernoulli","huberized"), 
    n.trees=c(100),
    interaction.depth=c(1),
    bag.fraction = c(1.0)
    # l2_leaf_reg = c(10),
    # eval_metric=c("Accuracy","AUC"),
    # loss_function=c("Logloss")
  )
  gbm_param_grid$home_accuracy <- NA
  gbm_param_grid$work_accuracy <- NA
  gbm_param_grid$accuracy <- NA

  result_train_train_home$taret_home <- as.numeric(result_train_train_home$taret_home)
  result_train_train_home$taret_work <- as.numeric(result_train_train_work$taret_work)
  
  print("Starting optimize hyperparameters")
  print(gbm_param_grid)
  
  for (i in 1:nrow(gbm_param_grid))
  {
    
    params = gbm_param_grid[i,]
    print(params)
    
    print(system.time(
      gbm_model_home_for_validation<-gbm(
        formula = target_home~.,
        distribution="bernoulli",
        bag.fraction = c(1.0),
        data=data.frame(result_train_train_home[,c("target_home",predictors),with=F]),
        n.trees=100,
        n.cores=12)
    ))

    print(system.time(
      gbm_model_work_for_validation<-gbm(
        target_work~.,
        distribution=as.character(params$distribution),
        bag.fraction = c(1.0),
        data=data.frame(result_train_train_home[,c("target_work",predictors[10:20]),with=F]),
        n.trees=params$n.trees,
        n.cores=12)
    ))
    
    result_train_validate_home$score_gbm_home <- predict(
      gbm_model_home_for_validation, 
      newdata=data.frame(result_train_validate_home[,predictors,with=F]), 
      n.trees=10, 
      type = "response")
    result_train_validate_work$score_gbm_work <- predict(
      gbm_model_work_for_validation, 
      data=data.frame(result_train_validate_work[,predictors[10:20],with=F]), 
      n.trees=params$n.trees, 
      type = "response")  
    
    pred_home <- result_train_validate_home[order(customer_id,-score_gbm_home),][,head(.SD,1),by=.(customer_id)]
    pred_work <- result_train_validate_work[order(customer_id,-score_gbm_work),][,head(.SD,1),by=.(customer_id)]
    
    # catboost_param_grid$home_accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
    # catboost_param_grid$work_accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)

    gbm_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
    gbm_param_grid$work_accuracy[i] <- sum(pred_work$work_dist <= 0.02) / nrow(pred_work)
    gbm_param_grid$accuracy[i] <- (sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02)) / (nrow(pred_home) + nrow(pred_work))

    print(gbm_param_grid[i,])
    
  }
  
  optimal_gbm_home_params <- gbm_param_grid[which.max(gbm_param_grid$home_accuracy),]
  optimal_gbm_work_params <- gbm_param_grid[which.max(gbm_param_grid$work_accuracy),]
  
  print(gbm_param_grid)

  





# SCORE TEST DATSET

    
    # Choose the best model
    
    catboost_fit_params_home <- list(iterations = 1000,
                                thread_count = 48,
                                loss_function = 'Logloss',
                                border_count = 32,
                                depth = 12,
                                l2_leaf_reg = 10,
                                use_best_model = FALSE,
                                learning_rate = 0.03)
    catboost_fit_params_work <- list(iterations = 1000,
                                     thread_count = 48,
                                     loss_function = 'Logloss',
                                     border_count = 32,
                                     depth = 12,
                                     use_best_model = FALSE,
                                     learning_rate = 0.03)
  
    catboost_model_home <- catboost.train(learn_pool = catboost_train_home, params = catboost_fit_params_home)
    catboost_model_work <- catboost.train(learn_pool = catboost_train_work, params = catboost_fit_params_work)
    
    result_test$score_home <- catboost.predict(catboost_model_home, catboost_test, prediction_type = "Probability")
    result_test$score_work <- catboost.predict(catboost_model_work, catboost_test, prediction_type = "Probability")
    
    # Get Home Prediction for TEST
    pred_test_home <- result_test[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
    colnames(pred_test_home) <- c("customer_id","home_lat","home_lon")
    
    # Get Home Prediction for TEST
    pred_test_work <- result_test[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)][,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon")]
    colnames(pred_test_work) <- c("customer_id","work_lat","work_lon")
  
  
  
  
  
    # Create output dataset
    
    pred <- merge(pred_test_work, pred_test_home, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
    pred <- rbind(pred, result_add)
    
    length(unique(pred$customer_id))
    
    write.csv(pred, "output/pred.csv", sep=",", row.names = F, col.names = T, quote=F)



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



