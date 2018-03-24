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
    ntree=c(100), 
    maxnodes=c(200),
    nodesize=c(2)
    # sampsize=c(1.0)
  )
  rf_param_grid$home_accuracy <- NA
  rf_param_grid$work_accuracy <- NA
  rf_param_grid$accuracy <- NA
  
  # RandomForest works with 52 levels factors
  
  rf_predictors <- c(predictors[-mcc_attribute_num], "mcc_group")
  # rf_predictors <- numerics

  require(doMC)

  n_threads = 12
  registerDoMC(n_threads)
  
  datasets <- createTrainTest(0.8)
  result_train_train_home <- datasets$train_home
  result_train_train_work <- datasets$train_work
  result_train_validate_home <- datasets$validate_home
  result_train_validate_work <- datasets$validate_work
  
  result_train_train_home$target_home1 <- as.numeric(result_train_train_home$target_home) - 1
  result_train_train_work$target_work1 <- as.numeric(result_train_train_work$target_work) - 1
  
  result_train_train_home$target_home <- result_train_train_home$target_home1
  result_train_train_work$target_work <- result_train_train_work$target_work1
  
  for (i in 1:nrow(rf_param_grid))
  {
    

    params = rf_param_grid[i,]

    print(params)
  
    rf_model_home_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
    {
      
        randomForest(
          target_home~.,
          data=result_train_train_home[,c("target_home",rf_predictors),with=F],
          nodesize=params$nodesize,
          maxnodes=params$maxnodes,
          sampsize=params$sampsize,
          ntree=params$ntree)

    }

    
    rf_model_work_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
    {
      
        randomForest(
          target_work~.,
          data=result_train_train_work[,c("target_work",rf_predictors),with=F],
          nodesize=params$nodesize,
          maxnodes=params$maxnodes,
          sampsize=params$sampsize,
          ntree=params$ntree)

    }

      result_train_validate_home$score_home <- predict(rf_model_home_for_validation, result_train_validate_home[,c(rf_predictors),with=F], ntree=params$ntree)
      result_train_validate_work$score_work <- predict(rf_model_work_for_validation, result_train_validate_work[,c(rf_predictors),with=F], ntree=params$ntree)
  
      pred_home <- result_train_validate_home[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
      pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
  
      rf_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
      rf_param_grid$work_accuracy[i] <- sum(pred_work$home_dist <= 0.02) / nrow(pred_work)
      rf_param_grid$accuracy[i] <- (sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02)) / (nrow(pred_home) + nrow(pred_work))

      print(rf_param_grid[i,])
   
  }
  
  optimal_rf_home_params <- rf_param_grid[which.max(rf_param_grid$home_accuracy),]
  optimal_rf_work_params <- rf_param_grid[which.max(rf_param_grid$work_accuracy),]
  
  print(rf_param_grid)
  
  
  
  
  # Alogirithm2: CatBoost
  
  catboost_param_grid = expand.grid(
    depth=c(12),
    iterations=c(1000),
    ignored_features=c(1000),
    border_count=c(32),
    l2_leaf_reg = c(10),
    # eval_metric=c("Accuracy","AUC"),
    loss_function=c("Logloss")
  )
  
  require(doMC)
  
  registerDoMC(4)
  cv_sets = 4
  
  catboost_param_grid$home_accuracy <- NA
  catboost_param_grid$work_accuracy <- NA
  catboost_param_grid$best_iterations_home <- NA
  catboost_param_grid$best_iterations_work <- NA
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
      border_count = params$border_count,
      depth = params$depth,
      use_best_model = F,
      learning_rate = 0.03,
      metric_period=1000,
      thread_count=ifelse(cv_sets==1,-1,12),
      l2_leaf_reg = params$l2_leaf_reg)
    
    # Init data to store performance between CV runs
    # cv_accuracy_home <- c()
    # cv_accuracy_work <- c()
    # cv_accuracy <- c()
    # n_trees <- c()
    # best_iterations_home <- c()
    # best_iterations_work <- c()
    
    
    
    # Cross-Validation parallel loop
    cv_result <- foreach (cv = 1:cv_sets, .combine=rbind) %dopar%
    {
      
      # Generate CV datasets
      
        datasets <- createTrainTest(0.75)
        result_train_train_home <- datasets$train_home
        result_train_train_work <- datasets$train_work
        result_train_validate_home <- datasets$validate_home
        result_train_validate_work <- datasets$validate_work
      
      # Convert to catboost datasets
      
        catboost_train_train_home <- catboost.load_pool(data=result_train_train_home[,predictors,with=F], 
                                                        label = as.numeric(result_train_train_home$target_home)-1,
                                                        feature_names = as.list(predictors),
                                                        cat_features=categorical_attrs,
                                                        thread_count = -1)
        catboost_train_validate_home <- catboost.load_pool(data=result_train_validate_home[,predictors,with=F], 
                                                           label = as.numeric(result_train_validate_home$target_home)-1,
                                                           feature_names = as.list(predictors),
                                                           cat_features=categorical_attrs,
                                                           thread_count = -1)
        
        catboost_train_train_work <- catboost.load_pool(data=result_train_train_work[,predictors,with=F], 
                                                        label = as.numeric(result_train_train_work$target_work)-1,
                                                        feature_names = as.list(predictors),
                                                        cat_features=categorical_attrs,
                                                        thread_count = -1)
        catboost_train_validate_work <- catboost.load_pool(data=result_train_validate_work[,predictors,with=F], 
                                                           label = as.numeric(result_train_validate_work$target_work)-1,
                                                           feature_names = as.list(predictors),
                                                           cat_features=categorical_attrs,
                                                           thread_count = -1)
    
      # Fit models
        
        print(paste("Starting model fit thread=", cv, sep=""))
        
        if (cv == 1)
          catboost_fit_params["metric_period"] <- 5
        
      
        catboost_model_home_for_validation <- catboost.train(learn_pool = catboost_train_train_home, test_pool=catboost_train_validate_home, params = catboost_fit_params)
        catboost_model_work_for_validation <- catboost.train(learn_pool = catboost_train_train_work, test_pool=catboost_train_validate_work, params = catboost_fit_params)

      
      # Score validation data
      
        result_train_validate_home$score_catboost_home <- catboost.predict(catboost_model_home_for_validation, catboost_train_validate_home, prediction_type = "Probability")
        result_train_validate_work$score_catboost_work <- catboost.predict(catboost_model_work_for_validation, catboost_train_validate_work, prediction_type = "Probability")  
        
      
      # Make predictions for validation data
      
        pred_home <- result_train_validate_home[order(customer_id,-score_catboost_home),][,head(.SD,1),by=.(customer_id)]
        pred_work <- result_train_validate_work[order(customer_id,-score_catboost_work),][,head(.SD,1),by=.(customer_id)]
      
      
      # Store performance for curent model fit
      
        cv_accuracy_home <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
        cv_accuracy_work <- sum(pred_work$work_dist <= 0.02) / nrow(pred_work)
        cv_accuracy <- (sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02)) / (nrow(pred_home) + nrow(pred_work))
        best_iterations_home <- catboost_model_home_for_validation$tree_count
        best_iterations_work <- catboost_model_work_for_validation$tree_count
      
        data.frame(cv_accuracy_home,cv_accuracy_work,cv_accuracy,best_iterations_home,best_iterations_work)

    }
    
    
    # Average performance for multiple runs
    
      catboost_param_grid$home_accuracy[i] <- mean(cv_result$cv_accuracy_home)
      catboost_param_grid$home_accuracy_sd[i] <- sd(cv_result$cv_accuracy_home)
      catboost_param_grid$work_accuracy[i] <- mean(cv_result$cv_accuracy_work)
      catboost_param_grid$work_accuracy_sd[i] <- sd(cv_result$cv_accuracy_work)
      catboost_param_grid$accuracy[i] <- mean(cv_result$cv_accuracy)
      catboost_param_grid$accuracy_sd[i] <- sd(cv_result$cv_accuracy)
      catboost_param_grid$best_iterations_home[i] <- mean(cv_result$best_iterations_home)
      catboost_param_grid$best_iterations_home_sd[i] <- max(cv_result$best_iterations_home) - min(cv_result$best_iterations_home)
      catboost_param_grid$best_iterations_work[i] <- mean(cv_result$best_iterations_work)
      catboost_param_grid$best_iterations_work_sd[i] <- max(cv_result$best_iterations_work) - min(cv_result$best_iterations_work)

    print(catboost_param_grid[i,])
    
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

  

  # Alogirithm5: LightGBM
  
  require(lightGBM)
  
  lightgbm_param_grid = expand.grid(
    depth=c(10), 
    iterations=c(100),
    ignored_features=c(1000),
    l2_leaf_reg = c(10),
    # eval_metric=c("Accuracy","AUC"),
    loss_function=c("Logloss")
  )
  
  cv_sets = 1
  
  lightgbm_param_grid$home_accuracy <- NA
  lightgbm_param_grid$work_accuracy <- NA
  lightgbm_param_grid$best_iterations_home <- NA
  lightgbm_param_grid$best_iterations_work <- NA
  lightgbm_param_grid$accuracy <- NA  
  
  print("Starting optimize hyperparameters")
  print(lightgbm_param_grid)
  
  for (i in 1:nrow(lightgbm_param_grid))
  {
    
    params = lightgbm_param_grid[i,]
    print(params)
    
    # catboost_fit_params <- list(iterations = params$iterations,
    #                             #eval_metric = params$eval_metric,
    #                             eval_metric="Accuracy",
    #                             loss_function = params$loss_function,
    #                             border_count = 32,
    #                             depth = params$depth,
    #                             use_best_model = TRUE,
    #                             learning_rate = 0.03,
    #                             metric_period=50,
    #                             l2_leaf_reg = params$l2_leaf_reg,
    #                             thread_count=12)
    # 
    # # Init data to store performance between CV runs
    # cv_accuracy_home <- c()
    # cv_accuracy_work <- c()
    # cv_accuracy <- c()
    # n_trees <- c()
    # best_iterations_home <- c()
    # best_iterations_work <- c()
    
    # Cross-Validation loop
    for (cv in 1:cv_sets)
    {
      
      # Generate CV datasets
      
      datasets <- createTrainTest(0.8)
      result_train_train_home <- datasets$train_home
      result_train_train_work <- datasets$train_work
      result_train_validate_home <- datasets$validate_home
      result_train_validate_work <- datasets$validate_work
      
      # Convert to catboost datasets
      
      catboost_train_train_home <- catboost.load_pool(data=result_train_train_home[,predictors,with=F], 
                                                      label = as.numeric(result_train_train_home$target_home)-1,
                                                      feature_names = as.list(predictors),
                                                      cat_features=categorical_attrs,
                                                      thread_count = -1)
      catboost_train_validate_home <- catboost.load_pool(data=result_train_validate_home[,predictors,with=F], 
                                                         label = as.numeric(result_train_validate_home$target_home)-1,
                                                         feature_names = as.list(predictors),
                                                         cat_features=categorical_attrs,
                                                         thread_count = -1)
      
      catboost_train_train_work <- catboost.load_pool(data=result_train_train_work[,predictors,with=F], 
                                                      label = as.numeric(result_train_train_work$target_work)-1,
                                                      feature_names = as.list(predictors),
                                                      cat_features=categorical_attrs,
                                                      thread_count = -1)
      catboost_train_validate_work <- catboost.load_pool(data=result_train_validate_work[,predictors,with=F], 
                                                         label = as.numeric(result_train_validate_work$target_work)-1,
                                                         feature_names = as.list(predictors),
                                                         cat_features=categorical_attrs,
                                                         thread_count = -1)
      
      # Fit models
      
      dtrain_home <- lgb.Dataset(data = as.matrix(result_train_train_home[,numerics,with=F]), label = as.numeric(result_train_train_home$target_home)-1, free_raw_data = FALSE)
      dtest_home <-  lgb.Dataset(data = as.matrix(result_train_validate_home[,numerics,with=F]), label = as.numeric(result_train_validate_home$target_home)-1, free_raw_data = FALSE)
      dtrain_work <- lgb.Dataset(data = as.matrix(result_train_train_work[,numerics,with=F]), label = as.numeric(result_train_train_work$target_work)-1, free_raw_data = FALSE)
      dtest_work <-  lgb.Dataset(data = as.matrix(result_train_validate_work[,numerics,with=F]), label = as.numeric(result_train_validate_work$target_work)-1, free_raw_data = FALSE)
      valids_home <- list(train = dtrain_home, test = dtest_home)
      valids_work <- list(train = dtrain_home, test = dtest_home)
      
      lightgbm_model_home_for_validation <- lightgbm(
        data = dtrain_home,
        # valids = valids,
        max_depth=10,
        num_leaves=5,
        nrounds=1000, 
        objective="binary", 
        nthread=12)
      lightgbm_model_work_for_validation <- lightgbm(
        data = dtrain_work,
        max_depth=10,
        num_leaves=5,
        nrounds=1000, 
        objective="binary", 
        nthread=12)
      
      
      # Score validation data
      
      result_train_validate_home$score_home <- predict(lightgbm_model_home_for_validation, dtest_home)
      result_train_validate_work$score_work <- predict(lightgbm_model_work_for_validation, dtest_work)  
      
      
      # Make predictions for validation data
      
      pred_home <- result_train_validate_home[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]
      pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
      
      
      # Store performance for curent model fit
      
      cv_accuracy_home <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
      cv_accuracy_work <- sum(pred_work$work_dist <= 0.02) / nrow(pred_work)
      cv_accuracy <- (sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02)) / (nrow(pred_home) + nrow(pred_work))
      best_iterations_home <- catboost_model_home_for_validation$tree_count
      best_iterations_work <- catboost_model_work_for_validation$tree_count
      
      
      
    }
    
    
    # Average performance for multiple runs
    
    catboost_param_grid$home_accuracy[i] <- mean(cv_accuracy_home)
    catboost_param_grid$home_accuracy_sd[i] <- sd(cv_accuracy_home)
    catboost_param_grid$work_accuracy[i] <- mean(cv_accuracy_work)
    catboost_param_grid$work_accuracy_sd[i] <- sd(cv_accuracy_work)
    catboost_param_grid$accuracy[i] <- mean(cv_accuracy)
    catboost_param_grid$accuracy_sd[i] <- sd(cv_accuracy)
    catboost_param_grid$best_iterations_home[i] <- mean(best_iterations_home)
    catboost_param_grid$best_iterations_home_sd[i] <- max(best_iterations_home) - min(best_iterations_home)
    catboost_param_grid$best_iterations_work[i] <- mean(best_iterations_work)
    catboost_param_grid$best_iterations_work_sd[i] <- max(best_iterations_work) - min(best_iterations_work)
    
    print(catboost_param_grid[i,])
    
  }
  
  optimal_catboost_home_params <- catboost_param_grid[which.max(catboost_param_grid$home_accuracy),]
  optimal_catboost_work_params <- catboost_param_grid[which.max(catboost_param_grid$work_accuracy),]
  
  print(catboost_param_grid)





  
  
  
  
  
  
  
  
  
  
  
  
  # Optimize Decision Rule
  datasets <- createTrainTest(0.8)
  result_train_train_home <- datasets$train_home
  result_train_train_work <- datasets$train_work
  result_train_validate_home <- datasets$validate_home
  result_train_validate_work <- datasets$validate_work
  
  # Convert to catboost datasets
  
  catboost_train_train_home <- catboost.load_pool(data=result_train_train_home[,predictors,with=F], 
                                                  label = as.numeric(result_train_train_home$target_home)-1,
                                                  feature_names = as.list(predictors),
                                                  cat_features=categorical_attrs,
                                                  thread_count = -1)
  catboost_train_validate_home <- catboost.load_pool(data=result_train_validate_home[,predictors,with=F], 
                                                     label = as.numeric(result_train_validate_home$target_home)-1,
                                                     feature_names = as.list(predictors),
                                                     cat_features=categorical_attrs,
                                                     thread_count = -1)
  
  catboost_train_train_work <- catboost.load_pool(data=result_train_train_work[,predictors,with=F], 
                                                  label = as.numeric(result_train_train_work$target_work)-1,
                                                  feature_names = as.list(predictors),
                                                  cat_features=categorical_attrs,
                                                  thread_count = -1)
  catboost_train_validate_work <- catboost.load_pool(data=result_train_validate_work[,predictors,with=F], 
                                                     label = as.numeric(result_train_validate_work$target_work)-1,
                                                     feature_names = as.list(predictors),
                                                     cat_features=categorical_attrs,
                                                     thread_count = -1)
  
  catboost_fit_params_home <- list(iterations = 1000,
                                   thread_count = 12,
                                   loss_function = 'Logloss',
                                   border_count = 32,
                                   depth = 12,
                                   l2_leaf_reg = 10,
                                   use_best_model = FALSE,
                                   learning_rate = 0.03)
  catboost_fit_params_work <- list(iterations = 1000,
                                   thread_count = 12,
                                   loss_function = 'Logloss',
                                   border_count = 32,
                                   depth = 12,
                                   use_best_model = FALSE,
                                   learning_rate = 0.03)
  
  # Build models
  
    catboost_model_home <- catboost.train(learn_pool = catboost_train_train_home, params = catboost_fit_params_home)
    catboost_model_work <- catboost.train(learn_pool = catboost_train_train_work, params = catboost_fit_params_work)
    
    result_train_validate_home$score_home <- catboost.predict(catboost_model_home, catboost_train_validate_home, prediction_type = "Probability")
    result_train_validate_work$score_work <- catboost.predict(catboost_model_work, catboost_train_validate_work, prediction_type = "Probability")
    
  
  get_averaged_prediction <- function(df, cutoff)
  {
 
    top_scores <- df[,.(
      p1_score=score_home[1], 
      p2_score=score_home[2], 
      p1_lat=pos_atm_orig_lat[1], 
      p1_lon=pos_atm_orig_lon[1], 
      p2_lat=pos_atm_orig_lat[2], 
      p2_lon=pos_atm_orig_lon[2]),
      by=.(customer_id)]
    
    top_scores$p1_p2_dist <- sqrt((top_scores$p1_lat-top_scores$p2_lat)^2 + (top_scores$p1_lon-top_scores$p2_lon)^2)
    top_scores$p1_p2_avg_lat <- (top_scores$p1_lat + top_scores$p2_lat) / 2
    top_scores$p1_p2_avg_lon <- (top_scores$p1_lon + top_scores$p2_lon) / 2
    
    top_scores$pred_lat <- ifelse(!is.na(top_scores$p1_p2_dist) & top_scores$p1_p2_dist < cutoff, top_scores$p1_p2_avg_lat, top_scores$p1_lat)
    top_scores$pred_lon <- ifelse(!is.na(top_scores$p1_p2_dist) & top_scores$p1_p2_dist < cutoff, top_scores$p1_p2_avg_lon, top_scores$p1_lon)
    
    return (top_scores[,.(customer_id, pred_lat, pred_lon)])
  
  }
  
  result_train$score_home <- catboost.predict(catboost_model_home, catboost_train, prediction_type = "Probability")
  result_train$score_work <- catboost.predict(catboost_model_work, catboost_train, prediction_type = "Probability")
  
  result_train_ordered_home <- result_train[order(customer_id,-score_home),]
  result_train_ordered_work <- result_train[order(customer_id,-score_work),]
    
  averaged_home <- get_averaged_prediction(result_train_ordered_home, 0.03)
  averaged_work <- get_averaged_prediction(result_train_ordered_work, 0.03)
  
  
  # Check performance of averaging
  
    averaging_home_check <- merge(result_train_home[,.(head(home_orig_lat,1), head(home_orig_lon,1)),by=customer_id], averaged_home, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
    averaging_work_check <- merge(result_train_work[,.(head(work_orig_lat,1), head(work_orig_lon,1)),by=customer_id], averaged_work, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
    
    averaging_home_check$home_dist <- computeDist(averaging_home_check$pred_lat, averaging_home_check$pred_lon, averaging_home_check$home_orig_lat, averaging_home_check$home_orig_lon)
    averaging_work_check$work_dist <- computeDist(averaging_work_check$pred_lat, averaging_work_check$pred_lon, averaging_work_check$work_orig_lat, averaging_work_check$work_orig_lon)
    home_accuracy <- sum(averaging_home_check$home_dist1 < 0.02) / nrow(averaging_home_check)
    work_accuracy <- sum(averaging_work_check$work_dist1 < 0.02) / nrow(averaging_work_check)
    
    print(paste("home accuracy = ",home_accuracy," work accuracy = ", work_accuracy, sep=""))  
  
    
    
    
    
    
  # Analyze errors
  # pred2_test_home$home_error_lat <- pred2_test_home$home_orig_lat - pred2_test_home$p1_lat
  # pred2_test_home$home_error_lon <- pred2_test_home$home_orig_lon - pred2_test_home$p1_lon
  # pred2_test_work$work_error_lat <- pred2_test_work$work_orig_lat - pred2_test_work$p1_lat
  # pred2_test_work$work_error_lon <- pred2_test_work$work_orig_lon - pred2_test_work$p1_lon
  # 
  # print(paste("mean home_error_lat=",mean(pred2_test_home$home_error_lat),sep=""))
  # print(paste("mean home_error_lon=",mean(pred2_test_home$home_error_lon),sep=""))
  # print(paste("mean work_error_lat=",mean(pred2_test_work$work_error_lat),sep=""))
  # print(paste("mean work_error_lat=",mean(pred2_test_work$work_error_lon),sep=""))
  # 
  # computeDist <- function(x1, y1, x2, y2)
  # {
  #   sqrt((x1 - x2)^2 + (y1 - y2)^2)
  # }
  
  
  
  pred2 <- merge(pred2_test_work[,.(customer_id,work_lat,work_lon)], pred2_test_home[,.(customer_id, home_lat, home_lon)], by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)
  
  
  
  
  

