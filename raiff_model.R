library(data.table)
library(catboost)

result = fread("output/raiff_attrs.csv", sep=",", header=T)

predictors <- colnames(result)[substring(colnames(result),1,3) == "top"]
predictors <- predictors[5:length(predictors)]
predictors_eps <- colnames(result)[substring(colnames(result),1,3) == "eps"]
predictors <- c(predictors, predictors_eps,  c("is_moscow","is_piter","is_other","center.dist","size_x","size_y","city_group",
                              "pos_amount",
                              "pos_amount_avg",
                              "pos_amount_cnt",
                              "pos_amount_min",
                              "pos_amount_max",
                              "pos_sat_trn_cnt",
                              "pos_sun_trn_cnt",
                              "pos_weekend_trn_cnt",
                              "pos_weekday_trn_cnt",
                              "pos_dow_cnt",
                              "pos_sat_amount",
                              "pos_sun_amount",
                              "pos_weekend_amount",
                              "pos_weekday_amount",
                              "cust_amount",
                              "cust_amount_avg",
                              "cust_amount_cnt",
                              "cust_amount_max",
                              "cust_weekend_trn_cnt",
                              "cust_weekend_trn_rate",
                              "cust_sat_trn_cnt",
                              "cust_sun_trn_cnt",
                              "cust_dow_cnt",
                              "cust_sat_amount",
                              "cust_sun_amount",
                              "cust_weekend_amount",
                              "cust_weekday_amount",
                              "pos_amount_rate",
                              "pos_cnt_rate",
                              "pos_amount_max_rate",
                              "pos_dow_cnt_rate",
                              "pos_sat_cnt_rate",
                              "pos_sun_cnt_rate",
                              "pos_sat_amount_rate",
                              "pos_sun_amount_rate",
                              "pos_weekend_amount_rate",
                              "pos_weekday_amount_rate",
                              "cluster_rank_by_size",
                              "cluster_rank_by_max_diss",
                              "cluster_rank_by_av_diss",
                              "cluster_rank_by_diameter",
                              "cluster_rank_by_separation",
                              "cluster_dist",
                              "cluster_diss_to_max_diss",
                              "cluster_cnt",
                              "diameter",
                              "separation",
                              "cluster_cnt",
                              "max_diss",
                              "av_diss",
                              "mcc",
                              "is_atm",
                              "cluster_mean_lat",
                              "cluster_mean_lon"
                              # "pos_atm_cell"
                             
))
predictors <- unique(predictors)



result$is_atm <- ifelse(result$is_atm == "Y", 1, 0)

# ToDO: delete
result$top_city_center_dist[is.na(result$top_city_center_dist)] <- mean(result$top_city_center_dist, na.rm=T)
result$is_moscow <- factor(ifelse(result$top_city=='MOSCOW',1,0))
result$is_piter <- factor(ifelse(result$top_city=='SAINT PETERSBURG',1,0))
result$is_other <- factor(ifelse(!result$top_city %in% c('MOSCOW','SAINT PETERSBURG'),1,0))
result$pos_sat_cnt_rate <- ifelse(is.na(result$pos_sat_cnt_rate),0,result$pos_sat_cnt_rate)
result$pos_sun_cnt_rate <- ifelse(is.na(result$pos_sun_cnt_rate),0,result$pos_sun_cnt_rate)
result$pos_sat_amount_rate <- ifelse(is.na(result$pos_sat_amount_rate),0,result$pos_sat_amount_rate)
result$pos_sun_amount_rate <- ifelse(is.na(result$pos_sun_amount_rate),0,result$pos_sun_amount_rate)
result$pos_weekend_amount_rate <- ifelse(is.na(result$pos_weekend_amount_rate),0,result$pos_weekend_amount_rate)
result$pos_weekday_amount_rate <- ifelse(is.na(result$pos_weekday_amount_rate),0,result$pos_weekday_amount_rate)
result$cluster_diss_to_max_diss <- ifelse(is.na(result$cluster_diss_to_max_diss), 0, result$cluster_diss_to_max_diss)

result$mcc <-factor(result$mcc)
result$city_group <- group.factor(factor(result$city), top.values=50)
result$mcc_group <- group.factor(factor(result$mcc), top.values=52)
#result$city_group1 <- group.factor(factor(result$city), top.values=100)
#result$city_group2 <- group.factor(factor(result$city), top.values=150)

# result$pos_atm_lat_round <- round(result$pos_atm_lat*10)
# result$pos_atm_lon_round <- round(result$pos_atm_lon*10)
# result$pos_atm_cell <- factor(paste(result$pos_atm_lat_round,result$pos_atm_lon_round,sep=""))


# Define base datasets

  result_train = result[result$df=="train",]
  result_test = result[result$df == "test",]
  
  
  
  # Work dataset is half size of home dataset
  result_train_home = result_train
  result_train_work = result_train[!is.na(result_train$work_lat) & !is.na(result_train$work_lon),]
  
  # ToDO: Delete!
  # result_train_work$work_dist <- sqrt((result_train_work$pos_atm_orig_lat - result_train_work$work_lat)^2 + (result_train_work$pos_atm_orig_lon - result_train_work$work_lon)^2)
  
  # Correct target variables
  # result_train_home$target_home <- factor(ifelse(result_train_home$target_home == 1 | result_train_home$home_dist <= 0.02, 1, 0))
  # result_train_work$target_work <- factor(ifelse(result_train_work$target_work == 1 | result_train_work$work_dist <= 0.02, 1, 0))

  result_train_home$target_home <- factor(ifelse(result_train_home$home_dist <= 0.02, 1, 0))
  result_train_work$target_work <- factor(ifelse(result_train_work$work_dist <= 0.01, 1, 0))
  
  # Divide Home & Work datasets into train/validate by customers
  train_home_customers <- unique(result_train_home$customer_id)
  train_work_customers <- unique(result_train_work$customer_id)
  train_train_home_customers = sample(train_home_customers, round(0.8*length(train_home_customers)), FALSE)
  train_train_work_customers = sample(train_work_customers, round(0.8*length(train_work_customers)), FALSE)
  train_validate_home_customers = setdiff(train_home_customers, train_train_home_customers)
  train_validate_work_customers = setdiff(train_work_customers, train_train_work_customers)
  
  result_train_train_home = result_train_home[result_train_home$customer_id %in% train_train_home_customers,]
  result_train_validate_home = result_train_home[result_train_home$customer_id %in%train_validate_home_customers,]
  
  result_train_train_work = result_train_work[result_train_work$customer_id %in% train_train_work_customers,]
  result_train_validate_work = result_train_work[result_train_work$customer_id %in%train_validate_work_customers,]
  
  print("Stats for datasets...")
  print(paste(
    "Train=", length(unique(result_train$customer_id)),
    " Test=", length(unique(result_test$customer_id)),
    " Train(Home)=", length(unique(result_train_home$customer_id)),
    " Train(Work)=", length(unique(result_train_work$customer_id)),
    " Train-Train(Home)", length(unique(result_train_train_home$customer_id)),
    " Train-Validate(Home)", length(unique(result_train_validate_home$customer_id)),
    " Train-Train(Work)=", length(unique(result_train_train_work$customer_id)),
    " Train-Validate(Work)=",length(unique(result_train_validate_work$customer_id)),sep=""))
  




# Define catboost datasets
  
  mcc_attribute_num <- which(predictors=="mcc")
  city_attribute_num <- which(predictors %in% c("city_group","city_group1","city_group2"))
  cell_attribute_num <- which(predictors=="pos_atm_cell")
  categorical_attrs = c(mcc_attribute_num,city_attribute_num,cell_attribute_num)

  catboost_train_home <- catboost.load_pool(data=result_train_home[,predictors,with=F], 
                                            label = as.numeric(result_train_home$target_home)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=categorical_attrs,
                                            thread_count = -1)
  catboost_test <- catboost.load_pool(data=result_test[,predictors,with=F], 
                                           feature_names = as.list(predictors),
                                           cat_features=categorical_attrs,
                                           thread_count = -1)
  
  catboost_train_work <- catboost.load_pool(data=result_train_work[,predictors,with=F], 
                                            label = as.numeric(result_train_work$target_work)-1,
                                            feature_names = as.list(predictors),
                                            cat_features=categorical_attrs,
                                            thread_count = -1)
  # catboost_test_work <- catboost.load_pool(data=result_test[,predictors,with=F],
  #                                          feature_names = as.list(predictors),
  #                                          cat_features=categorical_attrs,
  #                                          thread_count = -1)
  
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
    maxnodes=c(100),
    nodesize=c(5),
    sampsize=c(1.0)
  )
  rf_param_grid$home_accuracy <- NA
  rf_param_grid$work_accuracy <- NA
  rf_param_grid$accuracy <- NA
  
  # RandomForest works with 52 levels factors
  
  rf_predictors <- c(predictors[-mcc_attribute_num], "mcc_group")

  n_threads = 48
  registerDoMC(n_threads)
  
  for (i in 1:nrow(rf_param_grid))
  {
  
    params = rf_param_grid[i,]
    print(params)

    require(doMC)

    rf_model_home_for_validation <- foreach(thread = 1:n_threads, .combine=randomForest::combine, .multicombine=TRUE) %dopar%
    {
    
      randomForest(
        target_home~.,
        data=result_train_train_home[,c("target_home",rf_predictors),with=F], 
        ntree=params$ntree)

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
  
  catoost_param_grid = expand.grid(
    depth=c(6,8,10,12,14,16), 
    iterations=c(250),
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
      # eval_metric="Accuracy",
      loss_function = params$loss_function,
      border_count = 32,
      depth = params$depth,
      use_best_model = FALSE,
      learning_rate = 0.03,
      metric_period=100,
      l2_leaf_reg = params$l2_leaf_reg,
      thread_count=12)
    
    catboost_model_home_for_validation <- catboost.train(learn_pool = catboost_train_train_home, params = catboost_fit_params)
    catboost_model_work_for_validation <- catboost.train(learn_pool = catboost_train_train_work, params = catboost_fit_params)
    
    result_train_validate_home$score_catboost_home <- catboost.predict(catboost_model_home_for_validation, catboost_train_validate_home, prediction_type = "Probability")
    result_train_validate_work$score_catboost_work <- catboost.predict(catboost_model_work_for_validation, catboost_train_validate_work, prediction_type = "Probability")  
    
    pred_home <- result_train_validate_home[order(customer_id,-score_catboost_home),][,head(.SD,1),by=.(customer_id)]
    pred_work <- result_train_validate_work[order(customer_id,-score_catboost_work),][,head(.SD,1),by=.(customer_id)]
    
    # catboost_param_grid$home_accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
    # catboost_param_grid$work_accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)

    catboost_param_grid$home_accuracy[i] <- sum(pred_home$home_dist <= 0.02) / nrow(pred_home)
    catboost_param_grid$work_accuracy[i] <- sum(pred_work$work_dist <= 0.02) / nrow(pred_work)
    catboost_param_grid$accuracy[i] <- sum(pred_home$home_dist <= 0.02) + sum(pred_work$work_dist <= 0.02) / (nrow(pred_home) + nrow(pred_work))

    print(catboost_param_grid[i])
    
  }
  
  optimal_catboost_home_params <- catboost_param_grid[which.max(catboost_param_grid$home_accuracy),]
  optimal_catboost_work_params <- catboost_param_grid[which.max(catboost_param_grid$work_accuracy),]
  
  print(catboost_param_grid)









# SCORE TEST DATSET

    
    # Choose the best model
    
    catboost_fit_params_home <- list(iterations = 1000,
                                thread_count = 48,
                                loss_function = 'Logloss',
                                border_count = 32,
                                depth = 10,
                                l2_leaf_reg = 10,
                                use_best_model = FALSE,
                                learning_rate = 0.03)
    catboost_fit_params_work <- list(iterations = 1000,
                                     thread_count = 48,
                                     loss_function = 'Logloss',
                                     border_count = 32,
                                     depth = 8,
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



