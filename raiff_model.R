library(data.table)

result = fread("result.csv", sep=";")



library(xgboost)







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
                              "city_pop",
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

x<-data.frame(table(result$mcc))predictors[]
x[order(-x$Freq),]$Var1[1:10]

# result$is_6011 = ifelse(result$mcc=="6011",1,0) 
# result$is_5411 = ifelse(result$mcc=="5411",1,0)
# result$is_5814 = ifelse(result$mcc=="5814",1,0)
# result$is_5912 = ifelse(result$mcc=="5912",1,0)
# result$is_5812 = ifelse(result$mcc=="5812",1,0)
# result$is_5541 = ifelse(result$mcc=="5541",1,0)
# result$is_5499 = ifelse(result$mcc=="5499",1,0)
# result$is_4111 = ifelse(result$mcc=="4111",1,0)
# result$is_5691 = ifelse(result$mcc=="5691",1,0)
# result$is_5977 = ifelse(result$mcc=="5977",1,0)
# result$is_5921 = ifelse(result$mcc=="5921",1,0)
# result$is_5331 = ifelse(result$mcc=="5331",1,0)
# result$is_5999 = ifelse(result$mcc=="5999",1,0)
# result$is_5261 = ifelse(result$mcc=="5261",1,0)
# result$is_5661 = ifelse(result$mcc=="5661",1,0)
# result$is_5651 = ifelse(result$mcc=="5651",1,0)
# result$is_5211 = ifelse(result$mcc=="5211",1,0)
# result$is_5311 = ifelse(result$mcc=="5311",1,0)
# result$is_5641 = ifelse(result$mcc=="5641",1,0)
# result$is_8099 = ifelse(result$mcc=="8099",1,0)

result_train = result[result$df=="train",]
result_test = result[result$df == "test",]

result_train$target_home <- factor(ifelse(result_train$target_home == 1 | result_train$home_dist <= 0.02, 1, 0))
result_train$target_work <- factor(ifelse(result_train$target_work == 1 | result_train$home_dist <= 0.02, 1, 0))






# FIND BEST PARAMS ON TRAIN DATASET

train_customers <- unique(result_train$customer_id)
train_train_customers = sample(train_customers, 7500, FALSE)
train_validate_customers = setdiff(train_customers, train_train_customers)

result_train_train = result_train[result_train$customer_id %in% train_train_customers,]
result_train_validate = result_train[result_train$customer_id %in%train_validate_customers,]

result_train_train_work = result_train_train[!is.na(work_lat) & !is.na(work_lon),]
result_train_validate_work = result_train_validate[!is.na(work_lat) & !is.na(work_lon),]


maxdepth=c(10,15)
nrounds=c(100,1000)
subsample=c(1.0)

grid_search_size = length(maxdepth) * length(nrounds) * length(subsample)
xgb_home_param_sets <- data.frame(maxdepth=numeric(grid_search_size), nrounds=numeric(grid_search_size), subsample=numeric(grid_search_size), accuracy=numeric(grid_search_size))

i <- 0


for (i1 in maxdepth)
{
  for (i2 in nrounds)
  {
    for (i3 in subsample)
    {
      
      print(paste("maxdepth=",i1," nrounds=",i2," subsample=",i3,sep=""))
      i <- i +1
  
      # rf_model_for_validation <- randomForest(target_home~., data=result_train_train[,c(predictors,'target_home'),with=F], ntree=100)
      # glm_model_for_validation <- glm(target_home~., data=home_train_data[,c(predictors,'target_home'),with=F], family=binomial(logit))
  
      p_max_depth = i1
      p_maxrounds = i2
      p_subsample = i3
      
      xgb_home_param_sets$maxdepth[i] <- i1
      xgb_home_param_sets$nrounds[i] <- i2
      xgb_home_param_sets$subsample[i] <- i3
  
      xgb_model_home_for_validation <- xgboost(
        data=data.matrix(result_train_train[,c(predictors),with=F]), 
        label=as.numeric(result_train_train$target_home)-1, 
        nrounds=p_maxrounds,
        max_depth=p_max_depth,
        subsample=p_subsample,
        verbose=0,
        objective="binary:logistic", 
        eval_metric="auc")

      # result_train_validate$score_home <- predict(rf_model_for_validation, result_train_validate[,c(predictors),with=F], ntree=100)
      result_train_validate$score_home <- predict(xgb_model_home_for_validation, data.matrix(result_train_validate[,c(predictors),with=F]))

      pred_home <- result_train_validate[order(customer_id,-score_home),][,head(.SD,1),by=.(customer_id)]

      pred_home$error <- computeDist(pred_home$pos_atm_orig_lat, pred_home$pos_atm_orig_lon, pred_home$home_orig_lat, pred_home$home_orig_lon)
      xgb_home_param_sets$accuracy[i] <- sum(as.numeric(pred_home$target_home)-1) / nrow(pred_home)
  
    }
  }
}

optimal_xgb_home_params <- xgb_home_param_sets[which.max(xgb_home_param_sets$accuracy),]







xgb_work_param_sets <- data.frame(maxdepth=numeric(grid_search_size), nrounds=numeric(grid_search_size), subsample=numeric(grid_search_size), accuracy=numeric(grid_search_size))

i <- 0

for (i1 in maxdepth)
{
  for (i2 in nrounds)
  {
    for (i3 in subsample)
    {
      
      print(paste("maxdepth=",i1," nrounds=",i2," subsample=",i3,sep=""))
      i <- i +1
      
      # rf_model_for_validation <- randomForest(target_home~., data=result_train_train[,c(predictors,'target_home'),with=F], ntree=100)
      # glm_model_for_validation <- glm(target_home~., data=home_train_data[,c(predictors,'target_home'),with=F], family=binomial(logit))
      
      p_max_depth = i1
      p_maxrounds = i2
      p_subsample = i3
      
      xgb_work_param_sets$maxdepth[i] <- i1
      xgb_work_param_sets$nrounds[i] <- i2
      xgb_work_param_sets$subsample[i] <- i3

      xgb_model_work_for_validation <- xgboost(
        data=data.matrix(result_train_train_work[,c(predictors),with=F]), 
        label=as.numeric(result_train_train_work$target_work)-1,
        nrounds=p_maxrounds,
        max_depth=p_max_depth,
        subsample=p_subsample,
        verbose=0,
        objective="binary:logistic", 
        eval_metric="auc")
      
      # result_train_validate$score_home <- predict(rf_model_for_validation, result_train_validate[,c(predictors),with=F], ntree=100)
      result_train_validate_work$score_work <- predict(xgb_model_work_for_validation, data.matrix(result_train_validate_work[,c(predictors),with=F]))
      
      pred_work <- result_train_validate_work[order(customer_id,-score_work),][,head(.SD,1),by=.(customer_id)]
      
      pred_work$error <- computeDist(pred_work$pos_atm_orig_lat, pred_work$pos_atm_orig_lon, pred_work$home_orig_lat, pred_work$home_orig_lon)
      xgb_work_param_sets$accuracy[i] <- sum(as.numeric(pred_work$target_work)-1) / nrow(pred_work)
  
    }
  }
}

optimal_xgb_work_params <- xgb_work_param_sets[which.max(xgb_work_param_sets$accuracy),]









model_home <- glm(target_home~., data=result_train[,c(predictors,'target_home'),with=F], family=binomial(logit))
result_train$score_home <- predict(model_home, result_train[,c(predictors),with=F], type="response")
AUC::auc(AUC::roc(result_train$score_home, result_train$target_home))
AUC::auc(AUC::roc(result$score_home, result$target_home))



data.matrix(result_train[,c(predictors),with=F])




# SCORE TEST DATSET


  # Fit RandomForest for Home on Train
  xgb.model_home <- xgboost(
    data=data.matrix(result_train[,c(predictors),with=F]), label=as.numeric(result_train$target_home)-1,
    max_depth=10,
    subsample=1.0,
    nrounds=1000, 
    objective="binary:logistic", 
    eval_metric="auc")
  
    catboost_train_home <- catboost.load_pool(data=result_train[,predictors,with=F], 
                                               label = as.numeric(result_train$target_home)-1,
                                               feature_names = as.list(predictors),
                                               cat_features=c(62),
                                               thread_count = -1)
    catboost_test_home <- catboost.load_pool(data=result_test[,predictors,with=F], 
                                         label = as.numeric(result_test$target_home)-1,
                                         feature_names = as.list(predictors),
                                         cat_features=c(62),
                                         thread_count = -1)
    
    fit_params <- list(iterations = 1000,
                       thread_count = 10,
                       eval_metric = 'Accuracy',
                       border_count = 32,
                       depth = 10,
                       use_best_model = TRUE,
                       learning_rate = 0.03)
    
    catboost_model <- catboost.train(learn_pool = catboost_train_home, params = fit_params)
    
    result_test$score_home <- catboost.predict(catboost_model, catboost_test_home, prediction_type = "Probability")
    
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
  
  catboost_train_work <- catboost.load_pool(data=result_train[!is.na(work_lat) & !is.na(work_lon),predictors,with=F], 
                                       label = as.numeric(result_train$target_work[!is.na(result_train$work_lat) & !is.na(result_train$work_lon)])-1,
                                       feature_names = as.list(predictors),
                                       cat_features=c(62),
                                       thread_count = -1)
  catboost_test_work <- catboost.load_pool(data=result_test[,predictors,with=F], 
                                      label = NULL,
                                      feature_names = as.list(predictors),
                                      cat_features=c(62),
                                      thread_count = -1)
  
  
  
  catboost_model <- catboost.train(learn_pool=catboost_train_work, params=fit_params)
  
  result_test$score_work <- catboost.predict(catboost_model, catboost_test_work, prediction_type = "Probability")
  
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



