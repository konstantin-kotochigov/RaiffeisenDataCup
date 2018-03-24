require(data.table)
library(catboost)

result = fread("output/raiff_attrs.csv", sep=",", header=T)
  
# result <- result[is_additional_point==0,]

predictors <- colnames(result)[substring(colnames(result),1,3) == "top"]
predictors <- predictors[5:length(predictors)]
predictors_eps <- colnames(result)[substring(colnames(result),1,3) == "eps"]
predictors_cluster <- colnames(result)[substring(colnames(result),1,7) == "cluster"]
predictors_cell <- colnames(result)[substring(colnames(result),1,4) == "cell"]
predictors <- c(
  predictors, 
  predictors_eps, 
  predictors_cluster, 
  predictors_cell,  
  c("is_moscow","is_piter","is_other","center_dist","size_x","size_y","city_group",
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
                              "mcc",
                              "is_atm",
                              "cluster_mean_lat",
                              "cluster_mean_lon",
                              "is_additional_point","dist_to_real_transaction"
                              #"pos_atm_cell"
                             
))
predictors <- unique(predictors)



result$is_atm <- ifelse(result$is_atm == "Y", 1, 0)

# ToDO: delete
result$is_moscow <- factor(ifelse(result$top_city=='MOSCOW',1,0))
result$is_piter <- factor(ifelse(result$top_city=='SAINT PETERSBURG',1,0))
result$is_other <- factor(ifelse(!result$top_city %in% c('MOSCOW','SAINT PETERSBURG'),1,0))


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
  
  result_train_home$target_home <- factor(ifelse(result_train_home$home_dist <= 0.02, 1, 0))
  result_train_work$target_work <- factor(ifelse(result_train_work$work_dist <= 0.01, 1, 0))
  
  # Divide Home & Work datasets into train/validate by customers
  train_home_customers <- unique(result_train_home$customer_id)
  train_work_customers <- unique(result_train_work$customer_id)
  
  
  
  
  
  
  
  
  
  
  
  # Function to create Train/Test for parameter optimization
  createTrainTest <- function(p)
  {
    
    # Create train/test indexes
    
      train_train_home_customers = sample(train_home_customers, round(p*length(train_home_customers)), FALSE)
      train_train_work_customers = sample(train_work_customers, round(p*length(train_work_customers)), FALSE)
      
      train_validate_home_customers = setdiff(train_home_customers, train_train_home_customers)
      train_validate_work_customers = setdiff(train_work_customers, train_train_work_customers)
    
    
    # Create datasets
    
      train_home = result_train_home[result_train_home$customer_id %in% train_train_home_customers,]
      validate_home = result_train_home[result_train_home$customer_id %in%train_validate_home_customers,]
      
      train_work = result_train_work[result_train_work$customer_id %in% train_train_work_customers,]
      validate_work = result_train_work[result_train_work$customer_id %in%train_validate_work_customers,]

    
    # Print results    

      #print("Stats for datasets...")
      # print(paste(
      #   "Train=", length(unique(result_train$customer_id)),
      #   " Test=", length(unique(result_test$customer_id)),
      #   " Train(Home)=", length(unique(result_train_home$customer_id)),
      #   " Train(Work)=", length(unique(result_train_work$customer_id)),
      #   " Train-Train(Home)", length(unique(train_home$customer_id)),
      #   " Train-Validate(Home)", length(unique(validate_home$customer_id)),
      #   " Train-Train(Work)=", length(unique(train_work$customer_id)),
      #   " Train-Validate(Work)=",length(unique(validate_work$customer_id)),sep=""))
      
    return (
      list(
        train_home=train_home, 
        train_work=train_work, 
        validate_home=validate_home, 
        validate_work=validate_work)
    )
  
  }
  
  datasets <- createTrainTest(0.75)
  result_train_train_home <- datasets$train_home
  result_train_train_work <- datasets$train_work
  result_train_validate_home <- datasets$validate_home
  result_train_validate_work <- datasets$validate_work
  

  
  
  factors <- get.factor.columns(data.frame(result[,predictors,with=FALSE]))
  numerics <- get.numeric.columns(data.frame(result[,predictors,with=FALSE]))

  # numeric_auc_importance_home <- get.auc.importance(data.frame(result_train_home[,predictors,with=F]), result_train_home$target_home)
  # numeric_auc_importance_work <- get.auc.importance(data.frame(result_train_work[,predictors,with=F]), result_train_work$target_work)
  # 
  # top_100_home_numeric_predictors <- numeric_auc_importance_home$var[1:100]
  # top_100_work_numeric_predictors <- numeric_auc_importance_work$var[1:100]
  # 
  # top_50_home_numeric_predictors <- numeric_auc_importance_home$var[1:50]
  # top_50_work_numeric_predictors <- numeric_auc_importance_work$var[1:50]
  # 
  # top_150_home_numeric_predictors <- numeric_auc_importance_home$var[1:150]
  # top_150_work_numeric_predictors <- numeric_auc_importance_work$var[1:150]
  # 
  # top_100_home_predictors <- c(factors,top_100_home_numeric_predictors)
  # top_100_work_predictors <- c(factors,top_100_work_numeric_predictors)




  # Filter zero variation predictors
  # columns_info_home <- classify.columns(data.frame(result_train_home), y.name="target_home")
  # columns_info_work <- classify.columns(data.frame(result_train_work), y.name="target_work")
  # no_variance_home_cols <- columns_info_home$var[columns_info_home$lev == 1]
  # no_variance_work_cols <- columns_info_work$var[columns_info_work$lev == 1]
  # predictors <- setdiff(predictors, c(no_variance_home_cols,no_variance_work_cols))
  # 
  # predictors <- unique(c(factors,top_50_home_predictors, top_50_work_predictors))
  

  

  




# Define catboost datasets
  
  require(catboost)
  
  mcc_attribute_num <- which(predictors=="mcc")
  city_attribute_num <- which(predictors %in% c("city_group","city_group1","city_group2"))
  # cell_attribute_num <- which(predictors=="pos_atm_cell")
  categorical_attrs = c(mcc_attribute_num,city_attribute_num)

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









