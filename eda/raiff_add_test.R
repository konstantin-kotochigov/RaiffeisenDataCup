to_add <- setdiff(unique(raw.df$customer_id[raw.df$df=="test"]), unique(result_test$customer_id))


n <- length(to_add)
library(fpc)
result_add <- data.frame(customer_id=character(n), home_lat=numeric(n), home_lon=numeric(n), work_lat=numeric(n), work_lon=numeric(n), stringsAsFactors = FALSE)
for (i in 1:n)
{
  print(i)
  custid = to_add[i]
  
  current_transactions = raw.df[raw.df$customer_id == custid,]
  current_transactions = current_transactions[!is.na(pos_atm_lat) & !is.na(pos_atm_lon),]
  
  if (sum(current_transactions$top_city_center_dist < 50) != 0)
    current_transactions = current_transactions[top_city_center_dist < 50,]
  
  current_transactions_5411_5499 = current_transactions[current_transactions$mcc %in% c("5411","5499")]
  # current_transactions_5411 = current_transactions[current_transactions$mcc == "5411"]
  # current_transactions_5411 = current_transactions[current_transactions$mcc == "5411"]
  
  # If no supermarkets use whole dataset
  if (nrow(current_transactions_5411_5499) != 0)
    current_transactions = current_transactions_5411_5499
  
  # Aggreagte data in each POS
  current_transactions <- current_transactions[,.N,by=.(customer_id,pos_atm_lat, pos_atm_lon)]
  
  if (nrow(current_transactions) <= 2)
  {
    current_transactions$clus = 1
    best_cluster = 1
  }
  
  if (nrow(current_transactions)>2)
  {
    p_clusters <- pamk(current_transactions[,c("pos_atm_lat","pos_atm_lon")], krange=2:min(10, nrow(current_transactions)-1), critout=F)
    # p$pamobject$clusinfo
    current_transactions$clus = p_clusters$pamobject$clustering
      p_clusters_info <- p_clusters$pamobject$clusinfo
    best_cluster = which.max(p_clusters_info[,c("size")])
  }
  
  center = current_transactions[,.(cluster_mean_lat=mean(pos_atm_lat), cluster_mean_lon=mean(pos_atm_lon)),by=.(clus)][best_cluster]
  
  result_add$customer_id[i] <- custid
  result_add$work_lat[i] <- center$cluster_mean_lat
  result_add$work_lon[i] <- center$cluster_mean_lon
  result_add$home_lat[i] <- center$cluster_mean_lat
  result_add$home_lon[i] <- center$cluster_mean_lon
  
}
