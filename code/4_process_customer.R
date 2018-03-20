

# Function to cluster dataset
cluster_customer_transactions <- function(df)
{
  
  num_points = nrow(unique(df))
  
  if (num_points == 1)
  {
    df$cluster_id = 1
    clusters_info = data.frame(size = 1, max_diss = 0, av_diss = 0, diameter=0, separation = 0)
  }
  
  if (num_points == 2) 
  {
    
    df$cluster_id = 1
    
    # A cheat to 1 get class clusterization and cluster details
    p_clusters <- pam(rbind(df,df), k=1)
    clusters_info <- as.data.frame(p_clusters$clusinfo)
    clusters_info$size = clusters_info$size / 2
    clusters_info$av_diss = clusters_info$av_diss * 2
    
  }
  
  if (num_points >= 3)
  {
    
    # Clusterize (all transactions and most important categories)
    p_clusters <- pamk(df, krange=2:min(10, num_points-1), critout=F)
    clusters_info <- p_clusters$pamobject$clusinfo
    df$cluster_id <- p_clusters$pamobject$clustering
    
  }
  
  clusters_center <- df[order(cluster_id),.(cluster_mean_lat=mean(pos_atm_lat,na.rm=T),cluster_mean_lon=mean(pos_atm_lon,na.rm=T)),by=.(cluster_id)]
  clusters_info <- cbind(clusters_center, clusters_info)
  colnames(clusters_info)[colnames(clusters_info) %in% c('size','max_diss','av_diss','diameter','separation')] <- c('cluster_size','cluster_max_diss','cluster_av_diss','cluster_diameter','cluster_separation')
  
  # Rank current POS cluster by different parameters
  clusters_info$cluster_rank_by_size <- order(-clusters_info$cluster_size)
  clusters_info$cluster_rank_by_max_diss <- order(-clusters_info$cluster_max_diss)
  clusters_info$cluster_rank_by_av_diss <- order(-clusters_info$cluster_av_diss)
  clusters_info$cluster_rank_by_diameter <- order(-clusters_info$cluster_diameter)
  clusters_info$cluster_rank_by_separation <- order(-clusters_info$cluster_separation)
  
  # Ratio of current POS cluster to max POS clusters
  clusters_info$cluster_max_diss_rate_max <- clusters_info$cluster_max_diss / (max(clusters_info$cluster_max_diss)+0.001)
  clusters_info$cluster_size_rate_max = clusters_info$cluster_size / (max(clusters_info$cluster_size)+0.001)
  clusters_info$cluster_av_diss_rate_max = clusters_info$cluster_av_diss / (max(clusters_info$cluster_av_diss)+0.001)
  clusters_info$cluster_diameter_rate_max = clusters_info$cluster_diameter / (max(clusters_info$cluster_diameter)+0.001)
  clusters_info$cluster_separation_rate_max = clusters_info$cluster_separation / (max(clusters_info$cluster_separation) + 0.001)
  
  clusters_info$cluster_max_diss_rate_avg <- clusters_info$cluster_max_diss / (mean(clusters_info$cluster_max_diss)+0.001)
  clusters_info$cluster_size_rate_avg = clusters_info$cluster_size / (mean(clusters_info$cluster_size)+0.001)
  clusters_info$cluster_av_diss_rate_avg = clusters_info$cluster_av_diss / (mean(clusters_info$cluster_av_diss)+0.001)
  clusters_info$cluster_diameter_rate_avg = clusters_info$cluster_diameter / (mean(clusters_info$cluster_diameter)+0.001)
  clusters_info$cluster_separation_rate_avg = clusters_info$cluster_separation / (mean(clusters_info$cluster_separation) + 0.001)
  
  clusters_info$cluster_max_diss_rate_min <- clusters_info$cluster_max_diss / (min(clusters_info$cluster_max_diss)+0.001)
  clusters_info$cluster_size_rate_min = clusters_info$cluster_size / (min(clusters_info$cluster_size)+0.001)
  clusters_info$cluster_av_diss_rate_min = clusters_info$cluster_av_diss / (min(clusters_info$cluster_av_diss)+0.001)
  clusters_info$cluster_diameter_rate_min = clusters_info$cluster_diameter / (min(clusters_info$cluster_diameter)+0.001)
  clusters_info$cluster_separation_rate_min = clusters_info$cluster_separation / (min(clusters_info$cluster_separation) + 0.001)
  
  # Ratio of current cluster to first cluster
  
  
  
  return(list(clusters_info=clusters_info, cluster_id=df$cluster_id))
  
}

# A function to calcluate distance-based attributes
# Input: customer transactions
# Output: same transactions enriched with new attributes
process_customer <- function(current_transactions)
{
  
  require(fpc)
  
  current_transactions$id <- as.numeric(row.names(current_transactions))
  
  # toDO: compute candidatePoints
  # toDO: current_trsansaction <- rbind(current_transcations, candidatePoints)
  
  # Compute distance matrix
  
  transactionsDistMatrix <- as.matrix(dist(current_transactions[,.(pos_atm_orig_lat, pos_atm_orig_lon),]))
  # ToDO: candidatesDistMatrix <- transactionsDistMatrix[,]
  # ToDO: d <- rbind(transactionsDistMatrix, candidatesDistMatrix)
  d <- transactionsDistMatrix

  # Calculate neighborhood attributes
  for (j in c("any","6011","5411","5814","5812","5499","5912","5541","4111","5691","5977","5921","5999","5331","5261","5661"))
  {
    for (z in current_transactions$id)
    {
      
          # Use transactions of particular category
        if (j == "any")
          type_transaction_ids <- setdiff(current_transactions$id, z)
        else
          type_transaction_ids <- setdiff(current_transactions$id[current_transactions$mcc==j], z)

        current_transactions[z,paste("eps_1_cnt_",j,sep="")] <- sum(d[z,type_transaction_ids] < 0.02)
        current_transactions[z,paste("eps_2_cnt_",j,sep="")] <- sum(d[z,type_transaction_ids] < 0.05)
        current_transactions[z,paste("eps_3_cnt_",j,sep="")] <- sum(d[z,type_transaction_ids] < 0.10)

        current_transactions[z,paste("eps_1_rate_",j,sep="")] <- current_transactions[z,paste("eps_1_cnt_",j,sep=""),with=FALSE] / ncol(d)
        current_transactions[z,paste("eps_2_rate_",j,sep="")] <- current_transactions[z,paste("eps_2_cnt_",j,sep=""),with=FALSE] / ncol(d)
        current_transactions[z,paste("eps_3_rate_",j,sep="")] <- current_transactions[z,paste("eps_3_cnt_",j,sep=""),with=FALSE] / ncol(d)

    }
  }
  
  
  # 
  for (i in c(1,3,5,7,9))
  {
    for (j in c("any","6011","5411","5814","5812","5499","5912","5541","4111","5691","5977","5921","5999","5331","5261","5661"))
    {
      # print(paste("processing ",i,"_",j,"\r",sep=""))
      for (z in current_transactions$id)
      {
        
        # Use transactions of particular category
        if (j == "any")
          type_transaction_ids <- setdiff(current_transactions$id, z)
        else
          type_transaction_ids <- setdiff(current_transactions$id[current_transactions$mcc==j], z)
        
        top_n_type <- type_transaction_ids[order(d[z,type_transaction_ids])][1:i]
        top_n_type_distance <- mean(d[z,top_n_type])
        top_n_type_distance_max <- max(d[z,top_n_type])
        
        if (j=="any")
        {
          closest_merchant_categories <- current_transactions$mcc[current_transactions$id %in% top_n_type]
          current_transactions[z,paste("top_",i,"_6011_rate",sep="")] <- sum(closest_merchant_categories=="6011") / i
          current_transactions[z,paste("top_",i,"_5411_rate",sep="")] <- sum(closest_merchant_categories=="5411") / i
          current_transactions[z,paste("top_",i,"_5814_rate",sep="")] <- sum(closest_merchant_categories=="5814") / i
          current_transactions[z,paste("top_",i,"_5812_rate",sep="")] <- sum(closest_merchant_categories=="5812") / i
          current_transactions[z,paste("top_",i,"_5499_rate",sep="")] <- sum(closest_merchant_categories=="5499") / i
          current_transactions[z,paste("top_",i,"_5912_rate",sep="")] <- sum(closest_merchant_categories=="5912") / i
          current_transactions[z,paste("top_",i,"_5541_rate",sep="")] <- sum(closest_merchant_categories=="5541") / i
          current_transactions[z,paste("top_",i,"_5691_rate",sep="")] <- sum(closest_merchant_categories=="5691") / i
          current_transactions[z,paste("top_",i,"_5977_rate",sep="")] <- sum(closest_merchant_categories=="5977") / i
          current_transactions[z,paste("top_",i,"_5921_rate",sep="")] <- sum(closest_merchant_categories=="5921") / i
        }
        
        if (is.na(top_n_type_distance)) top_n_type_distance <- 2
        if (is.na(top_n_type_distance_max)) top_n_type_distance_max <- 2
        
        attr_name <- paste("top_",i,"_",j,"_mean_distance",sep="")
        current_transactions[z,attr_name] <- top_n_type_distance

        attr_name <- paste("top_",i,"_",j,"_max_distance",sep="")
        current_transactions[z,attr_name] <- top_n_type_distance_max
        
      }
    }
  }
  
  # Compute distance to Home (meaningful values for Train only)
  current_transactions$home_dist <- sqrt((current_transactions$pos_atm_orig_lat - current_transactions$home_orig_lat)^2 + (current_transactions$pos_atm_orig_lon - current_transactions$home_orig_lon)^2)
  nearest_point <- which.min(current_transactions$home_dist)
  nearest_point <- ifelse(length(nearest_point)==0,0,nearest_point)
  current_transactions$nearest_dist <- min(current_transactions$home_dist)
  current_transactions$target_home <- ifelse(current_transactions$id == nearest_point, 1, 0)
  
  # Compute distance to Work (meaningful values for Train only)
  current_transactions$work_dist <- sqrt((current_transactions$pos_atm_orig_lat - current_transactions$work_lat)^2 + (current_transactions$pos_atm_orig_lon - current_transactions$work_lon)^2)
  work_nearest_point <- which.min(current_transactions$work_dist)
  work_nearest_point <- ifelse(length(work_nearest_point)==0,0,work_nearest_point)
  current_transactions$work_nearest_dist <- min(current_transactions$work_dist)
  current_transactions$target_work <- ifelse(current_transactions$id == work_nearest_point, 1, 0)
  
  # Compute distance from point to top city center
  current_transactions$center.dist <- sqrt((current_transactions$pos_atm_lat-current_transactions$top_city_lat)^2+(current_transactions$pos_atm_lon - current_transactions$top_city_lon)^2)
  
  
  # ToDO: Filter outlier transactions, set their cluster to 0
  
  
  # Clustering
  
  categories_to_cluster <- c('all')
  for (merchant_category in categories_to_cluster)
  {
    
    # Select datsets
    if (merchant_category == "all")
      transactions_to_cluster <- current_transactions[, c("pos_atm_lat","pos_atm_lon")]
    if (merchant_category != "all")
      transactions_to_cluster <- current_transactions[mcc==merchant_category, c("pos_atm_lat","pos_atm_lon")]
    
    # Set new attribute names
    clustering_attr_name = paste("cluster_",merchant_category, sep="")
    cluster_dist_attr_name = paste("cluster_dist_",merchant_category, sep="")
    cluster_cnt_attr_name = paste("cluster_cnt_",merchant_category, sep="")
    
    # When no transactions of specific category, set clustering data to dummy values
    if (nrow(transactions_to_cluster)==0)
    {
      current_transactions[,clustering_attr_name] <- 0
      
      # Compute Distance to current cluster center
      current_transactions[,cluster_dist_attr_name] = 2
      
      current_transactions[,cluster_cnt_attr_name] <- 0
      
    }
    else
    {
      
      # Cluster transactions of current MCC type
      result <- cluster_customer_transactions(transactions_to_cluster)
    
      # Get resulting cluster number
      current_transactions[,clustering_attr_name] <- result$cluster_id
      
      # Merge cluster data
      current_transactions <- merge(current_transactions, result$clusters_info, by.x=clustering_attr_name, by.y="cluster_id", all.x=T, all.y=F)
      
      # Compute distance to current cluster center
      current_transactions[,cluster_dist_attr_name] = computeDist(current_transactions$pos_atm_lat, current_transactions$pos_atm_lon, current_transactions$cluster_mean_lat, current_transactions$cluster_mean_lon)
      
      # Get number of clusters
      current_transactions[,cluster_cnt_attr_name] <- nrow(result$clusters_info)
      
    }
  }
  
  
  current_transactions
  
}