# A function to calcluate distance-based attributes
# Input: customer transactions
# Output: same transactions enriched with new attributes

process_customer <- function(current_transactions)
{
  
  current_transactions$id <- as.numeric(row.names(current_transactions))
  
  # Compute distance matrix
  
  d <- as.matrix(dist(current_transactions[,.(pos_atm_lat, pos_atm_lon),]))
  
  # 
  for (i in c(1,3,5,7,9))
  {
    for (j in c("any","6011","5411","5814","5812","5499"))
    {
      # print(paste("processing ",i,"_",j,"\r",sep=""))
      for (z in current_transactions$id)
      {
        
        if (j == "any")
          type_transaction_ids <- setdiff(current_transactions$id, z)
        else
          type_transaction_ids <- setdiff(current_transactions$id[current_transactions$mcc==j], z)
        
        top_n_type <- type_transaction_ids[order(d[z,type_transaction_ids])][1:i]
        top_n_type_distance <- mean(d[z,top_n_type])
        
        if (is.na(top_n_type_distance))
          top_n_type_distance <- 2
        
        attr_name <- paste("top_",i,"_",j,"_mean_distance",sep="")
        current_transactions[z,attr_name] <- top_n_type_distance
        
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
  current_transactions$work_dist <- sqrt((current_transactions$pos_atm_lat - current_transactions$work_lat)^2 + (current_transactions$pos_atm_lon - current_transactions$work_lon)^2)
  work_nearest_point <- which.min(current_transactions$work_dist)
  work_nearest_point <- ifelse(length(work_nearest_point)==0,0,work_nearest_point)
  current_transactions$work_nearest_dist <- min(current_transactions$work_dist)
  current_transactions$target_work <- ifelse(current_transactions$id == work_nearest_point, 1, 0)
  
  # Compute distance from point to top city center
  current_transactions$center.dist <- sqrt((current_transactions$pos_atm_lat-current_transactions$top_city_lat)^2+(current_transactions$pos_atm_lon - current_transactions$top_city_lon)^2)
  
  current_transactions$is_moscow <- ifelse(current_transactions$top.city=='MOSCOW',1,0)
  current_transactions$is_piter <- ifelse(current_transactions$top.city=='SAINT PETERSBURG',1,0)
  current_transactions$is_other <- ifelse(!current_transactions$top.city %in% c('MOSCOW','SAINT PETERSBURG'),1,0)
  
  
  # Clustering
  
  num_points = nrow(unique(current_transactions[,c("pos_atm_lat","pos_atm_lon")]))
  
  if (num_points == 1)
  {
    current_transactions$clus = 1
    p_clusters_info = data.frame(size = 1, max_diss = 0, av_diss = 0, diameter=0, separation = 0)
  }
  
  if (num_points == 2) 
  {
    
    current_transactions$clus = 1
    
    # A cheat to 1 get class clusterization and cluster details
    p_clusters <- pam(rbind(current_transactions[,c("pos_atm_lat","pos_atm_lon")],current_transactions[,c("pos_atm_lat","pos_atm_lon")]), k=1)
    p_clusters_info <- as.data.frame(p_clusters$clusinfo)
    p_clusters_info$size = p_clusters_info$size / 2
    p_clusters_info$av_diss = p_clusters_info$av_diss * 2
    
  }
  
  if (num_points >= 3)
  {
    
    # Clusterize
    p_clusters <- pamk(current_transactions[,c("pos_atm_lat","pos_atm_lon")], krange=2:min(10, nrow(current_transactions)-1), critout=F)
    
    current_transactions$clus = p_clusters$pamobject$clustering
    p_clusters_info <- p_clusters$pamobject$clusinfo
    
  }
  
  clusters_info <- current_transactions[order(clus),.(cluster_mean_lat=mean(pos_atm_lat,na.rm=T),cluster_mean_lon=mean(pos_atm_lon,na.rm=T)),by=.(clus)]
  clusters_info <- cbind(clusters_info, p_clusters_info)
  
  clusters_info$cluster_rank_by_size <- clusters_info$clus[order(-clusters_info$size)]
  clusters_info$cluster_rank_by_max_diss <- clusters_info$clus[order(-clusters_info$max_diss)]
  clusters_info$cluster_rank_by_av_diss <- clusters_info$clus[order(-clusters_info$av_diss)]
  clusters_info$cluster_rank_by_diameter <- clusters_info$clus[order(-clusters_info$diameter)]
  clusters_info$cluster_rank_by_separation <- clusters_info$clus[order(-clusters_info$separation)]
  
  current_transactions <- merge(current_transactions,clusters_info, by.x="clus", by.y="clus")
  
  current_transactions$cluster_dist = computeDist(current_transactions$pos_atm_lat, current_transactions$pos_atm_lon, current_transactions$cluster_mean_lat, current_transactions$cluster_mean_lon)
  
  current_transactions$cluster_diss_to_max_diss = current_transactions$max_diss / max(clusters_info$max_diss)
  current_transactions$cluster_cnt <- nrow(clusters_info)
  
  current_transactions
  
}