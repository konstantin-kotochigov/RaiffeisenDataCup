

custid="05ab4594e030673ef6c721b78ec27241"
center <- test[test$customer_id == custid, c("lat","lon","pop")]
x.supermarkets <- train.supermarkets[train.supermarkets$customer_id==custid,]
x.supermarkets <- test.supermarkets[test.supermarkets$customer_id==custid,]
bgMap = get_map(location=c(lat = center$lat, lon = center$lon), source = "google", zoom = 11, color="bw")

x.supermarkets$clus <- customer_trn_super$clus

ggmap(bgMap) + 
  geom_point(aes(x = pos_address_lon, y = pos_address_lat), data = x.supermarkets, color=x.supermarkets$clus, alpha = .75, size=2, color="yellow") + 
  # geom_text(aes(x = work_add_lon, y = work_add_lat, label="x"), data=x.weekend, vjust=1,hjust=1) +
  # geom_point(aes(x = pos_address_lon, y = pos_address_lat), data = x.cafes, alpha = .75, size=2, colour="red") +
  # geom_point(aes(x = home_add_lon, y = home_add_lat), data = x.supermarkets[1], alpha = 0.75, size=5, color="blue",pch=3) + 
  geom_point(aes(x = V2, y = V1), data = c, alpha = 0.75, size=10, color="black",pch=3)
# geom_point(aes(x = work_add_lon, y = work_add_lat), data = x[1,], alpha = 0.75, size=5, color="green",pch=3)







train.pos_aggregates <- train.df[,.(s=sum(amount),cnt=.N),by=.(pos_address_lat,pos_address_lon,mcc,home_add_lat,home_add_lon,work_add_lat,work_add_lon,customer_id)]
test.pos.aggregates <-   test.df[,.(s=sum(amount),cnt=.N),by=.(pos_address_lat,pos_address_lon,mcc,customer_id)]

train.supermarkets <- train.pos_aggregates[train.pos_aggregates$mcc %in% c(5411),]
train.cafes <- train.pos_aggregates[train.pos_aggregates$mcc %in% c(5812,5814),]

test.supermarkets <- test.pos.aggregates[test.pos.aggregates$mcc %in% c(5411),]
test.cafes <- test.pos.aggregates[test.pos.aggregates$mcc %in% c(5812,5814),]

# dbs <- dbscan(x.supermarkets[,c("pos_address_lat","pos_address_lon")], eps=0.02, minPts=3)
# x.supermarkets$clus = dbs$cluster
# x.supermarkets <- x.supermarkets[x.supermarkets$clus==0,]
# c <- x.supermarkets[,.(mean(pos_address_lat),mean(pos_address_lon)),]





n.train.customers <- length(train.customers)
n.test.customers <- length(test.customers)

train.result <- data.frame(customer_id = train.customers, supermarkets=numeric(length(train.customers)), score=numeric(length(train.customers)))
test.result <- data.frame(customer_id = test.customers, home_lat=numeric(n.test.customers), home_lon=numeric(n.test.customers),
                          work_lat=numeric(n.test.customers), work_lon=numeric(n.test.customers))
for (i in 1:length(train.customers))
{
  
  custid = train.customers[i]
  
  print(paste("Analyzing customer ",i," ",custid,sep=""))
  
  customer_trn_super <- train.supermarkets[train.supermarkets$customer_id == custid,]
  train.result$customer_id[i] <- custid
  train.result$supermarkets[i] <- nrow(customer_trn_super)
  
  # If too few supermarkets
  if (nrow(customer_trn_super) < 3)
  {
    customer_trn_super$clus = 1
    best_cluster = 1
  }
  else
  {
    
    # Clusterize
    p_clusters <- pamk(customer_trn_super[,c("pos_address_lat","pos_address_lon")], krange=2:min(10, nrow(customer_trn_super)-1), critout=T)
    customer_trn_super$clus = p_clusters$pamobject$clustering
    
    # With Most purchases
    # best_cluster = which.max(customer_trn_super[,.(s=sum(s)),by=.(clus)]$s)
    
    
    # The Most Dense
    # p_clusters_info <- p_clusters$pamobject$clusinfo
    best_cluster <- which.max(p_clusters_info[,"size"])
    
  }
  
  
  customer_trn_super <- customer_trn_super[customer_trn_super$clus == best_cluster,]
  c <- customer_trn_super[,.(mean(pos_address_lat),mean(pos_address_lon)),]
  
  train.result$score[i] <- computeHomeAccuracy(c$V1,c$V2,customer_trn_super$home_add_lat[1], customer_trn_super$home_add_lon[1])
  
}



for (i in 1:length(test.customers))
{
  
  custid = test.customers[i]
  
  print(paste("Analyzing customer ",i," ",custid,sep=""))
  
  customer_trn_super <- test.supermarkets[test.supermarkets$customer_id == custid,]
  test.result$customer_id[i] <- custid
  test.result$supermarkets[i] <- nrow(customer_trn_super)
  
  # If too few supermarkets
  if (nrow(customer_trn_super) < 3)
  {
    customer_trn_super$clus = 1
    best_cluster = 1
  }
  else
  {
    
    # Clusterize
    p_clusters <- pamk(customer_trn_super[,c("pos_address_lat","pos_address_lon")], krange=2:min(10, nrow(customer_trn_super)-1), critout=T)
    customer_trn_super$clus = p_clusters$pamobject$clustering
    
    # With Most purchases
    # best_cluster = which.max(customer_trn_super[,.(s=sum(s)),by=.(clus)]$s)
    
    
    # The Most Dense
    # p_clusters_info <- p_clusters$pamobject$clusinfo
    best_cluster <- which.max(p_clusters_info[,"size"])
    
  }
  
  
  customer_trn_super <- customer_trn_super[customer_trn_super$clus == best_cluster,]
  c <- customer_trn_super[,.(mean(pos_address_lat),mean(pos_address_lon)),]
  
  test.result$home_lat[i] <- c$V1
  test.result$home_lon[i] <- c$V2
  test.result$work_lat[i] <- c$V1
  test.result$work_lon[i] <- c$V2
  
}

s <- read.csv("/Users/Konstantin/scoring.csv",sep=",")
s <- merge(s, test.result[,c("customer_id","home_lat","home_lon")],
           by.x="X_ID_",by.y="customer_id")
s$home_lat_ <- ifelse(is.na(s$home_lat), s$X_HOME_LAT_, s$home_lat)
s$home_lon_ <- ifelse(is.na(s$home_lon), s$X_HOME_LON, s$home_lon)

write.table(
  s[,c("X_ID_","X_WORK_LAT_","X_WORK_LON_","home_lat_","home_lon_")],
  "C://Users//KOnstantin//scoring1.csv",
  sep=",",
  quote=FALSE,
  col.names=TRUE,
  row.names=FALSE)

View(s)


ggmap(bgMap) + 
  geom_point(aes(x = pos_address_lon, y = pos_address_lat), data = x.supermarkets, alpha = .75, size=2, color="yellow") + 
  geom_point(aes(x = V2, y = V1), data=c, alpha = .75, size=2, colour="red") +
  geom_point(aes(x = home_add_lon, y = home_add_lat), data = x[1,], alpha = 0.75, size=5, color="blue",pch=3)

