# library(ggmap)
library(data.table)

showMap <- function(custid)
{
  require(ggmap)
  
  x <- raw.df[raw.df$customer_id == custid,,]
  area <- areaCoordinates[customer_id==custid,]
  
  bgMap = get_map(location=c(area$center_lon, lon = area$center_lat), source = "google", zoom = area$zoom, color="bw")
  ggmap(bgMap) + geom_point(aes(x = pos_atm_lon, y = pos_atm_lat), data = x, alpha = .75, size=3, color="red") + 
    geom_point(aes(x = home_lon, y = home_lat), data = x, alpha = .75, size=3, color="blue") + 
    geom_point(aes(x = work_lon, y = work_lat), data = x, alpha = .75, size=3, color="green")
}

showMap <- function(custid)
{
  require(ggmap)
  
  x <- raw.df[raw.df$customer_id == custid,,]
  center = data.frame(x=top_cluster_center[2], y=top_cluster_center[1])
  
  bgMap = get_map(location=c(mean(x$pos_atm_lon,na.rm=T), lon = mean(x$pos_atm_lat,na.rm=T)), source = "google", zoom = 9, color="bw")
  ggmap(bgMap) + geom_point(aes(x = pos_atm_lon, y = pos_atm_lat), data = x, alpha = .75, size=3, color="red") + 
    geom_point(aes(x = home_lon, y = home_lat), data = x[1,], alpha = .75, size=3, color="blue") + 
    geom_point(aes(x = work_lon, y = work_lat), data = x[1,], alpha = .75, size=3, color="green") + 
    geom_point(aes(x = center$x, y = center$y), data = center, alpha = .75, size=3, color="yellow")
}

# Load data

  if (Sys.info()['sysname']=="Windows") setwd("/Users/Konstantin/Downloads/")
  raw.train <- fread("train_set.csv")
  raw.test <- fread("test_set.csv")


# Load cities reference

  # cities <- fread("worldcitiespop.txt")[,c("Country","City","Latitude","Longitude","Population")][,2:5]
  # colnames(cities) <- c("city","city_lat","city_lon","city_pop")
  # cities <- cities[!is.na(cities$city_pop),]
  # cities <- cities[cities$city_pop > 1000,]
  # cities$city <- toupper(cities$city)
  
  # Delete ambiguous cities
  # cities <- cities[!duplicated(cities$city),]


# Data Quality

  raw.test$home_add_lat <- 0.0
  raw.test$home_add_lon <- 0.0
  raw.test$work_add_lat <- 0.0
  raw.test$work_add_lon <- 0.0
  
  raw.test$mcc <- gsub(",","",raw.test$mcc)
  
  raw.train$df <- "train"
  raw.test$df <- "test"
  
  colnames(raw.train)[colnames(raw.train) %in% c("pos_adress_lat","pos_adress_lon")] <- c("pos_lat","pos_lon")
  colnames(raw.test)[colnames(raw.test) %in% c("pos_address_lat","pos_address_lon")] <- c("pos_lat","pos_lon")
  
  
  # Next transofmrations for both datasets
  raw.df <- rbind(raw.train, raw.test)
  
  
  colnames(raw.df)[colnames(raw.df) %in% c("atm_address_lat","atm_address_lon")] <- c("atm_lat","atm_lon")
  colnames(raw.df)[colnames(raw.df) %in% c("home_add_lat","home_add_lon")] <- c("home_lat","home_lon")
  colnames(raw.df)[colnames(raw.df) %in% c("work_add_lat","work_add_lon")] <- c("work_lat","work_lon")
  
  
  raw.df$city <- toupper(raw.df$city)
  raw.df$city[raw.df$city=="MOSKVA"] <- "MOSCOW"
  raw.df$city[raw.df$city=="SANKT-PETERBU"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="ST-PETERSBURG"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="ST PETERSBURG"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="ST PETERBURG"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="ST-PETERBURG"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="ST.PETERSBURG"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="SANKT-PETERSB"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="Moskva"] <- "MOSCOW"
  raw.df$city[raw.df$city=="St Petersburg"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="SAINT PETERSB"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="SAINT-PETERSB"] <- "SAINT PETERSBURG"
  raw.df$city[raw.df$city=="NOVOROSIYSK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="NOVOROSSIIYSK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="MOSKOW"] <- "MOSCOW"
  raw.df$city[raw.df$city=="NOVOROSSIISK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="N.NOVGOROD"] <- "NIZHNIY NOVGOROD"
  raw.df$city[raw.df$city=="NIZHNIY NOVGO"] <- "NIZHNIY NOVGOROD"
  raw.df$city[raw.df$city=="G MOSKVA"] <- "MOSCOW"
  raw.df$city[raw.df$city=="G. MOSKVA"] <- "MOSCOW"
  raw.df$city[raw.df$city=="VORONEJ"] <- "VORONEZH"
  raw.df$city[raw.df$city=="SOLNECHNII"] <- "SOLNECHNYY"
  raw.df$city[raw.df$city=="EKATERINBURG"] <- "YEKATERINBURG"
  raw.df$city[raw.df$city=="NVSIBR"] <- "NOVOSIBIRSK"
  raw.df$city[raw.df$city=="VLADIMIRSKAYA"] <- "VLADIMIR"
  raw.df$city[raw.df$city=="MARINO"] <- "MOSCOW"
  raw.df$city[raw.df$city=="AKSAI"] <- "AKSAY"
  
  # cities$city_lat[cities$city=="TULA"] <- 54.193033
  # cities$city_lon[cities$city=="TULA"] <- 37.617752
  # cities$city_lat[cities$city=="DZERZHINSK"] <- 56.2440992 
  # cities$city_lon[cities$city=="DZERZHINSK"] <- 43.4351804
  # cities$city_lat[cities$city=="KIROV"] <- 58.5966500
  # cities$city_lon[cities$city=="KIROV"] <- 49.660070
  # cities$city_lat[cities$city=="PUSHKINO"] <- 56.007524
  # cities$city_lon[cities$city=="PUSHKINO"] <- 37.848574
  # cities$city_lat[cities$city=="KOMMUNAR"] <- 59.621609
  # cities$city_lon[cities$city=="KOMMUNAR"] <- 30.393483
  # cities$city_lat[cities$city=="VLADIMIR"] <- 56.1365500
  # cities$city_lon[cities$city=="VLADIMIR"] <- 40.3965800
  # cities$city_lat[cities$city=="IVANTEEVKA"] <- 55.9711100
  # cities$city_lon[cities$city=="IVANTEEVKA"] <- 37.9208300
  # cities$city_lat[cities$city=="REVDA"] <- 56.798724
  # cities$city_lon[cities$city=="REVDA"] <- 59.907083
  # cities$city_lat[cities$city=="ISTOK"] <- 56.8519000
  # cities$city_lon[cities$city=="ISTOK"] <- 60.6122000
  # cities$city_lat[cities$city=="GATCHINA"] <- 59.5763900
  # cities$city_lon[cities$city=="GATCHINA"] <- 30.1283300
  # cities$city_lat[cities$city=="ROSTOV"] <- 47.222543
  # cities$city_lon[cities$city=="ROSTOV"] <- 39.718732
  # cities$city_lat[cities$city=="ARMAVIR"] <- 44.9892000
  # cities$city_lon[cities$city=="ARMAVIR"] <- 41.1234000
  # cities$city_lat[cities$city=="BOR"] <- 56.3580800
  # cities$city_lon[cities$city=="BOR"] <- 44.0747700
  # cities$city_lat[cities$city=="PAVLOVO"] <- 55.9686000
  # cities$city_lon[cities$city=="PAVLOVO"] <- 43.0912000
  # cities$city_lat[cities$city=="SVETLOGORSK"] <- 66.9376200
  # cities$city_lon[cities$city=="SVETLOGORSK"] <- 88.3526500
  # cities$city_lat[cities$city=="YALTA"] <- 44.495273
  # cities$city_lon[cities$city=="YALTA"] <- 34.166353
  # cities$city_lat[cities$city=="PROLETARSKIY"] <- 55.02543
  # cities$city_lon[cities$city=="PROLETARSKIY"] <- 37.38640
  # cities$city_lat[cities$city=="RODNIKI"] <- 55.65583 
  # cities$city_lon[cities$city=="RODNIKI"] <- 38.05679
  # cities$city_lat[cities$city=="SOVETSK"] <- 55.65583
  
  # cities <- rbind(cities, data.frame(city="OKTYABRSKIY", city_lat=55.60851, city_lon=37.97634, city_pop=NA))
  # cities <- rbind(cities, data.frame(city="AKSAY", city_lat=47.2583800, city_lon=39.8667500, city_pop=NA))
  
  # cities <- cities[!cities$city %in% c('ARZAMAS','BELYY YAR','BOGANDINSKIY','CHEBARKUL','CHEBOKSARY','GORODISHCHE',
  #                           'KAMYSHLOV','KRASNODAR','LACOMBE','LUKOYANOV','MARINO','NOVI SAD','OSA',
  #                           'ROSSOSH','SOLNECHNYY','SOVETSK','TOBOLSK','TROITSK','VITIM','VOLZHSKIY','VURNARY','YUBILEYNYY'),]
  
  
  
  raw.df$pos_atm_lat = ifelse(is.na(raw.df$pos_lat), raw.df$atm_lat, raw.df$pos_lat)
  raw.df$pos_atm_lon = ifelse(is.na(raw.df$pos_lon), raw.df$atm_lon, raw.df$pos_lon)
  
  



  # View(raw.df[,.(cnt=.N),by=city][order(-cnt),,][1:100,,])



# Lookup City Center
  
  
  
  
  # Lookup city center and population from Reference
  # raw.df <- merge(raw.df, cities[,c("city","city_lat","city_lon","city_pop")], by.x="city", by.y="city", all.x=TRUE, all.y=FALSE)

  # Lookup city center from Transactions
  cities.center.manual <- 
    raw.df[,
           .(
             city_lat=mean(pos_atm_lat, na.rm=T), 
             city_lon=mean(pos_atm_lon, na.rm=T),
             range_x = max(pos_atm_lon, na.rm=T) - min(pos_atm_lon, na.rm=T),
             range_y = max(pos_atm_lat, na.rm=T) - min(pos_atm_lat, na.rm=T)), 
           by=.(customer_id,city)]
  raw.df <- merge(raw.df, cities.center.manual, by.x=c("customer_id","city"), by.y=c("customer_id","city"), all.x=T, all.y=F)
  
  # First use reference data then manual data
  # raw.df$city_lat = ifelse(is.na(raw.df$city_lat), raw.df$center.manual.lat, raw.df$city_lat)
  # raw.df$city_lon = ifelse(is.na(raw.df$city_lon), raw.df$center.manual.lon, raw.df$city_lon)
  # raw.df[,c("center.manual.lat","center.manual.lon")] <- NULL
  
  
  # Check how to filter outliers
  # n <- length(unique(raw.df$customer_id))
  # result <- data.frame(customer_id=character(n), dist=numeric(n), correct_50km=numeric(n))
  # library(fpc)
  # for (i in 1:1000)
  # {
  # 
  #   custid = unique(raw.df$customer_id)[i]
  #   print(i)
  #   current_transactions = raw.df[raw.df$customer_id == custid & !is.na(raw.df$pos_atm_lat) & !is.na(raw.df$pos_atm_lon),]
  #   p_clusters <- pamk(current_transactions[,c('pos_atm_lat','pos_atm_lon')], krange=2:min(nrow(current_transactions)-1,15))
  #   current_transactions$clus <- p_clusters$pamobject$clustering
  #   top_cluster <- which.max(p_clusters$pamobject$clusinfo[,1])
  #   top_cluster_center <- c(lat=mean(current_transactions$pos_atm_lat[current_transactions$clus == top_cluster]), lon=mean(current_transactions$pos_atm_lon[current_transactions$clus == top_cluster]))
  # 
  #   if (!is.na(max(current_transactions$home_lat, na.rm=T)))
  #   {
  #   result$dist[i] <- gpsDist(top_cluster_center[1], top_cluster_center[2], current_transactions$home_lat[1],current_transactions$home_lon[1])
  #   # result$dist[i] <- computeDist(top_cluster_center[1], top_cluster_center[2], current_transactions$home_lat[1],current_transactions$home_lon[1])
  #   result$correct_50km[i] <- ifelse(gpsDist(top_cluster_center[1], top_cluster_center[2], current_transactions$home_lat[1],current_transactions$home_lon[1]) > 100, 1, 0)
  #   }
  # }
  # 
  # ggplot() +
  #   geom_point(aes(x=pos_atm_lon, y=pos_atm_lat), data=current_transactions[,c('pos_atm_lat','pos_atm_lon')], color=current_transactions$clus) +
  #   geom_point(aes(x=home_lon, y=home_lat), data=current_transactions, size= 10, pch=3)
  # 
  # 
  
  # Choose top (the most frequent) city for each customer
  
  city.freq <- raw.df[!is.na(raw.df$pos_atm_lat) & !is.na(pos_atm_lon),.(n=.N),by=.(customer_id,city,city_lat,city_lon)]
  city.freq <- city.freq[order(customer_id,-n),,]
  customer.top.city <- city.freq[,.(top_city=head(city,1), top_city_lat=head(city_lat,1), top_city_lon=head(city_lon,1)),by=.(customer_id)]
  
  raw.df <- merge(raw.df, customer.top.city, by.x="customer_id", by.y="customer_id", all.x=TRUE, all.y=FALSE)
  
  
  
  # raw.df$city_lat[raw.df$city=="TULA"] <- 51.1848
  # raw.df$city_lon[raw.df$city=="TULA"] <- 37.6274
  # raw.df$city_lat[raw.df$city=="DZERZHINSK"] <- 43.4351804
  # raw.df$city_lon[raw.df$city=="DZERZHINSK"] <- 56.2440992
  # raw.df$city_lat[raw.df$city=="KIROV"] <- 58.5966500
  # raw.df$city_lon[raw.df$city=="KIROV"] <- 49.660070
  # raw.df$city_lat[raw.df$city=="PUSHKINO"] <- 56.007524
  # raw.df$city_lon[raw.df$city=="PUSHKINO"] <- 37.848574
  # raw.df$city_lat[raw.df$city=="KOMMUNAR"] <- 59.621609
  # raw.df$city_lon[raw.df$city=="KOMMUNAR"] <- 30.393483
  # raw.df$city_lat[raw.df$city=="VLADIMIR"] <- 56.1365500
  # raw.df$city_lon[raw.df$city=="VLADIMIR"] <- 40.3965800
  # raw.df$city_lat[raw.df$city=="IVANTEEVKA"] <- 55.9711100
  # raw.df$city_lon[raw.df$city=="IVANTEEVKA"] <- 37.9208300
  # raw.df$city_lat[raw.df$city=="REVDA"] <- 56.798724
  # raw.df$city_lon[raw.df$city=="REVDA"] <- 59.907083
  # raw.df$city_lat[raw.df$city=="ISTOK"] <- 56.8519000
  # raw.df$city_lon[raw.df$city=="ISTOK"] <- 60.6122000
  # raw.df$city_lat[raw.df$city=="GATCHINA"] <- 59.5763900
  # raw.df$city_lon[raw.df$city=="GATCHINA"] <- 30.1283300
  # raw.df$city_lat[raw.df$city=="ROSTOV"] <- 47.222543
  # raw.df$city_lon[raw.df$city=="ROSTOV"] <- 39.718732
  # raw.df$city_lat[raw.df$city=="ARMAVIR"] <- 44.9892000
  # raw.df$city_lon[raw.df$city=="ARMAVIR"] <- 41.1234000
  
  

  




if (Sys.info()['sysname']=="Windows")
  mcc <- read.table("mcc.csv",sep=",", header=T)
if (Sys.info()['sysname']!="Windows")
  mcc <- read.table("mcc.csv",sep=",", header=T, fileEncoding="latin1")

  
mcc <- mcc[-c(221),]

raw.df <- merge(raw.df, mcc[,c("code","category_eng")], by.x="mcc", by.y="code", all.x=T, all.y=F)

raw.df$is_atm <- ifelse(!is.na(raw.df$atm_lat) & !is.na(raw.df$atm_lon), "Y", "N")


raw.df$transaction_date[raw.df$transaction_date==""] <- "1900-01-01"

raw.df$dow <- weekdays(as.Date(raw.df$transaction_date))

# POS level aggregates
raw.df <- raw.df[,
                 c(
                   'customer_id',
                   'country',
                   'city',
                   'mcc',
                   'transaction_date',
                   'amount',
                   'currency',
                   'pos_atm_lat',
                   'pos_atm_lon',
                   'is_atm',
                   'category_eng',
                   'dow',
                   'city_lat',
                   'city_lon',
                 
                   'home_lat',
                   'home_lon',
                   'work_lat',
                   'work_lon',
                   'df',
                   'top_city',
                   'top_city_lat',
                   'top_city_lon')]

  raw.df <-raw.df[order(df, customer_id, city),]
  
  
  
  
  
  gpsDist <- function(lat1,lon1,lat2,lon2)
  {
    acos(
      sin(lat1*pi/180)*sin(lat2*pi/180) + 
        cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180)
    ) * 6371
  }
  
  raw.df$top_city_center_dist <- gpsDist(raw.df$pos_atm_lat, raw.df$pos_atm_lon, raw.df$top_city_lat, raw.df$top_city_lon)
  
  # check <- setdiff(unique(shit.df$customer_id), unique(raw.df$customer_id))
  
  # check <- shit.df[shit.df$customer_id %in% check,]
  # check.cities <- as.data.frame(table(check$city))
  # check.cities[order(-check.cities$Freq),]

# write.csv(raw.df, "/Users/Konstantin/work/raw_df",sep=";",col.names=TRUE,row.names=FALSE)


