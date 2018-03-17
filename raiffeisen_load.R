# Load data

  raw.train <- fread("data/train_set.csv")
  raw.test <- fread("data/test_set.csv")
  if (Sys.info()['sysname']=="Windows")
    mcc <- read.table("data/mcc.csv",sep=",", header=T)
  if (Sys.info()['sysname']!="Windows")
    mcc <- read.table("data/mcc.csv",sep=",", header=T, fileEncoding="latin1")


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
  
  
# Apply all further transformations to both datasets

  raw.df <- rbind(raw.train, raw.test)
  
  colnames(raw.df)[colnames(raw.df) %in% c("atm_address_lat","atm_address_lon")] <- c("atm_lat","atm_lon")
  colnames(raw.df)[colnames(raw.df) %in% c("home_add_lat","home_add_lon")] <- c("home_lat","home_lon")
  colnames(raw.df)[colnames(raw.df) %in% c("work_add_lat","work_add_lon")] <- c("work_lat","work_lon")
  
  
# City names

  raw.df$city <- toupper(raw.df$city)

  raw.df$city[raw.df$city %in% c("SANKT-PETERBU","ST-PETERSBURG","ST PETERSBURG","ST.-PETERSBUR","SANKT PETERBU","SANKT-PETERS","SANQT PETERBU","ST PETERSBUR","S.PETERBURG","SAINT-PETERBU","ST PETERBURG","ST-PETERBURG","ST.PETERSBURG","SANKT-PETERSB","St Petersburg","SAINT PETERSB","SAINT-PETERSB"] <- "SAINT PETERSBURG"
  
  raw.df$city[raw.df$city %in% c("MOSKVA","Moskva","G MOSKVA","G. MOSKVA","MOSKOW","MARINO"] <- "MOSCOW"

  raw.df$city[raw.df$city=="NOVOROSIYSK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="NOVOROSSIIYSK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="NOVOROSSIISK"] <- "NOVOROSSIYSK"
  raw.df$city[raw.df$city=="N.NOVGOROD"] <- "NIZHNIY NOVGOROD"
  raw.df$city[raw.df$city=="NIZHNIY NOVGO"] <- "NIZHNIY NOVGOROD"
  raw.df$city[raw.df$city=="VORONEJ"] <- "VORONEZH"
  raw.df$city[raw.df$city=="SOLNECHNII"] <- "SOLNECHNYY"
  raw.df$city[raw.df$city=="EKATERINBURG"] <- "YEKATERINBURG"
  raw.df$city[raw.df$city=="NVSIBR"] <- "NOVOSIBIRSK"
  raw.df$city[raw.df$city=="VLADIMIRSKAYA"] <- "VLADIMIR"
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

  raw.df$is_moscow <- factor(ifelse(raw.df$top_city=='MOSCOW',1,0))
  raw.df$is_piter <- factor(ifelse(raw.df$top_city=='SAINT PETERSBURG',1,0))
  raw.df$is_other <- factor(ifelse(!raw.df$top_city %in% c('MOSCOW','SAINT PETERSBURG'),1,0))
  
  
  
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
  
  

  






  
mcc <- mcc[-c(221),]

raw.df <- merge(raw.df, mcc[,c("code","category_eng")], by.x="mcc", by.y="code", all.x=T, all.y=F)

raw.df$is_atm <- ifelse(!is.na(raw.df$atm_lat) & !is.na(raw.df$atm_lon), "Y", "N")

# Convert transaction_date to Date
raw.df[raw.df$transaction_date!="","transaction_date1"] <- as.Date(raw.df$transaction_date[raw.df$transaction_date!=""])
raw.df[raw.df$transaction_date=="","transaction_date1"] <- NA
raw.df$transaction_date <- raw.df$transaction_date1

# Get Weekday (for 45 missing records => NA)
raw.df$dow <- ifelse(is.na(raw.df$transaction_date),"NA",weekdays(raw.df$transaction_date))
raw.df[raw.df$transaction_date=="","dow"] <- "NA"

# Get public holidays
raw.df$mondt <- substring(raw.df$transaction_date,6,10)
raw.df$is_holiday <- factor(ifelse(raw.df$mondt %in% c("01-01","01-02","01-03","01-04","01-05","01-06","01-07","01-08","02-23","02-24","02-08",
  "05-01","05-08","01-09","06-12","11-06"), 1, 0))

# Set weekend and holidays flags for transcations
raw.df$is_weekend <- ifelse(raw.df$dow %in% c('Saturday','Sunday'), 1, 0)
raw.df$is_FSS_weekend <- ifelse(raw.df$dow %in% c('Friday','Saturday','Sunday'), 1, 0)
raw.df$is_weekend_holiday <- ifelse(raw.df$is_weekend==1 | raw.df$is_holiday==1, 1, 0)
raw.df$is_FSS_weekend_holiday <- ifelse(raw.df$is_FSS_weekend==1 | raw.df$is_holiday==1, 1, 0)

# Choose attributes
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
                   'is_moscow','is_piter','is_other',
                   'dow',
                   'is_weekend','is_FSS_weekend','is_weekend_holiday','is_FSS_weekend_holiday',
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

  
  
  gpsDist <- function(lat1,lon1,lat2,lon2)
  {
    acos(
      sin(lat1*pi/180)*sin(lat2*pi/180) + 
        cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180)
    ) * 6371
  }
  
  raw.df$top_city_center_dist <- gpsDist(raw.df$pos_atm_lat, raw.df$pos_atm_lon, raw.df$top_city_lat, raw.df$top_city_lon)
  raw.df$top_city_center_dist[is.na(raw.df$top_city_center_dist)] <- mean(raw.df$top_city_center_dist, na.rm=T)
  
  # check <- setdiff(unique(shit.df$customer_id), unique(raw.df$customer_id))
  
  # check <- shit.df[shit.df$customer_id %in% check,]
  # check.cities <- as.data.frame(table(check$city))
  # check.cities[order(-check.cities$Freq),]

print("Writing dataset")
# write.csv(raw.df, "output/raw_df.csv",sep=";",col.names=TRUE,row.names=FALSE)


