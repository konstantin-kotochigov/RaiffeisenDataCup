library(data.table)


# Delete empty transactions
filtered.df <- raw.df[!is.na(pos_atm_lat) & !is.na(pos_atm_lon),]


# Delete customers from Train with incostitent target variables
filterDF <- filtered.df[df=="train",
                   .(
                     cnt=.N, 
                     dups1=length(unique(home_lat)),
                     dups2=length(unique(home_lon)),
                     dups3=length(unique(work_lat)),
                     dups4=length(unique(work_lon))
                   ), by = customer_id]

# Delete customers with inconsistent target variable (TODO - manually check them!)
train.customers <- filterDF[dups1 == 1 & dups2 == 1 & dups3 == 1 & dups4 == 1, c("customer_id")]
test.customers <- unique(raw.df[raw.df$df=="test",c("customer_id")])

filtered.df <- merge(filtered.df, rbind(train.customers,test.customers), by.x="customer_id", by.y="customer_id")



# raw.df <- filtered.df

# Delete transactions that are 50km from top city center (for some customers all transactions - CHECK!)
shit.df <- filtered.df[filtered.df$top_city_center_dist >= 50,]
filtered.df <- filtered.df[is.na(filtered.df$top_city_center_dist) | filtered.df$top_city_center_dist < 50,]

filtered.df$home_lat[filtered.df$df=="test"] <- NA
filtered.df$home_lon[filtered.df$df=="test"] <- NA
filtered.df$work_lat[filtered.df$df=="test"] <- NA
filtered.df$work_lon[filtered.df$df=="test"] <- NA


# Calculate bounding area for each customer_id (don't forget to include home and work points)
areaCoordinates <- filtered.df[,
                            .(
                              left=min(pos_atm_lat,na.rm=T),
                              right=max(pos_atm_lat,na.rm=T),
                              bottom=min(pos_atm_lon,na.rm=T),
                              top=max(pos_atm_lon,na.rm=T),
                              home_lat=head(home_lat,1),
                              home_lon=head(home_lon,1),
                              work_lat=head(work_lat,1),
                              work_lon=head(work_lon,1),
                              center_lat=mean(pos_atm_lat, na.rm=T),
                              center_lon=mean(pos_atm_lon, na.rm=T)
                            ), by = .(customer_id)]

areaCoordinates$left <-   pmin(areaCoordinates$left, areaCoordinates$home_lat, areaCoordinates$work_lat, na.rm=T)
areaCoordinates$right <-  pmax(areaCoordinates$right, areaCoordinates$home_lat, areaCoordinates$work_lat, na.rm=T)
areaCoordinates$top <-    pmax(areaCoordinates$top, areaCoordinates$home_lon, areaCoordinates$work_lon, na.rm=T)
areaCoordinates$bottom <- pmin(areaCoordinates$bottom, areaCoordinates$home_lon, areaCoordinates$work_lon, na.rm=T)

areaCoordinates$home_lat <- NULL
areaCoordinates$home_lon <- NULL
areaCoordinates$work_lat <- NULL
areaCoordinates$work_lon <- NULL

areaCoordinates$center_lat <- (areaCoordinates$left+areaCoordinates$right) / 2
areaCoordinates$center_lon <- (areaCoordinates$top + areaCoordinates$bottom) / 2
areaCoordinates$size_x <- areaCoordinates$right - areaCoordinates$left
areaCoordinates$size_y <- areaCoordinates$top - areaCoordinates$bottom

areaCoordinates$size_x[areaCoordinates$size_x==0] <- 0.01
areaCoordinates$size_y[areaCoordinates$size_y==0] <- 0.01

areaCoordinates$size <- pmax(areaCoordinates$size_x, areaCoordinates$size_y) 
areaCoordinates$zoom <- ifelse(areaCoordinates$size < 0.15, 12, ifelse(areaCoordinates$size<0.30, 11, 10))



filtered.df <- merge(filtered.df, areaCoordinates, by.x="customer_id", by.y="customer_id", all.x=T, all.y=F)

# train.df <- raw.df[raw.df$df=="train",]
# test.df <- raw.df[raw.df$df=="test",]


write.csv(filtered.df, "output/filtered_df",sep=";",col.names=TRUE,row.names=FALSE)



