# Compute point-based and customer-based attributes



computeDist <- function(x1, y1, x2, y2)
{
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

print("Aggregating data")

# Normalize coordinates!
  
  normalize <- function(x,x0,size)
  {
    (x - x0) / size
  }
  
  # Save original values
  filtered.df$pos_atm_orig_lat <- filtered.df$pos_atm_lat
  filtered.df$pos_atm_orig_lon <- filtered.df$pos_atm_lon
  filtered.df$home_orig_lat <- filtered.df$home_lat
  filtered.df$home_orig_lon <- filtered.df$home_lon
  filtered.df$work_orig_lat <- filtered.df$work_lat
  filtered.df$work_orig_lon <- filtered.df$work_lon
  
  # Convert coordinates
  filtered.df$pos_atm_lat <- normalize(filtered.df$pos_atm_lat, filtered.df$left, filtered.df$size_x)
  filtered.df$pos_atm_lon <- normalize(filtered.df$pos_atm_lon, filtered.df$bottom, filtered.df$size_y)
  # filtered.df$top_city_lat <- normalize(filtered.df$top_city_lat, filtered.df$left, filtered.df$size_x)
  # filtered.df$top_city_lon <- normalize(filtered.df$top_city_lon, filtered.df$bottom, filtered.df$size_y)
  
  # filtered.df$home_lat <- normalize(filtered.df$home_lat, filtered.df$left, filtered.df$size_x)
  # filtered.df$home_lon <- normalize(filtered.df$home_lon, filtered.df$bottom, filtered.df$size_y)
  # filtered.df$work_lat <- normalize(filtered.df$home_lat, filtered.df$left, filtered.df$size_x)
  # filtered.df$work_lon <- normalize(filtered.df$home_lon, filtered.df$bottom, filtered.df$size_y)
  
  

# Aggregate transactions by POS

  pos_level_agg <- filtered.df[
    ,
    .(
      
      # Transaction Sum
      pos_amount=sum(amount), 
      pos_amount_avg=mean(amount),
      pos_amount_cnt=.N,
      pos_amount_min=min(amount, na.rm=T),
      pos_amount_max=max(amount,na.rm=T),
      
      # Transaction Count
      pos_sat_trn_cnt=sum(dow=='Saturday'),
      pos_sun_trn_cnt=sum(dow=='Sunday'),
      pos_weekend_trn_cnt=sum(dow %in% c('Saturday','Sunday')),
      pos_weekday_trn_cnt=sum(!dow %in% c('Saturday','Sunday')),
      pos_dow_cnt=length(unique(dow)),
      
      # Transaction Sum by Weekdays
      pos_sat_amount=sum(amount[dow=="Saturday"]),
      pos_sun_amount=sum(amount[dow=="Sunday"]),
      pos_weekend_amount=sum(amount[dow %in% c("Saturday","Sunday")]),
      pos_weekday_amount=sum(amount[!dow %in% c("Saturday","Sunday")]),
      # dow_mode=which.max(table(dow)),
      
      # Customer level attributes
      left=head(left,1),
      right=head(right,1),
      top=head(top,1),
      top_city_lat=head(top_city_lat,1),
      top_city_lon=head(top_city_lon,1),
      size_x=head(size_x,1),
      size_y=head(size_y,1),
      size=head(size,1),
      bottom=head(bottom,1),
      city=head(city,1),
      country=head(country,1),
      currency=head(currency,1),
      
      is_atm=head(is_atm,1),
      top_city=head(top_city,1),
      top_city_center_dist=head(top_city_center_dist,1),
      pos_atm_orig_lat=head(pos_atm_orig_lat,1),
      pos_atm_orig_lon=head(pos_atm_orig_lon,1)
      
    ),
    by=.(customer_id,pos_atm_lat,pos_atm_lon,mcc,df)]
  

# Customer level aggregates
 
   customer_level_agg <- filtered.df[,
    .(
      
      cust_amount = sum(amount),
      cust_amount_avg = mean(amount),
      cust_amount_cnt = .N,
      cust_amount_max = max(amount),
      cust_weekend_trn_cnt = sum(dow %in% c('Saturday','Sunday')),
      cust_sat_trn_cnt = sum(dow=='Saturday'),
      cust_sun_trn_cnt = sum(dow=='Sunday'),
      cust_weekend_trn_rate = sum(dow %in% c('Saturday','Sunday')) / .N,
      cust_dow_cnt = length(unique(dow)),
      cust_sat_amount = sum(amount[dow=="Saturday"]),
      cust_sun_amount = sum(amount[dow=="Sunday"]),
      cust_weekend_amount = sum(amount[dow %in% c("Saturday","Sunday")]),
      cust_weekday_amount = sum(amount[!dow %in% c("Saturday","Sunday")]),
      
      home_lat=head(home_lat,1),
      home_lon=head(home_lon,1),
      work_lat=head(work_lat,1),
      work_lon=head(work_lon,1),
      
      home_orig_lat = head(home_orig_lat,1),
      home_orig_lon = head(home_orig_lon,1),
      work_orig_lat = head(work_orig_lat,1),
      work_orig_lon = head(work_orig_lon,1)
      
      ), 
    by=.(customer_id)]
  
  df <- merge(pos_level_agg, customer_level_agg, by.x="customer_id", by.y="customer_id")
  
  
  # Current POS amount of all spent money
  df$pos_amount_rate <- round(df$pos_amount / df$cust_amount,3)

  # Current POS transactions of all transactions
  df$pos_cnt_rate <- round(df$pos_amount_cnt / df$cust_amount_cnt, 3)
  
  df$pos_amount_max_rate <- round(df$pos_amount_max / df$cust_amount_max, 3)
  
  df$pos_dow_cnt_rate <- round(df$pos_dow_cnt / df$cust_dow_cnt, 3)
  
  df$pos_sat_cnt_rate <- ifelse(df$cust_sat_trn_cnt == 0, 0, round(df$pos_sat_trn_cnt / df$cust_sat_trn_cnt, 3))
  
  df$pos_sun_cnt_rate <- ifelse(df$cust_sun_trn_cnt==0, 0, round(df$pos_sun_trn_cnt / df$cust_sun_trn_cnt, 3))
  
  df$pos_sat_amount_rate <- ifelse(df$cust_sat_amount==0, 0, round(df$pos_sat_amount / df$cust_sat_amount, 3))
  
  df$pos_sun_amount_rate <- ifelse(df$cust_sun_amount==0, 0, round(df$pos_sun_amount / df$cust_sun_amount, 3))
  
  df$pos_weekend_amount_rate <- ifelse(df$cust_weekend_amount==0,0,round(df$pos_weekend_amount / df$cust_weekend_amount, 3))
  
  df$pos_weekday_amount_rate <- ifelse(df$cust_weekday_amount==0,0,round(df$pos_weekday_amount / df$cust_weekday_amount, 3))
  
  
  
  
  
  
  compute_cell_count <- function(param_df,rounding)
  {
  
    train_home_df = param_df[df=="train",.(home_orig_lat,home_orig_lon)]
    train_work_df = param_df[df=="train" & !is.na(work_orig_lat),.(work_orig_lat,work_orig_lon)]
    
    train_home_df[,
      c("home_orig_lat_cell","home_orig_lon_cell") := 
      .(round(home_orig_lat,rounding),round(home_orig_lon,rounding)),
    ]
    train_work_df[,
      c("work_orig_lat_cell","work_orig_lon_cell") := 
      .(round(work_orig_lat,rounding),round(work_orig_lon,rounding)),
    ]
    home_cells <- train_home_df[,.(cell_home_counts = .N),by=.(home_orig_lat_cell,home_orig_lon_cell)]
    work_cells <- train_work_df[,.(cell_work_counts = .N),by=.(work_orig_lat_cell,work_orig_lon_cell)]
    
    home_cells$normliazed_10_home_counts <- 0
    home_cells$normliazed_1_home_counts <- 0
    work_cells$normliazed_10_work_counts <- 0
    work_cells$normliazed_1_work_counts <- 0
    
    for (i in 1:nrow(home_cells))
    {
      
      current_lat <- home_cells$home_orig_lat_cell[i]
      current_lon <- home_cells$home_orig_lon_cell[i]
      
      # Five element neighborhood
      radius = 10^(1-rounding) / 2
      
      lat_indexes <- home_cells$home_orig_lat_cell >= current_lat - radius & current_lat + radius >= home_cells$home_orig_lat_cell
      lon_indexes <- home_cells$home_orig_lon_cell >= current_lon - radius & current_lon + radius >= home_cells$home_orig_lon_cell
      
      neighborhood_10_home_counts <- sum(home_cells$cell_home_counts[lat_indexes & lon_indexes])
      home_cells$normliazed_10_home_counts[i] <- home_cells$cell_home_counts[i] / neighborhood_10_home_counts
      
      # One element neighborhood
      radius = 10^(-rounding)
      
      lat_indexes <- home_cells$home_orig_lat_cell >= current_lat - radius & current_lat + radius >= home_cells$home_orig_lat_cell
      lon_indexes <- home_cells$home_orig_lon_cell >= current_lon - radius & current_lon + radius >= home_cells$home_orig_lon_cell
      
      neighborhood_1_home_counts <- sum(home_cells$cell_home_counts[lat_indexes & lon_indexes])
      home_cells$normliazed_1_home_counts[i] <- home_cells$cell_home_counts[i] / neighborhood_1_home_counts
      
      
    }
    
    for (i in 1:nrow(work_cells))
    {
      
      current_lat <- work_cells$work_orig_lat_cell[i]
      current_lon <- work_cells$work_orig_lon_cell[i]
      
      # Five element neighborhood
      radius = 10^(1-rounding) / 2
      
      lat_indexes <- work_cells$work_orig_lat_cell >= current_lat - radius & current_lat + radius >= work_cells$work_orig_lat_cell
      lon_indexes <- work_cells$work_orig_lon_cell >= current_lon - radius & current_lon + radius >= work_cells$work_orig_lon_cell
      
      neighborhood_10_work_counts <- sum(work_cells$cell_work_counts[lat_indexes & lon_indexes])
      work_cells$normliazed_10_work_counts[i] <- work_cells$cell_work_counts[i] / neighborhood_10_work_counts
      
      # One element neighborhood
      radius = 10^(-rounding)
      
      lat_indexes <- work_cells$work_orig_lat_cell >= current_lat - radius & current_lat + radius >= work_cells$work_orig_lat_cell
      lon_indexes <- work_cells$work_orig_lon_cell >= current_lon - radius & current_lon + radius >= work_cells$work_orig_lon_cell
      
      neighborhood_1_work_counts <- sum(work_cells$cell_work_counts[lat_indexes & lon_indexes])
      work_cells$normliazed_1_work_counts[i] <- work_cells$cell_work_counts[i] / neighborhood_1_work_counts
      
      
    }
    
    
    
    return (list(home_cells=home_cells, work_cells=work_cells))
  
  }
  
  cells_1 <- compute_cell_count(df, 1)
  home_cells1 <- cells_1$home_cells
  work_cells1 <- cells_1$work_cells
  colnames(home_cells1)[3:5] <- c("cell_home_counts1","cell_normalized_10_home_counts1","cell_normalized_1_home_counts1")
  colnames(work_cells1)[3:5] <- c("cell_work_counts1","cell_normalized_10_work_counts1","cell_normalized_1_work_counts1")
  
  cells_2 <- compute_cell_count(df, 2)
  home_cells2 <- cells_2$home_cells
  work_cells2 <- cells_2$work_cells
  colnames(home_cells2)[3:5] <- c("cell_home_counts2","cell_normalized_10_home_counts2","cell_normalized_1_home_counts2")
  colnames(work_cells2)[3:5] <- c("cell_work_counts2","cell_normalized_10_work_counts2","cell_normalized_1_work_counts2")
  
  cells_3 <- compute_cell_count(df, 3)
  home_cells3 <- cells_3$home_cells
  work_cells3 <- cells_3$work_cells
  colnames(home_cells3)[3:5] <- c("cell_home_counts3","cell_normalized_10_home_counts3","cell_normalized_1_home_counts3")
  colnames(work_cells3)[3:5] <- c("cell_work_counts3","cell_normalized_10_work_counts3","cell_normalized_1_work_counts3")
  
  
  
  
    
  df$pos_atm_orig_lat_rounded1 <- round(df$pos_atm_orig_lat,1)
  df$pos_atm_orig_lon_rounded1 <- round(df$pos_atm_orig_lon,1)
  
  df$pos_atm_orig_lat_rounded2 <- round(df$pos_atm_orig_lat,2)
  df$pos_atm_orig_lon_rounded2 <- round(df$pos_atm_orig_lon,2)
  
  df$pos_atm_orig_lat_rounded3 <- round(df$pos_atm_orig_lat,3)
  df$pos_atm_orig_lon_rounded3 <- round(df$pos_atm_orig_lon,3)
  
  df <- merge(df, home_cells1, by.x=c("pos_atm_orig_lat_rounded1", "pos_atm_orig_lon_rounded1"), by.y=c("home_orig_lat_cell","home_orig_lon_cell"), all.x=T, all.y=F)
  df <- merge(df, work_cells1, by.x=c("pos_atm_orig_lat_rounded1", "pos_atm_orig_lon_rounded1"), by.y=c("work_orig_lat_cell","work_orig_lon_cell"), all.x=T, all.y=F)
  
  df <- merge(df, home_cells2, by.x=c("pos_atm_orig_lat_rounded2", "pos_atm_orig_lon_rounded2"), by.y=c("home_orig_lat_cell","home_orig_lon_cell"), all.x=T, all.y=F)
  df <- merge(df, work_cells2, by.x=c("pos_atm_orig_lat_rounded2", "pos_atm_orig_lon_rounded2"), by.y=c("work_orig_lat_cell","work_orig_lon_cell"), all.x=T, all.y=F)
  
  df <- merge(df, home_cells3, by.x=c("pos_atm_orig_lat_rounded3", "pos_atm_orig_lon_rounded3"), by.y=c("home_orig_lat_cell","home_orig_lon_cell"), all.x=T, all.y=F)
  df <- merge(df, work_cells3, by.x=c("pos_atm_orig_lat_rounded3", "pos_atm_orig_lon_rounded3"), by.y=c("work_orig_lat_cell","work_orig_lon_cell"), all.x=T, all.y=F)
  
  df$pos_atm_orig_lat_rounded1 <- NULL
  df$pos_atm_orig_lon_rounded1 <- NULL
  
  df$pos_atm_orig_lat_rounded2 <- NULL
  df$pos_atm_orig_lon_rounded2 <- NULL
  
  df$pos_atm_orig_lat_rounded3 <- NULL
  df$pos_atm_orig_lon_rounded3 <- NULL
  
  df$cell_home_counts1[is.na(df$cell_home_counts1)] <- 0
  df$cell_work_counts1[is.na(df$cell_work_counts1)] <- 0
  df$cell_normalized_10_home_counts1[is.na(df$cell_normalized_10_home_counts1)] <- 0
  df$cell_normalized_10_work_counts1[is.na(df$cell_normalized_10_work_counts1)] <- 0
  df$cell_normalized_1_home_counts1[is.na(df$cell_normalized_1_home_counts1)] <- 0
  df$cell_normalized_1_work_counts1[is.na(df$cell_normalized_1_work_counts1)] <- 0
  
  df$cell_home_counts2[is.na(df$cell_home_counts2)] <- 0
  df$cell_work_counts2[is.na(df$cell_work_counts2)] <- 0
  df$cell_normalized_10_home_counts2[is.na(df$cell_normalized_10_home_counts2)] <- 0
  df$cell_normalized_10_work_counts2[is.na(df$cell_normalized_10_work_counts2)] <- 0
  df$cell_normalized_1_home_counts2[is.na(df$cell_normalized_1_home_counts2)] <- 0
  df$cell_normalized_1_work_counts2[is.na(df$cell_normalized_1_work_counts2)] <- 0
  
  df$cell_home_counts3[is.na(df$cell_home_counts3)] <- 0
  df$cell_work_counts3[is.na(df$cell_work_counts3)] <- 0
  df$cell_normalized_10_home_counts3[is.na(df$cell_normalized_10_home_counts3)] <- 0
  df$cell_normalized_10_work_counts3[is.na(df$cell_normalized_10_work_counts3)] <- 0
  df$cell_normalized_1_home_counts3[is.na(df$cell_normalized_1_home_counts3)] <- 0
  df$cell_normalized_1_work_counts3[is.na(df$cell_normalized_1_work_counts3)] <- 0
  
  df$cell_home_work_counts1 <- (df$cell_home_counts1+df$cell_work_counts1)
  df$cell_home_work_counts2 <- (df$cell_home_counts2+df$cell_work_counts2)
  df$cell_home_work_counts3 <- (df$cell_home_counts3+df$cell_work_counts3)
  
  # Relative work frequency in the area
  df$cell_home_work_ratio1 <- ifelse(df$cell_home_work_counts1 == 0, NA, df$cell_work_counts1 / df$cell_home_work_counts1)
  df$cell_home_work_ratio2 <- ifelse(df$cell_home_work_counts2 == 0, NA, df$cell_work_counts2 / df$cell_home_work_counts2)
  df$cell_home_work_ratio3 <- ifelse(df$cell_home_work_counts3 == 0, NA, df$cell_work_counts3 / df$cell_home_work_counts3)
  
  # If 0/0 set to mean
  df$cell_home_work_ratio1[is.na(df$cell_home_work_ratio1)] <- mean(df$cell_home_work_ratio1, na.rm=T)
  df$cell_home_work_ratio2[is.na(df$cell_home_work_ratio2)] <- mean(df$cell_home_work_ratio2, na.rm=T)
  df$cell_home_work_ratio3[is.na(df$cell_home_work_ratio3)] <- mean(df$cell_home_work_ratio3, na.rm=T)
  
  
  # View(df1)
  # colnames(x)
  
  
  
  

  # write.table(df, "output/df_agregated.csv", sep=";", row.names=F, col.names = T)
  

  
  
  
  
  
  
  
  
  
  