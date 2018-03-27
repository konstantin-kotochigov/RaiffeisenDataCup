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
  filtered.df$top_city_lat <- normalize(filtered.df$top_city_lat, filtered.df$left, filtered.df$size_x)
  filtered.df$top_city_lon <- normalize(filtered.df$top_city_lon, filtered.df$bottom, filtered.df$size_y)
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
  

  write.table(df, "output/df_agregated.csv", sep=";", row.names=F, col.names = T)
  

  
  
  
  
  
  
  
  
  
  