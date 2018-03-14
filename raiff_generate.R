# Enrich data with distance-based attributes

# Generate distnace-based features For both Train and Test

  
  # List of customers to use (all train + test)
  
  raw_customers <- unique(df$customer_id)
  
  # Import libraries

    library(fpc)
    library(cluster)
    library(doMC)
  
  
  # Register parallel execution
  
    registerDoMC(24)
    n_threads = 24
  
  
  # Result dataset
  
    result <- NULL
  
  
  
  p_time <- system.time(
  
    {  
  
  result <- foreach(thread = 1:n_threads, .combine=rbind) %dopar%
  {
    
    # Initialize local result
    
      thread_result <- NULL
  
    
    # Set size of data to process for current task
    
      stripe = floor(length(raw_customers) / n_threads)
    
    
    # Block start
    
      from_custid = (thread-1)*stripe + 1
    
    # Block end
    
      if (thread == n_threads)
        to_custid = length(raw_customers)
      if (thread != n_threads)
        to_custid = min(from_custid + stripe - 1, length(raw_customers))
    
    
    # Print work distribution parameters
    
      print(paste("initializing process ",thread," from=",from_custid, " to=", to_custid, sep=""))
    
      
    
    # Local loop
      
      for (c in from_custid : to_custid)
      {
        
        # print(paste("pid=",thread, " processing customer ",c,sep=""))
  
        # Print every 100 customers processed
        if (c%%200==0)
        {
  
          Sys.sleep(20)
        }
        
        if (thread == 1 & c%%10==0)
        {
          
          print(paste("pid ",thread," ","processing customer ",c,"\r",sep=""))
        }
    
        # Customer Data
    
          custid = raw_customers[c]
          current_transactions <- df[customer_id == custid,]
          # customer_data = current_transactions
    
        # Process customer
          
          customer_data <- process_customer(current_transactions)
    
        
        # If first iteration initialize dataframe struvcture
          
          if (is.null(thread_result))
            thread_result <- customer_data[0==1,]
        
        thread_result <- rbind(thread_result, customer_data)
        
      }
    
    
    # print(nrow(thread_result))
    
    thread_result
    
  }
    
  
    })
  
  p_time
    
  write.csv(result, "output/raiff_attrs.csv", sep=";", row.names=F)



