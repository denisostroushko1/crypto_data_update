
  oldest_data <- read.csv("./Data/ETH_USD_2015-08-08_2020-10-21-CoinDesk.csv")
  
  oldest_data <- oldest_data[,-1]
  
  oldest_data$Date <- as.Date(oldest_data$Date)
  
  colnames(oldest_data) <- c("datetime", "price", "open", "high", "low")
  
  last_record <- max(oldest_data$datetime)
  
  #### new data 
  
  df <- read.csv("./Data/ETH.csv")
  
  df$datetime <- as.Date(df$datetime)
  
  # make historic and current data sets compatible 
  df <- df %>% arrange(datetime)
  
  df <- df %>% 
    dplyr::select(datetime, close, open, high, low)
  
  colnames(df) <- colnames(oldest_data)
  
  ## select data that is not in historic data set yet 
  
  df_supp <- df %>% filter(datetime > last_record)
  
  ## combine the data sets now 
  
  all_data_ETH <- rbind(oldest_data, df_supp) %>% arrange(datetime)
 
################
# store 
  
write.csv(all_data_ETH, "./Data/one_time_ETH.csv")

source("keys.R")  

put_object(file = "./Data/one_time_ETH.csv", 
           object = "all_available_ETH.csv",
           bucket = bucket_name)  
