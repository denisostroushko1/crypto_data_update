
  oldest_data <- read.csv("./Data/LINK-USD.csv")
  
  oldest_data$Date <- as.Date(oldest_data$Date)
  
  oldest_data <- 
    oldest_data %>% select(Date, Close, Open, High,  Low)
  
  colnames(oldest_data) <- c("datetime", "price", "open", "high", "low")
  
  last_record <- max(oldest_data$datetime)
  
  #### new data 
  
  df <- read.csv('./Data/LINK.csv')
  
  df$datetime <- as.Date(df$datetime)
  
  # make historic and current data sets compatible 
  df <- df %>% arrange(datetime)
  
  df <- df %>% 
    dplyr::select(datetime, close, open, high, low)
  
  colnames(df) <- colnames(oldest_data)
  
  ## select data that is not in historic data set yet 
  
  df_supp <- df %>% filter(datetime > last_record)
  
  ## combine the data sets now 
  
  all_data_LINK <- rbind(oldest_data, df_supp) %>% arrange(datetime)

################
# store 
  
write.csv(all_data_LINK, "./Data/one_time_LINK.csv")

source("keys.R")  

put_object(file = "./Data/one_time_LINK.csv", 
           object = "all_available_LINK.csv",
           bucket = bucket_name)  
