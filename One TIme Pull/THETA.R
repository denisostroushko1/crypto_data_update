
  oldest_data <- read.csv('./Data/THETA Historical Data - Investing.com.csv')
  
  oldest_data$datetime <- as.Date(oldest_data$Date,format='%B %d, %Y')
  
  oldest_data <- na.omit(oldest_data)
  
  oldest_data <- oldest_data %>% dplyr::select(datetime, Price, Open, High, Low)
  
  colnames(oldest_data) <- c("datetime", "price", "open", "high", "low")
  
  other_f <- oldest_data
  
  for(i in 2:5){
    other_f[,i] <- as.character(other_f[,i])
    
    other_f[,i] <- as.numeric(
      gsub(",", # take this symbol
           "", # replace wth this symbol
           other_f[,i]) # in this column
    )
  }
  
  last_record <- max(oldest_data$datetime)
  
  df <- read.csv('./Data/THETA.csv')
  
  df$datetime <- as.Date(df$datetime)
  
  # make historic and current data sets compatible 
  df <- df %>% arrange(datetime)
  
  df <- df %>% 
    dplyr::select(datetime, close, open, high, low)
  
  colnames(df) <- colnames(oldest_data)
  
  ## select data that is not in historic data set yet 
  
  df_supp <- df %>% filter(datetime > last_record)
  
  ## combine the data sets now 
  
  all_data_THETA <- rbind(oldest_data, df_supp) %>% arrange(datetime)

################
# store 
  
write.csv(all_data_THETA, "./Data/one_time_THETA.csv")

source("keys.R")  

put_object(file = "./Data/one_time_THETA.csv", 
           object = "all_available_THETA.csv",
           bucket = bucket_name)     