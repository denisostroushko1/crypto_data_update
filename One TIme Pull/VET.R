
  oldest_data <- read.csv('./Data/VET-USD (1).csv')

  oldest_data$datetime <- as.Date(oldest_data$Date)

  oldest_data <- na.omit(oldest_data)

  oldest_data <- oldest_data %>% dplyr::select(datetime, Close, Open, High, Low)

  colnames(oldest_data) <- c("datetime", "price", "open", "high", "low")

  last_record <- max(oldest_data$datetime)

  #### new data

  df <- read.csv('./Data/VET.csv')

  df$datetime <- as.Date(df$datetime)

  min_datetime <- min(df$datetime)
  # make historic and current data sets compatible
  df <- df %>% arrange(datetime)

  df <- df %>%
    dplyr::select(datetime, close, open, high, low)

  colnames(df) <- colnames(oldest_data)

  ## select data that is not in historic data set yet

  oldest_data_supp <- oldest_data %>% filter(datetime < min_datetime)

  ## combine the data sets now

  all_data_VET <- rbind(oldest_data_supp, df) %>% arrange(datetime)


################
# store 
  
write.csv(all_data_VET, "./Data/one_time_VET.csv")

source("keys.R")  

put_object(file = "./Data/one_time_VET.csv", 
           object = "all_available_VET.csv",
           bucket = bucket_name)       
  