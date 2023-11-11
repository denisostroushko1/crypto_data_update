
## last one time pull: 2023-11-10 


oldest_data <- read.csv("./Data/Bitcoin Historical Data - Investing.com.csv")
  
  # oldest_data <- read.csv('Bitcoin Historical Data - Investing.com.csv', header = T)
  
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

  d2 <- read.csv("./Data/BTC_USD_2013-09-30_2020-12-15-CoinDesk.csv")

  d2 <- d2 %>% select(-Currency)

  colnames(d2) <- colnames(other_f)

  d2$datetime <- as.Date(as.character(d2$datetime))

  min <- min(d2$datetime)

  other_f <- other_f %>% filter(datetime < min)

  other_f <- rbind(other_f, d2)
  
  last_record <- max(other_f$datetime)
  
  df <- read.csv("./Data/BTC.csv")
  
  df$datetime <- as.Date(df$datetime)
  
  # make historic and current data sets compatible 
  df <- df %>% arrange(datetime)
  
  df <- df %>% 
    dplyr::select(datetime, close, open, high, low)
  
  colnames(df) <- colnames(other_f)
  
  ## select data that is not in historic data set yet 
  
  df_supp <- df %>% filter(datetime > last_record)
  
  ## combine the data sets now 
  
  all_data <- rbind(other_f, df_supp) %>% arrange(datetime)

################
# store 
  
write.csv(all_data, "./Data/one_time_BTC.csv")

source("keys.R")  

put_object(file = "./Data/one_time_BTC.csv", 
           object = "all_available_BTC.csv",
           bucket = bucket_name)  

