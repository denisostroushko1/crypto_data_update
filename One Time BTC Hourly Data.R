
# https://www.cryptodatadownload.com/data/#google_vignette

if(file.exists('keys.R') == T){rm(list = ls())}

source("Master Packages.R")
source("Master Functions.R")

source('01 parameters for api.R')

if(file.exists('keys.R') == T){
  source("keys.R")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key, 
             "AWS_DEFAULT_REGION" =  aws_region)
  
  print("Connected to AWS using local keys")
}

if(file.exists('keys.R') == F){
  
  print(Sys.getenv("access_key"))
  print(Sys.getenv("secret_key"))
  print(Sys.getenv("aws_region"))
  print('If we see "***" then the keys were read in right')
  
        Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("access_key"),
                   "AWS_SECRET_ACCESS_KEY" = Sys.getenv("secret_key"), 
                   "AWS_DEFAULT_REGION" =  Sys.getenv("aws_region"))
  
  print("Connected to AWS using secret keys")
    
}

#################################################################
############            BTC              ########################
#################################################################

hourly_btc <- 
  
  read_csv(file = "./Data/Gemini_BTCUSD_1h.csv", skip = 1) %>% 
  as.data.frame() %>% 
  rename(timestamp = date) %>% 
  select(-`Volume BTC`, -`Volume USD`, -symbol) 

#################################################################

url <- "https://min-api.cryptocompare.com/data/v2/histohour"

params <- list(
  fsym = "BTC",
  tsym = "USDT",
  e = "Binance", 
  aggregate = 1,
  limit = 2000
)
  
# Make GET request to the API
response <- GET(url, query = params)
data <- content(response, "parsed")
historical_data <- data$Data$Data

historical_df <- as.data.frame(historical_data %>% bind_rows())

historical_df$timestamp <- as.POSIXct(historical_df$time %>% unlist(), origin = "1970-01-01", tz = "UTC")

historical_df <- 
  historical_df %>% 
  select(time, timestamp, close, high, low, open) %>% 
  rename(unix = time)

historical_df_f <- 
  historical_df %>% 
  filter(timestamp > max(hourly_btc$timestamp) ) 

upload <- 
  rbind(
    historical_df_f, 
    hourly_btc
  ) %>% 
  arrange(timestamp)

#################################################################

write.csv(upload, "hourly BTC.csv")

put_object(file = "hourly BTC.csv", 
           object = "hourly BTC.csv",
           bucket = bucket_name)   

unlink("hourly BTC.csv")

#################################################################
############            ETH              ########################
#################################################################

hourly_eth <- 
  
  read_csv(file = "./Data/Gemini_ETHUSD_1h.csv", skip = 1) %>% 
  as.data.frame() %>% 
#  mutate(timestamp = as.POSIXct(unix , origin = "1970-01-01", tz = "UTC") ) %>% 
  select(-`Volume ETH`, -`Volume ETH`, -symbol, -`Volume USD`) %>% 
  rename(timestamp = date)

#################################################################

url <- "https://min-api.cryptocompare.com/data/v2/histohour"

params <- list(
  fsym = "ETH",
  tsym = "USDT",
  e = "Binance", 
  aggregate = 1,
  limit = 100
)
  
# Make GET request to the API
response <- GET(url, query = params)
data <- content(response, "parsed")
historical_data <- data$Data$Data

historical_df <- as.data.frame(historical_data %>% bind_rows())

historical_df$timestamp <- as.POSIXct(historical_df$time %>% unlist(), origin = "1970-01-01", tz = "UTC")

historical_df <- 
  historical_df %>% 
  select(time, timestamp, close, high, low, open) %>% 
  rename(unix = time)

historical_df_f <- 
  historical_df %>% 
  filter(timestamp > max(hourly_eth$timestamp) ) 

upload <- 
  rbind(
    historical_df_f, 
    hourly_eth
  ) %>% 
  arrange(timestamp)

#################################################################

 write.csv(upload, "hourly ETH.csv")

put_object(file = "hourly ETH.csv", 
           object = "hourly ETH.csv",
           bucket = bucket_name)   

unlink("hourly ETH.csv")
