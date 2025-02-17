

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
#####
## STEP 1: 
## go and download all data that is needed for this by hand from these links 

#   https://www.blockchain.com/explorer/charts/total-bitcoins
#   https://www.coingecko.com/en/global-charts

#################################################################
#####
## STEP 2: 
## prepare data for BTC supply, use prices to get total market cap 

raw_btc_supply <- 
  fromJSON(paste(readLines('total-bitcoins.json'), collapse=""))$`total-bitcoins` %>% 
    mutate(timestamp = as.POSIXct((x %>% unlist())/1000, origin = "1970-01-01", tz = "UTC"), 
           date = as.Date(timestamp)) %>% 
  select(date, y) %>% 
  rename(supply = y)

true_dates <- 
  data.frame(
    date = seq(from = min(raw_btc_supply$date), 
               to = as.Date(Sys.Date()), 
               by = "1 day")
  )

btc_supply <- 
  left_join(
    x = true_dates, 
    y = raw_btc_supply, 
    by = "date"
  ) %>% 
  mutate(supply = zoo::na.locf(supply))

head(btc_supply)
tail(btc_supply)

tempfile_15 <- tempfile()
save_object(object = "s3://crypto-data-shiny/all_available_BTC.csv", file = tempfile_15)
df <- read.csv(tempfile_15)[,-1] %>% mutate(datetime = as.Date(datetime))

btc_mc <- 
  left_join(
    x = btc_supply, 
    y = df %>% select(datetime, price) %>% rename(date = datetime), 
    by = "date"
  ) %>% 
  mutate(btc_mc = supply*price) %>% 
  select(-supply,-price) %>% 
  na.omit()


#################################################################
## STEP 3: 
## Total Market Cap 

work_df <- read_csv("CoinGecko-GlobalCryptoMktCap.csv")
work_df <- work_df %>% 
  mutate(timestamp = as.POSIXct((work_df$snapped_at %>% unlist())/1000, origin = "1970-01-01", tz = "UTC"), 
         timestamp = as.Date(timestamp)) %>% 
  select(-total_volume, -snapped_at) %>%
  rename(price = market_cap)

head(work_df)
tail(work_df)

put_object(file = "CoinGecko-GlobalCryptoMktCap.csv", 
           object = "TOTAL MC.csv",
           bucket = bucket_name)   
    
#################################################################
## STEP 4: 
## Download Eth Market Cap, use Total - ETH - BTC to get Total 3 aka altcoin MC 

save_object(object = "s3://crypto-data-shiny/ETH MC.csv", file = tempfile_15)
df <- 
  read.csv(tempfile_15)[,-1]  %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  rename(date = datetime, 
         eth_mc = price)

head(df)
tail(df)

if(T == F){
  
  ## Quick remidnder where stuff is and where we keep it 
  
  ## TOTAL 
  min(work_df$timestamp)
  max(work_df$timestamp)
  
  ## BTC 
  min(btc_mc$date)
  max(btc_mc$date)
  
  ## ETH
  min(df$date)
  max(df$date)
}
## 
total_3 <- 
  left_join(
    x =  btc_mc, 
    y = work_df %>% rename(total_mc = price, date = timestamp), 
    by = "date"
  ) %>% 
  left_join(
    df, 
    by = "date"
  ) %>% 
  mutate(total_mc = ifelse(is.na(total_mc), 0, total_mc),
         btc_mc = ifelse(is.na(btc_mc), 0, btc_mc),
         eth_mc = ifelse(is.na(eth_mc), 0, eth_mc)
         ) %>% 
  mutate(
    price = total_mc - btc_mc - eth_mc
  ) %>% 
  filter(price > 0) %>% 
  rename(datetime = date) %>% 
  select(datetime, price)

head(total_3)
tail(total_3)

if(T == F){
  
  min(total_3$datetime)
  max(total_3$datetime)
  
  tail(total_3)  
  
  total_3$price[nrow(total_3)] %>% scales::dollar(., 2)
}

##############
# SEND TO AWS 
##############

write.csv(total_3, "TOTAL3 MC.csv")

head(total_3)
tail(total_3)

if(file.exists('keys.R') == F){
  put_object(file = "TOTAL3 MC.csv", 
             object = "TOTAL3 MC.csv",
             bucket = Sys.getenv("bucket_name"))
}else{
  put_object(file = "TOTAL3 MC.csv", 
             object = "TOTAL3 MC.csv",
             bucket = bucket_name)   
}

unlink("TOTAL3 MC.csv")
