
rm(list = ls())



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

#     colnames(to_populate_3)
#     "histo_minute_start" "histo_minute_end"   "isActive"           "exchange"           "symbol_from"        "symbol_to"          "data_days_amount"   "row_id"        


  # data with pairs is stored in 'to_populate_3'
  tempfile_15 <- tempfile() 
  
  obj =  paste0("s3://crypto-data-shiny/", 'available assets in API.csv')
  save_object(object = obj, file = tempfile_15)
  
  to_populate_3 <- read.csv(tempfile_15)
  
populate_list <- vector(mode = "list", length = length(to_populate_3$symbol_from))
 
if(T == F){
  lapply(populate_list, length) %>% unlist() %>% {which(. == 0)} 
}

####################################################
  
  nrow_running = 0
  
  for(i in 4138:nrow(to_populate_3)){
      
    if(i %% 100 == 0){print(paste0("Iteration ", i, " of ", nrow(to_populate_3), 
                                   ". Running total rows: ", prettyNum(nrow_running, big.mark = ",")
                                   )
                            )
      }
    
    api_request(
      FSYM = to_populate_3$symbol_from[i], 
      TSYM = to_populate_3$symbol_to[i], 
      E = to_populate_3$exchange[i], 
      AGGREGATE = 1, 
      LIMIT = 2000
    ) -> api_results_f_output

    
    if(length(api_results_f_output) != 0){
      process_api_request(API_DATA_RESULT = api_results_f_output) -> raw_data_iter
      cols2 <- c('timestamp',  'close',   'high',    'low',   'open', 'volumefrom', 'volumeto')
      
      historical_df <- 
        raw_data_iter %>% 
        select(all_of(cols2)) %>% 
        rename(datetime = timestamp, 
               price = close)
      
      historical_df$symbol_from = to_populate_3$symbol_from[i]
      historical_df$symbol_to = to_populate_3$symbol_to[i]
    
      historical_df <- historical_df %>% filter( price > 0)
      
      historical_df$datetime <- as.Date(historical_df$datetime, format="%m/%d/%Y")
  
      populate_list[[i]] <- historical_df
      
      nrow_running <- nrow_running + nrow(historical_df)
    }
  }
####################################################

#################
# removing some weird instances of insignificant coins
# they are supposed to have data, but have no records 

major_historical_df <- bind_rows(populate_list)

prettyNum(nrow(major_historical_df), big.mark = ",")
prettyNum(length(unique(major_historical_df$symbol_from)), big.mark = ",")

to_populate_3 %>% select(symbol_from) %>% 
  unique() %>% 
  mutate(in_repo = 1) -> in_master_list


major_historical_df %>% select(symbol_from) %>% 
  unique() %>% 
  mutate(in_historical = 1) -> in_historical

full_join(in_master_list, in_historical, by = "symbol_from") %>% arrange(symbol_from) -> check_df 

summary(check_df)

check_df %>% filter(is.na(in_historical )) %>% select(symbol_from) %>% unlist() -> to_remove

major_historical_df <- major_historical_df %>% 
  filter(!(symbol_from %in% to_remove))


prettyNum(nrow(major_historical_df), big.mark = ",")
prettyNum(length(unique(major_historical_df$symbol_from)), big.mark = ",")

#################

major_historical_df2 <- 
  major_historical_df %>% 
  filter(price > 0)

prettyNum(nrow(major_historical_df2), big.mark = ",")
prettyNum(length(unique(major_historical_df2$symbol_from)), big.mark = ",")

####################################
# remove all duplicated rows of data 

major_historical_df3 <- distinct(major_historical_df2)

prettyNum(nrow(major_historical_df3), big.mark = ",")
prettyNum(length(unique(major_historical_df3$symbol_from)), big.mark = ",")

## check for duplicates again  
if(T == F){
  
  to_populate_3 %>% select(symbol_from) %>% 
  unique() %>% 
  mutate(in_repo = 1) -> in_master_list

  
  major_historical_df %>% select(symbol_from) %>% 
    unique() %>% 
    mutate(in_historical = 1) -> in_historical
  
  full_join(in_master_list, in_historical, by = "symbol_from") %>% arrange(symbol_from) -> check_df 
  
  major_historical_df3 %>% 
    group_by(symbol_from, datetime) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% select(n) %>% summary()
# okay good there are no duplicates 
}

write.csv(major_historical_df3, "all coins historical data.csv")

  if(file.exists('keys.R') == F){
    put_object(file = "all coins historical data.csv", 
               object = "all coins historical data.csv",
               bucket = Sys.getenv("bucket_name"))
  }else{
    put_object(file = "all coins historical data.csv", 
               object = "all coins historical data.csv",
               bucket = bucket_name)   
  }
  
unlink("all coins historical data.csv")

