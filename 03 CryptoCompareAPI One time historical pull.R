
source("Master Packages.R")

#     colnames(to_populate_3)
#     "histo_minute_start" "histo_minute_end"   "isActive"           "exchange"           "symbol_from"        "symbol_to"          "data_days_amount"   "row_id"        

major_historical_df <- data.frame(
  datetime = POSIXct(), 
  price = numeric(), 
  high = numeric(), 
  low = numeric(), 
  open = numeric(), 
  volumefrom = numeric(), 
  volumeto = numeric(), 
  symbol_from = character(),
  symbol_to  = character()
)

i = 3736

 

for(i in 3736:nrow(to_populate_3)){
    
  if(i %% 100 == 0){print(paste0("Iteration ", i, " of ", nrow(to_populate_3)))}
  
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
  
    major_historical_df = rbind(major_historical_df, historical_df)
  }
  
  if(i %% 100 == 0){print(paste0("Running total rows of data: ", nrow(major_historical_df)))}
}

#################
# removing some weird instances of insignificant coins
# they are supposed to have data, but have no records 


to_populate_3 %>% select(symbol_from) %>% 
  unique() %>% 
  mutate(in_repo = 1) -> in_master_list


major_historical_df %>% select(symbol_from) %>% 
  unique() %>% 
  mutate(in_historical = 1) -> in_historical

full_join(in_master_list, in_historical, by = "symbol_from") %>% arrange(symbol_from) -> check_df 

check_df %>% filter(is.na(in_historical )) %>% select(symbol_from) %>% unlist() -> to_remove

major_historical_df <- major_historical_df %>% 
  filter(!(symbol_from %in% to_remove))

#################


major_historical_df2 <- 
  major_historical_df %>% 
  filter(price > 0)

####################################
# remove all duplicated rows of data 

major_historical_df3 <- distinct(major_historical_df2)

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

