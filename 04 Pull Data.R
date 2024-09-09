
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
# collect daily data from the API service for the risk dashboard 

cat("
      ***********************************
                 
                  PART 1
                  
      ***********************************
      ")

for(i in 1:nrow(to_pull)){
  tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  
  obj =  paste0("s3://crypto-data-shiny/", to_pull$aws_file_name[i])
  print(paste0("Starting: ", obj))
  
  save_object(object = obj, file = tempfile_15)
  
  print(paste0("Saved: ", obj))
  
  df <- read.csv(tempfile_15)
  print(paste0("Read to  CSV: ", obj))
  
  df$datetime <- as.Date(df$datetime)
  
  cols <- c('datetime',  'price',   'high',    'low',   'open')
  df <- df %>% select(all_of(cols))
  
  api_request(
    FSYM = to_pull$from_symbol[i], 
    TSYM = to_pull$to_sympol[i], 
    E = to_pull$exchange[i], 
    AGGREGATE = 1, 
    LIMIT = 2000
  ) -> api_results_f_output
  
  ### in case I overuse free API service, the request will come back empty 
  ## so there will be no data frame to manipulate 
  ## and the code may result in an error 
  ## so I want to skip over this part and move on with the program 
  
  if(length(api_results_f_output) != 0){
    process_api_request(API_DATA_RESULT = api_results_f_output) -> raw_data_iter
    
    cols2 <- c('timestamp',  'close',   'high',    'low',   'open')
    historical_df <- 
      raw_data_iter %>% 
      select(all_of(cols2)) %>% 
      rename(datetime = timestamp, 
             price = close)
  
    historical_df %>% 
      filter(datetime > max(df$datetime)) %>% 
      
      rbind(.,df) %>% 
      arrange(datetime) -> all_new 
    
    write.csv(all_new, to_pull$aws_file_name[i])
    
    if(file.exists('keys.R') == F){
      put_object(file = to_pull$aws_file_name[i], 
                 object = to_pull$aws_file_name[i],
                 bucket = Sys.getenv("bucket_name"))
    }else{
      put_object(file = to_pull$aws_file_name[i], 
                 object = to_pull$aws_file_name[i],
                 bucket = bucket_name)   
    }
    
    unlink(to_pull$aws_file_name[i])
    
    print(paste0("i: ", i, "; symbol: ", to_pull$from_symbol[i], " complete"))
  }
}

# ********************************************************************************
# ********************************************************************************

# collect hourly data for BTC, ETH
cat("
      ***********************************
                 
                  PART 2
                  
      ***********************************
      ")

#### eth 

url <- "https://min-api.cryptocompare.com/data/v2/histohour"

params <- list(
  fsym = "ETH",
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

tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
save_object(object = "s3://crypto-data-shiny/hourly ETH.csv", file = tempfile_15)

df <- read.csv(tempfile_15)[,-1]
df$check <- sapply(X = df$timestamp, FUN = nchar)

df <- 
  df %>% 
  mutate(
    timestamp = ifelse(check == 10, 
                       paste0(timestamp, " 00:00:00"), 
                       timestamp)
  ) %>% 
  select(-check) %>% 
  mutate(
    timestamp2 = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S")
  ) %>% 
  na.omit()

historical_df_f <- 
  historical_df %>% 
  filter(timestamp > max(df$timestamp) ) 

upload_eth <- 
  rbind(df %>% 
          select(-timestamp) %>% 
          rename(timestamp = timestamp2), 
        historical_df_f) %>% 
  arrange(timestamp)

write.csv(upload_eth, "hourly ETH.csv")

if(file.exists('keys.R') == F){
  put_object(file = "hourly ETH.csv", 
             object = "hourly ETH.csv",
             bucket = Sys.getenv("bucket_name"))
}else{
  put_object(file = "hourly ETH.csv", 
             object = "hourly ETH.csv",
             bucket = bucket_name)   
}

unlink("hourly ETH.csv")

#### BTC 

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

tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
save_object(object = "s3://crypto-data-shiny/hourly BTC.csv", file = tempfile_15)

df <- read.csv(tempfile_15)[,-1]
df$check <- sapply(X = df$timestamp, FUN = nchar)

df <- 
  df %>% 
  mutate(
    timestamp = ifelse(check == 10, 
                       paste0(timestamp, " 00:00:00"), 
                       timestamp)
  ) %>% 
  select(-check) %>% 
  mutate(
    timestamp2 = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S")
  ) %>% 
  na.omit()

historical_df_f <- 
  historical_df %>% 
  filter(timestamp > max(df$timestamp) ) 

upload_btc <- 
  rbind(df %>% 
          select(-timestamp) %>% 
          rename(timestamp = timestamp2), 
        historical_df_f) %>% 
  arrange(timestamp)

write.csv(upload_btc, "hourly BTC.csv")

if(file.exists('keys.R') == F){
  put_object(file = "hourly BTC.csv", 
             object = "hourly BTC.csv",
             bucket = Sys.getenv("bucket_name"))
}else{
  put_object(file = "hourly BTC.csv", 
             object = "hourly BTC.csv",
             bucket = bucket_name)   
}

unlink("hourly BTC.csv")


# ********************************************************************************
# ********************************************************************************

cat("
      ***********************************
                 
                  PART 3
                  
      ***********************************
      ")

##########################################################################
# collect CoinMarkterCap data for general use and new concept development 

tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e

obj =  paste0("s3://crypto-data-shiny/", "master_CMC_scraped.csv")
print(paste0("Starting: ", obj))

save_object(object = obj, file = tempfile_15)

print(paste0("Saved: ", obj))

df <- read.csv(tempfile_15)[,-1]

###### 
# identify new data to pull 
starting_date <- as.Date("2013-04-28")
last_date <- Sys.Date()
# there should be this many weeks 
number_of_weeks <- ceiling((last_date - starting_date)/7) 
data_to_collect <- starting_date + (0:number_of_weeks)*7
data_to_collect <- data_to_collect[data_to_collect<Sys.Date()]

dates_to_collect <- data_to_collect[data_to_collect > max(as.Date(df$date))]

print(paste0("There will be ", length(dates_to_collect), " new weeks of snapshot data from CMC"))

if(length(dates_to_collect) != 0){
  
  print("Staring CMC loop")
  
  for(i in 1:length(dates_to_collect)){
    
    year_input <-  year(dates_to_collect[i])
    month_input <- month(dates_to_collect[i])
    day_input <-   day(dates_to_collect[i])
    iter_df <- scrape_200_from_coinmarketcap(year_input, month_input, day_input)
    df <- rbind(df, scrape_200_from_coinmarketcap(year_input, month_input, day_input))
  
  }
  
  write.csv(df, "master_CMC_scraped.csv")
  
  if(file.exists('keys.R') == F){
    put_object(file = "master_CMC_scraped.csv", 
               object = "master_CMC_scraped.csv",
               bucket = Sys.getenv("bucket_name"))
  }else{
    put_object(file = "master_CMC_scraped.csv", 
               object = "master_CMC_scraped.csv",
               bucket = bucket_name)   
  }
  
  unlink("master_CMC_scraped.csv")

}

################################################################
# collect data from CryptoCompare API and update the data in AWS 

cat("
      ***********************************
                 
                  PART 4
                  
      ***********************************
      ")

  # STEP 1: identify any new coins that are not in the master file of all coins and 
  # exchanges where to pull them from 

WEEK_DAY_FOR_UPDATE = 7

if(wday(Sys.Date(), week_start = 1) != WEEK_DAY_FOR_UPDATE){
  print("Not updating repository of available pairs today"); 
}

  # data with pairs is stored in 'to_populate_3'
  tempfile_15 <- tempfile() 
  
  obj =  paste0("s3://crypto-data-shiny/", 'available assets in API.csv')
  save_object(object = obj, file = tempfile_15)
  
  to_populate_3_old <- read.csv(tempfile_15)
  

# Get this pull done on a Monday 
if(wday(Sys.Date(), week_start = 1) == WEEK_DAY_FOR_UPDATE){
  # data with pairs is stored in 'to_populate_3'
  print("Started update of repository of available pairs today")
  
  source('03 CryptoCompareAPI pairs data.R')
  
  print(paste0("There are ", nrow(to_populate_3_old), " asset combinations in the old data"))
  print(paste0("There are ", nrow(to_populate_3), " asset combinations in the new data"))
  
  setdiff(unique(to_populate_3$symbol_from), 
          unique(to_populate_3_old$symbol_from)) -> new_assets 
  
  print("NEW ASSETS")
  to_populate_3 %>% filter(symbol_from %in% new_assets) %>% select(symbol_from) %>% 
    rename(`New Coins` = symbol_from)
  
  write.csv(to_populate_3, "available assets in API.csv")
  
    if(file.exists('keys.R') == F){
      put_object(file = "available assets in API.csv", 
                 object = "available assets in API.csv",
                 bucket = Sys.getenv("bucket_name"))
    }else{
      put_object(file = "available assets in API.csv", 
                 object = "available assets in API.csv",
                 bucket = bucket_name)   
    }
    
  unlink("available assets in API.csv")
}else{
  to_populate_3 <- to_populate_3_old
}
  
  ###############
  # now download new data from API based on updated list of available pairs 
  
  # It is clear now that updating data for all 4,300 assets as of 6/28/2024
  # and growing is not feasible due to the set up of a free api version 
  
  # so, we will update the data until the data stops being available 
  # the following day, we pick up wehre we left off and keep on going 
  
  
cat("
      ***********************************
                 
                  PART 5
                  
      ***********************************
      ")
  
  print("Downloading old data from AWS")
    
    # data with pairs is stored in 'to_populate_3'
    tempfile_15 <- tempfile() 
    
    obj =  paste0("s3://crypto-data-shiny/", 'all coins historical data.csv')
    save_object(object = obj, file = tempfile_15)
    
    old_coins_data <- read.csv(tempfile_15) 
    
    if("X" %in% colnames(old_coins_data)){old_coins_data <- old_coins_data %>% select(-X)}

    
  ## popilate a list and turn it into a df later rather than growing a data frame within a loop 
  populate_list <- vector(mode = "list", length = length(to_populate_3$symbol_from))
  
  ### create the data frame one time, this will have one entry only: 
  # what asset to pick up to continue updating the data 
  {
    if(T == F){
      write.csv(data.frame(last_symbol = "BTC"), 
                "where to start.csv")

      put_object(file = "where to start.csv", 
           object = "where to start.csv",
           bucket = bucket_name)   
    }
  }
  
  # read in the last symbol to popuate 
  tempfile_15 <- tempfile() 
  obj =  paste0("s3://crypto-data-shiny/", 'where to start.csv')
  save_object(object = obj, file = tempfile_15)
  where_to_start <- read.csv(tempfile_15)[,-1]
  # old version of the update used a sorted list and stopped working at around the 
  # same spot 
  # such as A, B, C, D, E, F, ... and it would break at C or D 
  # 
  # now, we want to reorder the list such that if we break at D 
  # then the next update is D, E, F, ..., A, B, ... 
  
  which(to_populate_3$symbol_from == where_to_start) -> stored_start 
  
  reorder_to_populate_3 <- c(to_populate_3$symbol_from[stored_start:length(to_populate_3$symbol_from)], 
                             to_populate_3$symbol_from[1:(stored_start-1)])
  
  # checking lengths stores the length of the list that comes back from the API request 
  # as soon as the list is empty, we stop the loop 
  # we store the last symbol
  # and we process the results that we have 
  
  checking_length = 1 # this just must be non zero 
  counter = 1 # this is count how many assets we can update 
  
  i = stored_start
  
  while(checking_length != 0){
    
    
    # progress printed to my console only on my local machine 
    if(file.exists("keys.R")){
     if(counter %% 100 == 0){print(paste0("Iteration ", counter))}
    }
    
    # smart~~er way to identify how much to request from API to 
    # shuffle less data around 
    new_days_to_pull_i <- ifelse(
      !(to_populate_3$symbol_from[i] %in% old_coins_data$symbol_from), 
      
      as.numeric(
          anytime::anydate(Sys.Date()) - anytime::anydate(to_populate_3[i, ]$histo_minute_start)
          ), 
    
      as.numeric(
        anytime::anydate(Sys.Date()) - 
          anytime::anydate(max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime, na.rm = T))
      ) 
    )
    
    api_request(
      FSYM = to_populate_3$symbol_from[i], 
      TSYM = to_populate_3$symbol_to[i], 
      E = to_populate_3$exchange[i], 
      AGGREGATE = 1, 
      LIMIT = new_days_to_pull_i
    ) -> api_results_f_output
    
    checking_length = length(api_results_f_output)
    
    if(checking_length != 0){
      ### make data into a data frame 
      process_api_request(API_DATA_RESULT = api_results_f_output) -> raw_data_iter
      cols2 <- c('timestamp',  'close',   'high',    'low',   'open', 'volumefrom', 'volumeto')
      
      historical_df <- 
        raw_data_iter %>% 
        select(all_of(cols2)) %>% 
        rename(datetime = timestamp, 
               price = close)
      
      historical_df$symbol_from = to_populate_3$symbol_from[i]
      historical_df$symbol_to = to_populate_3$symbol_to[i]
      
      if(!is.na(max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime))){
        historical_df %>% filter(
            datetime > max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime)
        )
      }
          
      populate_list[[counter]] = historical_df
    }
    ### end loop 
    counter = counter + 1
    i = i + 1
    if(i > length(reorder_to_populate_3)){i=1}
  }
  
  where_ended <- to_populate_3$symbol_from[i]
  
  print(paste0("Collected data for ", prettyNum(counter, big.mark = ","),  " coins"))
  print(paste0("Started with ", where_to_start))
  print(paste0("Ended on ", where_ended))
  
  write.csv(data.frame(last_symbol = where_ended), 
                "where to start.csv")
  
  if(file.exists('keys.R') == F){
    put_object(file = "where to start.csv", 
               object = "where to start.csv",
               bucket = Sys.getenv("bucket_name"))
  }else{
    put_object(file = "where to start.csv", 
               object = "where to start.csv",
               bucket = bucket_name)   
  }
    
  unlink("where to start.csv")
  
  print(paste0("New rows of raw data added to the historical data set: ", prettyNum(nrow(bind_rows(populate_list) %>% distinct()), big.mark = ",")))
  print(paste0("Data could to be collected for ", prettyNum(length(populate_list), big.mark = ","), " assets "))
  print(paste0("Data was actually obtained for ", prettyNum(lapply(populate_list, nrow) %>% unlist() %>% length(), 
                                                            big.mark = ","), " assets"))
  
  ## turn populate_list into a data frame and append to the old_coins data 
  cat("
      *******************************
                PART 6 
      *******************************
      
      ")
    
    
  # some clearing: there is no easy way to convert EUR and other real currencies to 
  # USD that is easy. So, I am willing to discard these data 
  new_data2 <- bind_rows(populate_list)
  print(new_data2)
  
  if("symbol_to" %in% colnames(new_data2)){
    new_data <- new_data2 %>% filter(!(symbol_to %in% c("EUR", "JPY", "AUD")))
  }
  
  if(nrow(new_data) != 0){#### old data has more column than the data that I have processed here 
    # combine the data first and then start doing the chained price conversions 
    
    new_data$datetime <- as.Date(new_data$datetime, format="%Y-%m-%d")
    old_coins_data$datetime <- as.Date(old_coins_data$datetime, format="%Y-%m-%d")
    
    working_data <- 
      rbind(old_coins_data %>% select(all_of(colnames(new_data))), 
            new_data) %>% 
      arrange(symbol_from, datetime)
    
    working_data <- working_data %>% distinct()
    
    # these are all options of cryptos to convert into from some crypto 
    working_data %>% 
      select(symbol_to) %>% unique() %>% unlist() -> symbol_to_options 
    # remove USD, because that is the final destination 
    symbol_to_options <- symbol_to_options[symbol_to_options != "USD"]
    
    # collect chains of conversion through cryptos until we reach USD 
    results <- vector(mode = "list", length = length(symbol_to_options))
    
    for(i in 1:length(symbol_to_options)){
      
      curr = symbol_to_options[i]
      iter_res = c(curr)
      
      while(curr != "USD"){
        
        working_data %>% filter(symbol_from == curr) %>% select(symbol_to) %>% unique() %>% unlist() -> curr
        iter_res <- c(iter_res, curr)
        
      }
      
      names(iter_res) = paste0("to_",1:length(iter_res))
      results[[i]] = iter_res
    }
    
    ### these are all chains of conversion that eventually ends up in the US dollar. 
    ### it just so happens that they end up in USD, I expect that perhaps some will eventually
    ### end up in the USDT or something like that 
    
     ############################################################################################
    
    ## EXAMPLE OF A CONVERSION CHAIN 
    if(T == F){
      options(scipen = 999)
      rbind(
        working_data %>% filter(symbol_to == "DOGE") %>% head(1),
        working_data %>% filter(symbol_from == "DOGE") %>% head(1),
        working_data %>% filter(symbol_from == "BTC") %>% head(1)
      ) %>% 
        mutate(price = round(price, 6))
    }
    
    #### each entry in a list is a chain of conversion 
    #### each sub entry is a step within a chain 
    
    #   bind_rows(results)
    
    conversion_res <- vector(mode = "list", length = nrow(bind_rows(results)))
    
    for(i in 1:nrow(bind_rows(results))    ){
    
      # bind_rows(results)[i, ]
      
      bind_rows(results)[i, ] %>% apply(., 1, function(x){sum(is.na(x))}) -> na_entries 
      bind_rows(results)[i, ] %>% length() -> total_length
      
      loop_over_N = total_length - na_entries - 1 # take away one because one of the entries is the final destination 
      
      conversion_res[[i]] <- vector(mode = "list", length = loop_over_N)
      
      for( j in 1:loop_over_N   ){
        
        working_data %>% filter(symbol_from == bind_rows(results)[i, j] %>% unlist() & 
                                          symbol_to == bind_rows(results)[i, (j + 1)]  %>% unlist()
                                        ) %>% 
          select(datetime, price) -> iter_df
        
        iter_df$datetime <- as.Date(iter_df$datetime)
        
        if(j == 1){iter_df <- iter_df %>% mutate(conversion_from = bind_rows(results)[i, 1] %>% unlist()) %>% select(conversion_from, datetime, price)}
        
        conversion_res[[i]][[j]] <- iter_df
      }
      
    }
    
    #### process the results 
    to_usd_results <- vector(mode = "list", length = length(conversion_res))
    
    for(i in 1:length(to_usd_results)){
    
      # join all steps of conversion on a day 
      reduce(conversion_res[[i]], full_join, by = "datetime") -> all_res_i
      
      # identify and multiply all 'prices' columns 
      which(substr(colnames(all_res_i), 1, 5) == "price") -> price_cols_id
      colnames(all_res_i)[price_cols_id] -> price_cols
      
      all_res_i %>% 
        select(all_of(price_cols)) %>% 
        apply(., 1, function(x){prod(x)}) -> usd_price
      
      all_res_i$usd_price <- usd_price
      
      all_res_i <- all_res_i %>% select(conversion_from, datetime, usd_price)
        
      to_usd_results[[i]] <- all_res_i
    }
    
    final_conversions_df <- bind_rows(to_usd_results)
    
    ###############################
    
    # final data set 
    
    working_data2 <- 
      left_join(
        x = working_data %>% distinct(), 
        y = final_conversions_df %>% rename(symbol_to = conversion_from ) %>% distinct(), 
        
        by = c("symbol_to", "datetime")
      ) %>% 
      arrange(symbol_from, symbol_to, datetime, volumefrom) %>% 
      group_by(symbol_from, symbol_to, datetime) %>% 
      slice_tail(n = 1) %>% 
      ungroup()
    
    prettyNum(nrow(working_data), big.mark = ",")
    prettyNum(nrow(working_data2 %>% distinct()), big.mark = ",")
    
    ## 
    working_data2 <- 
      working_data2 %>% 
      mutate(
        usd_price_final = ifelse(symbol_to == "USD", 
                           price, 
                           price * usd_price)
      ) %>% 
      select(-usd_price) %>% 
      rename(usd_price = usd_price_final) %>% 
      na.omit() %>% 
      filter(symbol_from != "WBTC")
    
      prettyNum(nrow(working_data2), big.mark = ",")
      prettyNum(length(unique(working_data2$symbol_from)), big.mark = ",")
    
    
  ####################
    write.csv(working_data2, "all coins historical data.csv")
    
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
}


  