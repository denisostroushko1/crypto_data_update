

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

print("
      ***********************************<br>
      PART 1 <br>
      ***********************************<br>
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

##########################################################################
# collect CoinMarkterCap data for general use and new concept development 

tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e

obj =  paste0("s3://crypto-data-shiny/", "master_CMC_scraped.csv")
print(paste0("Starting: ", obj))

save_object(object = obj, file = tempfile_15)

print(paste0("Saved: ", obj))

df <- read.csv(tempfile_15)[,-1]

# ********************************************************************************
# ********************************************************************************

print("
      ***********************************<br>
      PART 2 <br>
      ***********************************<br>
      ")

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

print("
      ***********************************<br>
      PART 3 <br>
      ***********************************<br>
      ")

  # STEP 1: identify any new coins that are not in the master file of all coins and 
  # exchanges where to pull them from 

if(wday(Sys.Date(), week_start = 1) != 7){
  print("Not updating repository of available pairs today"); 
}

  # data with pairs is stored in 'to_populate_3'
  tempfile_15 <- tempfile() 
  
  obj =  paste0("s3://crypto-data-shiny/", 'available assets in API.csv')
  save_object(object = obj, file = tempfile_15)
  
  to_populate_3_old <- read.csv(tempfile_15)
  

# Get this pull done on a Monday 
if(wday(Sys.Date(), week_start = 1) == 7){
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

  
  ###############
  # now download new data from API based on updated list of available pairs 
  
  
print("
      ***********************************<br>
      PART 3 <br>
      ***********************************<br>
      ")
  
  print("Downloading old coins data from AWS")
    
    # data with pairs is stored in 'to_populate_3'
    tempfile_15 <- tempfile() 
    
    obj =  paste0("s3://crypto-data-shiny/", 'all coins historical data.csv')
    save_object(object = obj, file = tempfile_15)
    
    old_coins_data <- read.csv(tempfile_15)

    
  ## popilate a list and turn it into a df later rather than growing a data frame within a loop 
  populate_list <- vector(mode = "list", length = length(to_populate_3$symbol_from))
  
  for(i in 1:length(populate_list)){
      
    print(paste0("Last loop: ", i, " of ", length(populate_list)))
    ## file with AWS keys and such exists only on my local machine, and I want 
    # progress printed to my console only on my local machine 
    if(file.exists("keys.R")){
     if(i %% 100 == 0){print(paste0("Iteration ", i, " of ", length(populate_list)))}
    }
    
    # this ensures that only pull a small slice of the data from the API 
    
    
    new_days_to_pull_i <- ifelse(
      !(to_populate_3$symbol_from[i] %in% old_coins_data$symbol_from), 
      
      as.numeric(
          as.Date(Sys.Date()) - as.Date(to_populate_3[i, ]$histo_minute_start)
          ), 
    
      as.numeric(
        as.Date(Sys.Date()) - 
          as.Date(max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime, na.rm = T))
      ) 
    )
    
    # get the data 
    api_request(
      FSYM = to_populate_3$symbol_from[i], 
      TSYM = to_populate_3$symbol_to[i], 
      E = to_populate_3$exchange[i], 
      AGGREGATE = 1, 
      LIMIT = new_days_to_pull_i
    ) -> api_results_f_output
    
    # if data exists, turn output of previous function, 
    # which is a list, to a data frame and store the results 
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
      
      if(!is.na(max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime))){
        historical_df %>% filter(
            datetime > max(old_coins_data[old_coins_data$symbol_from == to_populate_3$symbol_from[i], ]$datetime)
        )
      }
      
      populate_list[[i]] = historical_df
    }
  }
  
  ## turn populate_list into a data frame and append to the old_coins data 
  
  major_historical_df3 <- 
    rbind(old_coins_data %>% select(-X), 
          bind_rows(populate_list)
          )
  
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

}