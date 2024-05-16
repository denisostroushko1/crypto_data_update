

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

unlink(df, "master_CMC_scraped.csv")
