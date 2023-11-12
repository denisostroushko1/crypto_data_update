
source("Master Packages.R")
source("Master Functions.R")

source('01 parameters for api.R')

if(1 == 2){

if(file.exists('keys.R') == T){
  source("keys.R")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key, 
             "AWS_DEFAULT_REGION" =  aws_region)
  
  print("Connected to AWS using local keys")
}

if(file.exists('keys.R') == F){
        Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("access_key"),
                   "AWS_SECRET_ACCESS_KEY" = Sys.getenv("secret_key"), 
                   "AWS_DEFAULT_REGION" =  Sys.getenv("aws_region"))
  
  print("Connected to AWS using secret keys")
    
}

  
for(i in 1:nrow(to_pull)){
  tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  
  obj =  paste0("s3://crypto-data-shiny/", to_pull$aws_file_name[i])
  print(paste0("Starting: ", obj))
  
  save_object(object = obj, file = tempfile_15)
  
  print(paste0("Saved: ", obj))
  
  df <- read.csv(tempfile_15)
  print(paste0("Read to  CSV: ", obj))
  
  df$datetime <- as.Date(df$datetime)
  
  df <- df %>% select(-X)
  
  api_request(
    FSYM = to_pull$from_symbol[i], 
    TSYM = to_pull$to_sympol[i], 
    E = to_pull$exchange[i], 
    AGGREGATE = 1, 
    LIMIT = 2000
  ) -> api_results_f_output
  
  process_api_request(API_DATA_RESULT = api_results_f_output) -> raw_data_iter
  
  cols <- c('timestamp',  'close',   'high',    'low',   'open')
  
  historical_df <- 
    raw_data_iter %>% 
    select(all_of(cols)) %>% 
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