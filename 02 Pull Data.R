
source("Master Packages.R")
source("Master Functions.R")

source('01 parameters for api.R')

for(i in 1:nrow(to_pull)){
  
  tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = paste0("s3://crypto-data-shiny/",
                              to_pull$aws_file_name[i]), file = tempfile_15)
  df <- read.csv(tempfile_15)
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
  
  put_object(file = to_pull$aws_file_name[i], 
           object = to_pull$aws_file_name[i],
           bucket = bucket_name)   

  unlink(to_pull$aws_file_name[i])
  
}

