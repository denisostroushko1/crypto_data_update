

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

##########################################################################################
##########################################################################################

starting_date <- as.Date("2013-04-28")
last_date <- Sys.Date()
# there should be this many weeks 
number_of_weeks <- ceiling((last_date - starting_date)/7) 

data_to_collect <- starting_date + (0:number_of_weeks)*7

data_to_collect <- data_to_collect[data_to_collect<Sys.Date()]

f_df <- 
  data.frame(
    ranks = rep(NA, 1), 
    symbols = NA, 
    names = NA, 
    usd_price = NA, 
    usd_vol = NA, 
    usd_mc =  NA, 
    date = NA
  )

#   i = 1

start_time = Sys.time()
for(i in 1:length(data_to_collect)){
  
  Sys.sleep(3)
  
  if(i %% 25 == 0){
    print(paste0(i, " of ", length(data_to_collect))); 
    print(Sys.time() - start_time) 
    }
  
  year_input <-  year(data_to_collect[i])
  month_input <- month(data_to_collect[i])
  day_input <-   day(data_to_collect[i])
  
  iter_df <- scrape_200_from_coinmarketcap(year_input, month_input, day_input)
  
  rows_factor <- ifelse(nrow(iter_df) < 200, nrow(iter_df), 200)
  
  f_df <- rbind(f_df, scrape_200_from_coinmarketcap(year_input, month_input, day_input))

}

f_df <- na.omit(f_df)

beepr::beep(3)

write.csv(f_df, "./Data/master_CMC_scraped.csv")

source("keys.R")  

put_object(file = "./Data/master_CMC_scraped.csv", 
           object = "master_CMC_scraped.csv",
           bucket = bucket_name)  