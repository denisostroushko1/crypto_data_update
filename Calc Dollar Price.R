

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

######################################################
# data with pairs is stored in 'to_populate_3'
tempfile_15 <- tempfile() 

obj =  paste0("s3://crypto-data-shiny/", 'all coins historical data.csv')
save_object(object = obj, file = tempfile_15)

major_historical_df3 <- read.csv(tempfile_15)

######################################################


major_historical_df3 %>% 
  select(symbol_from, symbol_to) %>% 
  unique() %>% 
  group_by(symbol_to) %>% 
  summarise(n = n()) %>% 
  arrange(-n, symbol_to) 

major_historical_df3 %>% 
  select(symbol_from, symbol_to) %>% 
  unique() %>% 
  group_by(symbol_to) %>% 
  summarise(n = n()) %>% 
  arrange(-n, symbol_to) %>% 
  ungroup() %>% 
  summarise(sum(n))

major_historical_df3 <- major_historical_df3 %>% filter(!(symbol_to %in% c("EUR", "JPY", "AUD")))

major_historical_df3 %>% 
  select(symbol_to) %>% unique() %>% unlist() -> symbol_to_options 

# of all symbols, which have conversion to USD? 
  ## this table shows that, for example, some coins are exchanged into AUD 
  ## and AUD can be exchanged into USD directly 

major_historical_df3 %>% 
  filter(symbol_from %in% symbol_to_options) %>% 
  select(symbol_from, symbol_to) %>% 
  unique() %>% 
  mutate(direct_usd = I(symbol_to == "USD")) -> bridge_1

bridge_1 %>% filter(direct_usd == T) %>% select(symbol_from) %>% unlist() -> direct_to_usd_conversion 

## this table shows that something like BNB can be exchanged to BTC
## and BTC then has to be exchanged into USD 
## NOTE: of course BNB can be exchanged into USDT, but I do not want to deal with 
##    how the data was collected at the previous step, and would rather find 
##    a solution here 

bridge_1 %>% filter(direct_usd == F) 



######################################################################
### identify all stable coins from the symbol to data 

major_historical_df3 %>% 
  select(symbol_to) %>% unique() %>% unlist() -> symbol_to_options 

major_historical_df3 %>% 
  filter(symbol_from %in% symbol_to_options) %>% 
  select(symbol_from, symbol_to) %>% unique()

major_historical_df3 %>% 
  filter(symbol_from %in% symbol_to_options) %>% 
  group_by(symbol_from) %>% 
  summarise(mean = mean(price), 
            sd = sd(price), 
            n = n()
  ) %>% 
  arrange(sd, symbol_from) %>% 
  filter(0.99 < mean & 1.01 > mean) %>% 
  select(symbol_from) %>% unlist()-> stablecoins

stablecoins <- c("USD", stablecoins)

###### identify all non-stable coins that are used as conversions from some asset 
symbol_to_options <- symbol_to_options[!(symbol_to_options %in% stablecoins)]

results <- vector(mode = "list", length = length(symbol_to_options))

for(i in 1:length(symbol_to_options)){
  
  curr = symbol_to_options[i]
  iter_res = c(curr)
  
  while(curr != "USD"){
    
    major_historical_df3 %>% filter(symbol_from == curr) %>% select(symbol_to) %>% unique() %>% unlist() -> curr
    iter_res <- c(iter_res, curr)
    
  }
  
  names(iter_res) = paste0("to_",1:length(iter_res))
  results[[i]] = iter_res
  
  print(i)
}

### these are all chains of conversion that eventually ends up in the US dollar. 
### it just so happens that they end up in USD, I expect that perhaps some will eventually
### end up in the USDT or something like that 

bind_rows(results)

 ############################################################################################

## EXAMPLE OF A CONVERSION CHAIN 
if(T == F){
  options(scipen = 999)
  rbind(
    major_historical_df3 %>% filter(symbol_to == "DOGE") %>% head(1),
    major_historical_df3 %>% filter(symbol_from == "DOGE") %>% head(1),
    major_historical_df3 %>% filter(symbol_from == "BTC") %>% head(1)
  ) %>% 
    mutate(price = round(price, 6))
}

#### each entry in a list is a chain of conversion 
### each sub entry is a step within a chain 

conversion_res <- vector(mode = "list", length = nrow(bind_rows(results)))

for(i in 1:nrow(bind_rows(results))    ){

  # bind_rows(results)[i, ]
  
  bind_rows(results)[i, ] %>% apply(., 1, function(x){sum(is.na(x))}) -> na_entries 
  bind_rows(results)[i, ] %>% length() -> total_length
  
  loop_over_N = total_length - na_entries - 1 # take away one because one of the entries is the final destination 
  
  conversion_res[[i]] <- vector(mode = "list", length = loop_over_N)
  
  for( j in 1:loop_over_N   ){
    
    major_historical_df3 %>% filter(symbol_from == bind_rows(results)[i, j] %>% unlist() & 
                                      symbol_to == bind_rows(results)[i, (j + 1)]  %>% unlist()
                                    ) %>% 
      select(datetime, price) -> iter_df
    
    iter_df$datetime <- as.Date(iter_df$datetime)
    
    if(j == 1){iter_df <- iter_df %>% mutate(conversion_from = bind_rows(results)[i, 1] %>% unlist()) %>% select(conversion_from, datetime, price)}
    
    conversion_res[[i]][[j]] <- iter_df
  }
  
}

#### process the results 

reduce(conversion_res[[2]], full_join, by = "datetime") -> all_res_i

which(substr(colnames(all_res_i), 1, 5) == "price") -> price_cols_id
colnames(all_res_i)[price_cols_id] -> price_cols

all_res_i %>% 
  select(all_of(price_cols)) %>% 
  apply(., 1, function(x){prod(x)}) -> usd_price

all_res_i$usd_price <- usd_price


