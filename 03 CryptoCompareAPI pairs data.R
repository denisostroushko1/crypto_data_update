
source("Master Packages.R")


#  url <- "https://min-api.cryptocompare.com/data/histoday"

# https://min-api.cryptocompare.com/documentation

# https://min-api.cryptocompare.com/documentation?key=Other&cat=allExchangesV4Endpoint
api_r <- GET("https://min-api.cryptocompare.com/data/v4/all/exchanges")


data <- content(api_r, "parsed")

if(length(data$Data$exchanges) == 0){
  print(paste0("WARNING: ", data$Message)); 
  
  print(paste0("Calls made: ", data$RateLimit$calls_made$total_calls, "; ", 
               data$RateLimit$calls_made$total_calls - data$RateLimit$max_calls$day, " over the limit"
  ))
}


if(length(data$Data$exchanges) != 0){
  # this is our data 
  DATA <- data$Data$exchanges
  
  # encahnge
    # i = 16 # Binance
  
  # BNB on binance
    # j =  1   
  
  # BNB to DAI on Binance 
    # k = 1 
  
  to_populate <- data.frame(
    histo_minute_start = Date(), 
    histo_minute_end = Date(),
    isActive = logical(), 
    exchange = character(), 
    symbol_from = character(), 
    symbol_to = character()
  )
  
  exchange_names <- names(data$Data$exchanges)
  passes = 1
  
  for(i in 1:length(data$Data$exchanges)){
    
    pairs_names <- names(data$Data$exchanges[[i]]$pairs)
    print(paste0("On exchange ", exchange_names[i], ". ", i, " of ", length(exchange_names)))
    
    # if an encahnge has no pairs data, just skip it 
    if(length(pairs_names) != 0){
      # these are all pairs available on Exchange 'i'
      for(j in 1:length(pairs_names)){
        
        available_symbols <- names(data$Data$exchanges[[i]]$pairs[[j]]$tsyms)
        
        for(k in 1:length(available_symbols)){
    
          data$Data$exchanges[[i]]$pairs[[j]]$tsyms[[k]] %>% bind_rows() %>% 
            select(-histo_minute_start_ts, -histo_minute_end_ts) -> DF_ijk 
          
          DF_ijk$exchange =    exchange_names[i]
          DF_ijk$symbol_from = pairs_names[j]
          DF_ijk$symbol_to =   available_symbols[k]
        
          to_populate <- rbind(to_populate, DF_ijk)
          
          # count passes through the loops 
          passes = passes + 1
          
          }
        
      }
    }
  
  }
  
  to_populate <- 
    to_populate %>% 
    mutate(
      histo_minute_end = as.Date(histo_minute_end), 
      histo_minute_start = as.Date(histo_minute_start), 
      
      data_days_amount = as.numeric(histo_minute_end - histo_minute_start)
    )
  
  # I want "TO" symbols to be USD~adjacent things first, and everything else after that 
    to_populate$symbol_to %>% unique() %>% {substr(., 1, 3)} %>% {which(. == "USD")} -> usd_adj_to_symbols_ids 
    usd_adj_to_symbols <- unique(to_populate$symbol_to)[usd_adj_to_symbols_ids] %>% sort()
    other_symbols <- setdiff(unique(to_populate$symbol_to), usd_adj_to_symbols) %>% sort()
    order_symbols <- c(usd_adj_to_symbols, other_symbols)
    
    to_populate$symbol_to <- factor(to_populate$symbol_to, levels = order_symbols)
  
  ####################
  View(to_populate)
  
  ####################
  # OBJECTIVE: I want to have one row per coin, whichever exchange it comes from. 
  #     ideally, it will be measured in USD or USDT, or something like that 
  #     this exchange should have the highest possible amount of historcial days 
  
  to_populate <- 
    to_populate %>% 
    arrange(symbol_from, -data_days_amount, symbol_to)
  
  # some coins have "TO" symbols that probably do not exist in our data 
  # prevents us from being able to convert prices to USD 
  # so, filter out those paits where "TO" symbols is not contained in the "FROM" symbol set 
  
  to_populate$symbol_from %>% unique() %>% sort() -> all_from_symbols
  
  to_populate_2 <- 
    to_populate %>% filter(symbol_to %in% all_from_symbols)
  
  print(paste0("NROW before filtering out symbols: ", prettyNum(nrow(to_populate), big.mark = ",")))
  print(paste0("NROW after filtering out symbols: ", prettyNum(nrow(to_populate_2), big.mark = ",")))
  
  print(paste0("Unique assets before filtering: ", prettyNum(length(unique(to_populate$symbol_from)), big.mark = ",")))
  print(paste0("Unique assets after filtering: ", prettyNum(length(unique(to_populate_2$symbol_from)), big.mark = ",")))
  
  # subset the data such that we have only 1 asset record in the data set 
  
  to_populate_3 <- 
    to_populate_2 %>% 
    group_by(symbol_from) %>% 
    mutate(row_id = 1:n()) %>% 
    ungroup() %>% 
    filter(row_id == 1)
  
  print(paste0("NROW after selecting one asset-row: ", prettyNum(nrow(to_populate_3), big.mark = ",")))
  print(paste0("Unique assets after selecting one asset-row: ", prettyNum(length(unique(to_populate_3$symbol_from)), big.mark = ",")))
  
  prettyNum(length(unique(to_populate_3$symbol_to)), big.mark = ",")
  
  to_populate_3 %>% 
    group_by(symbol_to) %>% 
    summarise(n = n()) %>% 
    arrange(-n)
  
  print(paste0("Unique assets after selecting one asset-row: ", prettyNum(length(unique(to_populate_3$symbol_from)), big.mark = ",")))
  
  # so this is the list of pairs that we need to collect and process 
  
  to_populate_3
  
  table(I(to_populate_3$data_days_amount > 2000))
  
  to_populate_3 %>% 
    filter(data_days_amount > 2000) %>% 
    select(symbol_from, data_days_amount) %>% View()
  
  #     colnames(to_populate_3)
  #     "histo_minute_start" "histo_minute_end"   "isActive"           "exchange"           "symbol_from"        "symbol_to"          "data_days_amount"   "row_id"        
}