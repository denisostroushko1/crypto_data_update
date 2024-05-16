scrape_200_from_coinmarketcap <- function(year, month, day){
  
  
  month_s <- ifelse(month < 10, paste0("0", month), as.character(month))
  day_s <- ifelse(day < 10, paste0("0", day), as.character(day))
  
 # URL of the first page
  url <- paste0("https://coinmarketcap.com/historical/", year, month_s, day_s, "/")
  ##########
  webpage <- read_html(url)
  
  body_content <- webpage %>%
    html_node("body")
  
  # Get the inner HTML content of the <body> tag
  body_html <- html_text(body_content)
  
  split_strings <- strsplit(body_html, ",")[[1]]
  split_strings <- str_replace_all(split_strings, "\\\\", "")
#      split_strings[997:1035]
  
  ####### ranks 
      indices <- grep("cmcRank", split_strings)
      # Extract the strings with "cmcRank" using the indices
      strings_with_cmcRank <- split_strings[indices]
   
  ####### symbols 
      # it was easy to find all ranks. 
      # assume that all symbols are two positions before ranks 
      indices.symbols <- indices - 2
      strings_with_symbols  <- split_strings[indices.symbols]
      
  ###### names 
      # same here, assume that names are 3 positions before ranks 
      indices.names <- indices - 3
      strings_with_names  <- split_strings[indices.names]
      
  ###### price 
      # need a different approach here 
      
      usd_price_pos <- which(split_strings == '\"USD\":{\"name\":\"2781\"') + 1
      # it looks like USD price us hiding behind a string such as 
      # "\\\"USD\\\":{\\\"name\\\":\\\"2781\\\""
      strings_with_USD_prices  <- split_strings[usd_price_pos]
      
  ##### volume of USD trades is right behind the prices 
      strings_with_USD_volumes  <- split_strings[usd_price_pos + 1]
      
  #### market cap in USD is right behind volumes 
      strings_with_USD_mc  <- split_strings[usd_price_pos + 2]
      
######## put together the data frame 
      data.frame(
        ranks = strings_with_cmcRank, 
        symbols = strings_with_symbols, 
        names = strings_with_names, 
        usd_price = strings_with_USD_prices, 
        usd_vol = strings_with_USD_volumes, 
        usd_mc = strings_with_USD_mc
      ) -> one_date_df

      
f_one_date_df <- 
  one_date_df %>% 
    mutate(
      ranks = as.integer(str_extract(ranks, "\\d+")), 
      
      symbols = substr(symbols, nchar('\"symbol\":\ '), nchar(symbols) ) %>% 
        gsub(pattern = '"', replacement = '', .) %>% 
        gsub(pattern = '\"', replacement = '', .) %>% 
        gsub(pattern = ':', replacement = '', .), 
      
      names = substr(names, nchar('"name:"'), nchar(names) ) %>% 
        gsub(pattern = '"', replacement = "", .) %>% 
        gsub(pattern = ':', replacement = "", .), 
      
      usd_price = substr(usd_price, nchar('"price:"'), nchar(usd_price) ) %>% 
        gsub(pattern = ':', replacement = "", .) %>% 
        as.numeric(), 
      
      usd_vol = substr(usd_vol, nchar('"volume24h:"'), nchar(usd_vol) ) %>% 
        gsub(pattern = ':', replacement = "", .) %>% 
        as.numeric(), 
      
      usd_mc = substr(usd_mc, nchar('"marketCap:"'), nchar(usd_mc) ) %>% 
        gsub(pattern = ':', replacement = "", .) %>% 
        as.numeric(), 
      
      date = paste0(year, "-", month_s, "-", day_s)
    ) 
                
  return(f_one_date_df)
  
}