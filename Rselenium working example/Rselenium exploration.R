
# java -jar selenium-server-standalone-3.5.3.jar -port 4444




# https://www.scrapingbee.com/blog/getting-started-with-rselenium/
  
# 1) you need to have a working Selenium server to use it.
# 2) make sure you have created a new R project
# 3) Since Selenium runs on Java, make sure you have it installed
# 4) Next, download a standalone version of Selenium, and put it in your R project folder.
# 5) Then, download ChromeDriver and also put it in the R project folder.
#     https://letzdotesting.com/download-chromedriver-for-mac/
# 
# 6) 
  

library(RSelenium)
library(rvest)
library(tidyverse)


# java -jar selenium-server-standalone-x.x.x.jar -port 4444
#         must have selenium-server-standalone-3.5.3.jar in the R folder 
#         run java -jar .. in the Rstudio terminal and wait for the program to install 

# before running this, manually open chromedriver in the terminal 

rD <- rsDriver(browser = "chrome", port = 4444L, chromever = "latest")
remDr <- rD[["client"]]

url <- "https://coinmarketcap.com/historical/20240512/"
remDr$navigate(url)

#Step 4: Click the "Load More" Button
# Use a loop to click the "Load More" button until all data is loaded. 
# Check if the button is present and click it until it disappears or is no longer clickable.
system.time({
  
  print("STEP 1")
  
  Sys.sleep(5) # Wait for the page to load initially
  
  # Function to check if the "Load More" button is present
  
  # Function to check if the "Load More" button is present
  
  BUTTON = '//button[contains(text(), "Load More")]'
  
  load_more_button_exists <- function() {
    tryCatch({
      remDr$findElement(using = 'xpath', value = BUTTON)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  # this needs to be timed 
  
  print("STEP 2")
    # Click the "Load More" button until it no longer exists
    while(load_more_button_exists()) {
      button <- remDr$findElement(using = 'xpath', value = BUTTON)
      button$clickElement()
      Sys.sleep(3) # Wait for new data to load
    }
    
  print("STEP 3")
  page_source_start <- remDr$getPageSource()
  
  page_source <- page_source_start[[1]]
  beepr::beep(5)

  print("FINISHED")
  })
   #  user   system  elapsed 
   # 4.966    4.676 1039.279  



  split_strings <- strsplit(page_source, ",")[[1]]
  split_strings <- str_replace_all(split_strings, "\\\\", "")

#      split_strings[997:1035]
  
  ####### ranks 
      indices <- grep("cmcRank", split_strings)
      length(indices)
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


page_source
