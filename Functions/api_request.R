
api_request <- function(
    FSYM, 
    TSYM, 
    E, 
    AGGREGATE, 
    LIMIT
){
  
  url <- "https://min-api.cryptocompare.com/data/histoday"
  
  params <- list(
    fsym = FSYM,
    tsym = TSYM,
    e = E, 
    aggregate = AGGREGATE,
    limit = LIMIT
  )
    
  # Make GET request to the API
  response <- GET(url, query = params)
  
  # Parse JSON content
  data <- content(response, "parsed")
  
  # Access the relevant data (assuming the data is nested under the "Data" key)
  historical_data <- data$Data

  return(historical_data)
  
}
  


# Make GET request to the API
