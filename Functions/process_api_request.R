
process_api_request <- function(
    API_DATA_RESULT
){
  historical_df <- as.data.frame(do.call(rbind, API_DATA_RESULT))
  historical_df <- data.frame(historical_df)
  
  historical_df$timestamp <- as.POSIXct(historical_df$time %>% unlist(), origin = "1970-01-01", tz = "UTC")
  
  one_col <- data.frame(droppy = rep(NA, nrow(historical_df)))
  
  for(i in 1:length(historical_df)){
    one_col <- cbind(one_col, 
                     historical_df[,i] %>% unlist())
    
  }
  
  one_col <- one_col[,-1]
  colnames(one_col) <- colnames(historical_df)
  historical_df <- one_col
  
  return(historical_df)

}