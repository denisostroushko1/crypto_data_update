
symbols = c('BTC', 
            'ETH', 
            'LINK', 
            'ADA', 
            'THETA', 
            'VET')
l = length(symbols)

e = c('Binance', 
      'Binance', 
      'Binance', 
      'Binance', 
      'Binance', 
      'Binance'
      )

aws_file_name <- c(
  "all_available_BTC.csv",
  "all_available_ETH.csv",
  "all_available_LINK.csv",
  "all_available_ADA.csv",
  "all_available_THETA.csv",
  "all_available_VET.csv"
)

to_pull <- 
  data.frame(
    from_symbol = symbols, 
    to_sympol = rep('USDT', l),
    exchange = e,
    aws_file_name
  )
