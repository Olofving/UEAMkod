getPortfolio <- function(tickers, w, from, to) {
  # Constructs a portfolio of the assets in tickers.
  # The output truncates the portfolio until all assets have market data
  #
  # Param:
  # tickers[1] - asset to compare
  # tickers[2] - comparing index
  # from - Load from earliest this date
  # to - Load to this day at latest
  #
  # Mattias Bertolino Quantitative Risk Analyst Uppsalaekonomerna Asset Management 2016-2017
  
  # Load libraries
  library(quantmod)
  library(ggplot2)
  
  # Load historical prices from Yahoo Finance
  getSymbols(tickers, from = from, to = to, src = 'yahoo', auto.assign = TRUE)
  tickers[2] <- substring(tickers[2], 2) # Remove ^ from Index
  
  # Construct portfolio from invidiual assets
  portf <- cbind(get(tickers[1])[, 4], get(tickers[2])[, 4])
  for (i in 1:length(w)) {
    portf <- cbind(portf, w[i]*na.omit(get(tickers[i+2])[, 4]))
  }
  portf <- na.approx(portf) # Interpolate missing dates
  portf[, 3] <- rowSums(portf[, -c(1, 2)]) # Sum to one portfolio
  portf <- portf[, -c(4:ncol(portf))]
  colnames(portf) <- c(tickers[1], tickers[2], "Portf")
  
  # Start at same date
  portf <- portf[index(portf[complete.cases(portf)])]
  portf <- portf[complete.cases(portf)]
  
  # Normalize
  for (i in 1:ncol(portf)) {
    portf[, i] <- portf[, i]/as.numeric((portf[1, i]))
  }
  
  return(portf)
}
