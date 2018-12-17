
rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)

tickers <- c("ABB.ST", "ACAN-B.ST", "ASSA-B.ST", "BALCO.ST", "CATE.ST", "DOM.ST", "DUST.ST", 
             "ELAN-B.ST", "HEXA-B.ST", "HOFI.ST", "NOBI.ST", "PNDORA.CO", "PROTCT.OL", "PACT.ST", 
             "SECU-B.ST","STEF-B.ST", "SWOL-B.ST", "TOM.OL", "TROAX.ST")
w <- c(164, 833, 228, 438, 136, 386, 388,
       360, 34, 240, 413, 28, 303,
       214, 225, 944, 252, 104)


fromdate = "2015-06-15"
todate = "2018-09-24"

getSymbols(tickers,from = fromdate,to = todate)

#tickers[2] <- substring(tickers[2], 2)


portf <- cbind(w[1]*na.omit(get(tickers[1])[, 4]))


for (i in 2:length(w)) {
  portf <- cbind(portf, w[i]*na.omit(get(tickers[i])[, 4]))
}


#portf[, 3] <- rowSums(portf[, -c(1, 2)])
portf <- na.approx(portf)
portftot <- rowSums(portf)

weights <- portf[nrow(portf),]/sum(portf[nrow(portf),])

#colnames(portf) <- tickers
#colnames(weights) <- tickers



portf.discrete <- na.omit(ROC(portf,type = "discrete")[-1,])
portftot.discrete <- na.omit(ROC(portftot,type = "discrete")[-1])

corr <- cor(portf.discrete[, 10], portf.discrete[, -10], method = "pearson")
corr <- cor(portf.discrete[, 10], portftot.discrete, method = "pearson")
#corrC <- cor(portf.discrete[, 2], portf.discrete[, -2], method = "pearson")
#corrS <- cor(portf.discrete[, 3], portf.discrete[, -3], method = "pearson")
#corrtot <- cor(portf.discrete[, 1],portftot.discrete ,method = "pearson")
CVaR = ES(portf.discrete,p = 0.95, weights = weights[nrow(weights)], method = "historical",portfolio_method = "component")
CVaRtot = ES(portftot.discrete, p = 0.95, method = "historical")
beta_asset = coef(lm(portf.discrete[, 1] ~ portf.discrete[, 2]))[2]








