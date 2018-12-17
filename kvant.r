# Compare potential asset to portfolio
#
# This script is made to facilitate the comparison between a potential
# asset and the existing portfolio.
#
# Mattias Bertolino Quantitative Risk Analyst Uppsalaekonomerna Asset Management 2016-2017
# Edits by Olof L?fving, Quantitative Risk Analyst Uppsalaekonomerna Asset Management 2017-2018

rm(list = ls()) #clear workspace

# Help functions
colMax <- function(data) sapply(data, max, na.rm = TRUE)

# Load packages
setwd("~/GitHub/UEAMkod")
source("getPortfolio.r")
library(xts)
library(normtest) # Jarque-Bera Normality test
library(PerformanceAnalytics) # CVaR
library(ggplot2)

# Assets to analyze
# tickers[1] - asset to compare
# tickers[2] - index to compare
# tickers[3:end] - portfolio assets
tickers <- c("NOBI.ST", "^OMXSPI", "ABB.ST", "ACAN-B.ST", "ASSA-B.ST", "BALCO.ST", "CATE.ST", "DOM.ST", "DUST.ST", 
             "ELAN-B.ST", "HEXA-B.ST", "HOFI.ST", "NOBI.ST", "PNDORA.CO", "PROTCT.OL", "PACT.ST", "SBB-B.ST", 
             "SECU-B.ST","STEF-B.ST", "SWOL-B.ST", "TOM.OL", "TROAX.ST")
w <- c(120, 833, 228, 438, 136, 386, 388,
       360, 34, 240, 413, 28, 303, 170, 2014,
       214, 225, 944, 252, 104)


# Load portfolio and calculate portfolio return
portf <- getPortfolio(tickers, w, from = "2015-06-15", to = "2018-12-14")
ret_portf <- na.omit(ROC(portf, type = 'discrete'))
# Compute some tests
for (i in 1:ncol(portf)) {
  #Compute Box-Pierce test to see if returns are serially correlated
  print(names(portf[, 1]))
  print(Box.test(ret_portf[, i], lag = 1)[3])

  # Decorrelating smoothing filter
  serial <- lm(ret_portf[2:nrow(ret_portf), i] ~ ret_portf[1:nrow(ret_portf) - 1, i], data = ret_portf)$coefficients[2]
  ret_portf[2:(nrow(ret_portf) - 1), i] <- (ret_portf[2:nrow(ret_portf), i] - serial*ret_portf[1:(nrow(ret_portf) - 1), i])/(1 - serial)

  # Compute Jarque-Bera normality test (Normal )
  print(jb.norm.test(ret_portf[, i], nrepl = 1000)[2])
}

# Test correlation under market stress
from = 30
to = 52
corr_long <- cor(ret_portf[, 1], ret_portf[, 3], method = "pearson")
corr_stress <- cor(ret_portf[from:to, 1], ret_portf[from:to, 3], method = "pearson")
plot(na.omit(rollapply(ret_portf[, 1], width = 30, FUN = sd, fill = NA)*sqrt(252)))

# Key
ev <- colMeans(ret_portf)*252
stddev_long <- apply(ret_portf, 2, sd)*sqrt(252)
#stddev_stress <- colMax(na.omit(rollapply(ret_portf, width = 30, FUN = sd, fill = NA)*sqrt(252)))
beta_asset_long = coef(lm(ret_portf[, 1] ~ ret_portf[, 2]))[2]
#beta_asset_stress = coef(lm(ret_portf[30:52, 1] ~ ret_portf[30:52, 2]))[2]
beta_portf_long = coef(lm(ret_portf[, 3] ~ ret_portf[, 2]))[2]
#beta_portf_stress = coef(lm(ret_portf[30:52, 3] ~ ret_portf[30:52, 2]))[2]
riskfree_rate = 0.01
shratio <- (ev-riskfree_rate)/stddev_long
cvar = ES(ret_portf, p = 0.95)
cvarHistorical = ES(ret_portf, p = 0.95, method = "historical")

##################################################
# Plots
#################################################

# Plot histogram
qplot(as.vector(ret_portf[, 1]), geom = 'blank',
      main = paste0("Histogram for ", tickers[1], " returns"),
      xlab = "Returns",
      ylab = "Density",
      xlim = c(-0.1, 0.15)) +  
  geom_line(aes(y = ..density.., colour = 'Empirical'),
            stat = 'density',
            alpha = 1,
            col = I("blue")) + 
  geom_histogram(aes(y = ..density..), 
                 alpha = 0.2,
                 fill = I("green"),
                 col = I("green"),
                 bins = 50) +
  stat_function(fun = dnorm,
                color = "red",
                args = list(mean = mean(ret_portf[, 1]), 
                            sd = sd(ret_portf[, 1]))) +
  theme(legend.position = c(0.85, 0.85),
        panel.background = element_rect(fill = 'white', colour = 'black'))



