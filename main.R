
source("UsingLib.R")
source("MPT.R")

#------------------------
rebalance.freq <- 3
optimize.freq <- 3
rolling.window <- 12
#-------------------------
# Equity Class
# VTI : US total Stock Market
# VGK : FTSE Europe Stock Market
# VPL : FTSE Pacific Stock Market (mainly on Japan)
# VWO : FTSE Emerging Stock Market
# Bond Class
# BND : US Total Bond Market
# BWX : International Treasury Market (exclude US)
# EMB : USD Emerging Bond Market
# JNK : High Yield Bond Market
#-------------------------
ETF.asset <- c("VTI","VGK","VPL","VWO","BND","BWX","EMB","JNK")
#ETF.asset <- c("^TWII","VGK","VPL","VTI","BND","TLT")
#Bech.index <- "SPY"  # Benchmark Index Symbol
#attr(ETF.asset,"index_symbol") <- Bech.index
TS.from <- "2000-01-01" #
TS.to <- Sys.Date()
num.bt.dataset <- 10
days.bt.sample <- 252 * 2
mc_rep = 2000 # Number of Monte Carlo Simulations
training_days = 30 
#TS.Qutoe <- "adjusted"  

#Download market data from Finance Yahoo
TS.Data <- stockDataDownload(ETF.asset,from = TS.from, to = TS.to) 

TS.Data <- TS.Data$adjusted
TS.Data <- as.timeSeries(TS.Data)

TS.Rnt <- returns(TS.Data,method="simple",na.rm=T)[-1,]
TS.Rnt <- na.omit(TS.Rnt)

TS.cov <- cov(TS.Rnt)
rpw <- riskParityPortfolio(TS.cov)



# backtesting
bt.dataset.list <- stockDataResample(ETF.dataset, 
                                     N_sample = length(ETF.asset), T_sample = days.bt.sample, 
                                     num_datasets = num.bt.dataset)
