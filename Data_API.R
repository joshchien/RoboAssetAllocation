
library(tidyquant)

ETF.asset <- c("VTI","SCHB","VTV","IVE","VOE","IWS","VBR","IWN","VEA","SCHF","VWO","IEMG","SHV","VTIP","MUB","TFI","AGG","BNDX","EMB","VWOB")

#------------------------
TS.from <- "1990-01-01" #
TS.to <- Sys.Date()
TS.Qutoe <- "adjusted"  
#------------------------
TS.yahoo.Dataset <- function(ts.name,ts.from,ts.to,ts.quote){
  
  # Download Asset Data
  TS.Data <- tq_get(ts.name,
                    get  = "stock.prices",
                    from = ts.from,
                    to   = ts.to)
  # Transform timeSeries Object from tbl_df, tibble
  DATE <- TS.Data$date
  Data <-  TS.Data[,-1]
  TS <- timeSeries(Data,DATE)
  Asset <- TS[,ts.quote] # Select "Adjusted" quote / 
  colnames(Asset) <- ts.name
  return(Asset)
}
#------------------------------------------------------------
asset.num <- length(ETF.asset)
for(i in 1:asset.num){
  temp <- TS.yahoo.Dataset(ETF.asset[i],TS.from,TS.to,TS.Qutoe)
  if(i==1){
    myPortfolio = temp
  }else{
    myPortfolio <- cbind(myPortfolio,temp)
  }
}

save(myPortfolio,file="myPortfolio.RData")