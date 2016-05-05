rm(list=ls())

processCSV <- function(df,year){
  newDF = df[,c('UniqueCarrier','ArrDelay','DepDelay')]
  newDFMean <- aggregate(cbind(ArrDelay, DepDelay)~UniqueCarrier,
                        data = newDF,
                        FUN = max)
  newDFMean$Year <- rep(year, dim(newDFMean)[1])
  carrierList = list('AA', 'AS','CO','DL','NW','UA','US','WN')
  newDFMean <- newDFMean[newDFMean$UniqueCarrier %in% carrierList,]
  
  return (newDFMean)
}

processYear <- function(year){
  filename = paste(as.character(year),'.csv', sep = '', collapse = NULL)
  tempDF <-read.csv(filename, header = T, sep=',')
  tempDFNew <- processCSV(tempDF, year)
  rm(tempDF)
  return (tempDFNew)
}

dfTotal <- data.frame()

for (i in seq(1987,2008)){
  print (i)
  df <- processYear(i)
  dfTotal <- rbind(dfTotal, df)
}

write.csv(dfTotal, file = 'YearlyMaxTotal.csv', row.names = F)
