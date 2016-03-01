require('quantmod')
SPY <- read.csv(file = "Documents/Data Files/SPY 5 Min OHLCV.csv", stringsAsFactors=FALSE) 
TICK <- read.csv(file = "Documents/Data Files/TICK 5 Min OHLC.csv", stringsAsFactors=FALSE) 
QCHA <- read.csv(file = "Documents/Data Files/QCHA 5 Min OHLC.csv", stringsAsFactors=FALSE)

DateTime_SPY<-as.POSIXlt(SPY[,1], format="%m/%d/%y %H:%M") 
DateTime_TICK<-as.POSIXlt(TICK[,1], format="%m/%d/%y %H:%M") 
DateTime_QCHA<-as.POSIXlt(QCHA[,1], format="%m/%d/%y %H:%M")

SPY <- SPY[c(2:6)]
TICK <- TICK[c(2:5)]
QCHA <- QCHA[c(2:5)]

SPYts <- data.frame(SPY,row.names = DateTime_SPY) 
TICKts <- data.frame(TICK,row.names = DateTime_TICK) 
QCHAts <- data.frame(QCHA,row.names = DateTime_QCHA)

SPYxts <- as.xts(SPYts)
TICKxts <- as.xts(TICKts)
QCHAxts <- as.xts(QCHAts)

data <- merge(SPYxts, TICKxts, QCHAxts)
colnames(data) <- c("SPY Open","SPY Hi","SPY Lo","SPY Close","SPY Volume","TICK Open","TICK Hi","TICK Lo","TICK Close","QCHA Open","QCHA Hi","QCHA Lo","QCHA Close")
chartSeries(SPYxts,subset='2015-05-01')
