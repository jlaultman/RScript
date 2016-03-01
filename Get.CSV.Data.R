raw_data <- read.csv(file = "T:/DERIV/Aultman/Projects/Data Sets/SPX_5m.csv",  stringsAsFactors=FALSE)
DateTime <- as.POSIXlt(raw_data[,1], format='%m/%d/%y %H:%M')
x <- raw_data[,2]
xts_data<- as.xts(data.frame(x,row.names=DateTime))

plot(xts_data)

