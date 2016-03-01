install.packages('highfrequency')
library('highfrequency')

data(realized_library)

data <- read.csv(file = "T:/DERIV/Aultman/Projects/Data Sets/OxfordManRealizedVolatilityIndices.csv",
                  stringsAsFactors=FALSE, header=TRUE, na.strings=c("", "NA"))

data <- data.frame(data$X,data$S.P.500..Live.)
data <- data[-(1:2),]
RV <- data.frame(data[,2])
date.chr <- as.character(data[,1])
date <- strptime(date.chr,format="%Y%m%d")
rv_ts <- data.frame(RV,row.names=date)
rv_xts <- as.xts(rv_ts)
rv_xts <- rv_xts[!is.na(rv_xts)]
rv_xts <- rv_xts['2015']
plot(rv_xts)

DJI_RV = realized_library$Dow.Jones.Industrials.Realized.Variance; #Select DJI
DJI_RV = DJI_RV[!is.na(DJI_RV)]; #Remove NA's
DJI_RV = DJI_RV['2008'];
plot(DJI_RV)

x = harModel(data=rv_xts , periods = c(1,5,22), RVest = c("rCov"), type="HARRV",h=1,transform=NULL);
summary(x)
plot(x)

