x    <- rnorm(1000)
y    <- (x - 2) + rnorm(1000)
outR <- lm(y ~ x)
summary(outR)

res <- outR$residuals
par(mfrow = c(2,1))
plot(res, col = "blue", main = "Residual Plot")
acf(res)

#Utilize quantmod to load the security symbols
require(quantmod)
symbols <- c("SPY", "DIA")
getSymbols(symbols)

#define training set
startT  <- "2010-01-01"
endT    <- "2013-01-01"
rangeT  <- paste(startT,"::",endT,sep ="")
tSPY   <- SPY[,6][rangeT]
tDIA   <- DIA[,6][rangeT]

#define out of sample set
startO  <- "2013-02-01"
endO    <- "2015-12-01"
rangeO  <- paste(startO,"::",endO,sep ="")
oSPY   <- SPY[,6][rangeO]
oDIA   <- DIA[,6][rangeO]

#compute price differences on in-sample data
pdtSPY <- dailyReturn(tSPY)[-1]
#pdtSPY <- diff(tSPY)[-1]
#pdtDIA <- diff(tDIA)[-1]
pdtDIA <- dailyReturn(tDIA)[-1]

#build the model
model  <- lm(pdtSPY ~ pdtDIA - 1)

#extract the hedge ratio
hr     <- as.numeric(model$coefficients[1])
                    
#spread price (in-sample)
spreadT <- tSPY - hr * tDIA
                    
#compute statistics of the spread
meanT    <- as.numeric(mean(spreadT,na.rm=TRUE))
sdT      <- as.numeric(sd(spreadT,na.rm=TRUE))
upperThr <- meanT + 1 * sdT
lowerThr <- meanT - 1 * sdT
                    
#visualize the in-sample spread + stats
plot(spreadT, main = "SPY vs. DIA spread (in-sample period)")
abline(h = meanT, col = "red", lwd =2)
abline(h = meanT + 1 * sdT, col = "blue", lwd=2)
abline(h = meanT - 1 * sdT, col = "blue", lwd=2)

hist(spreadT, col = "blue", breaks = 100, main = "Spread Histogram (SPY vs. DIA)")
abline(v = meanT, col = "red", lwd = 2)

indSell <- which(spreadT >= meanT + sdT)
indBuy  <- which(spreadT <= meanT - sdT)

spreadL  <- length(spreadT)
pricesB  <- c(rep(NA,spreadL))
pricesS  <- c(rep(NA,spreadL))
sp       <- as.numeric(spreadT)
tradeQty <- 100
totalP   <- 0

for(i in 1:spreadL) {
  spTemp <- sp[i]
  if(spTemp < lowerThr) {
    if(totalP <= 0){
      totalP     <- totalP + tradeQty
      pricesB[i] <- spTemp
    }
  } else if(spTemp > upperThr) {
    if(totalP >= 0){
      totalP <- totalP - tradeQty
      pricesS[i] <- spTemp
    }
  }
}

plot(spreadT, main = "SPY vs. DIA spread (in-sample period)")
abline(h = meanT, col = "red", lwd =2)
abline(h = meanT + 1 * sdT, col = "blue", lwd = 2)
abline(h = meanT - 1 * sdT, col = "blue", lwd = 2)
points(xts(pricesB,index(spreadT)), col="green", cex=1.9, pch=19)
points(xts(pricesS,index(spreadT)), col="red", cex=1.9, pch=19)

#Start Out of Sample Period
###
###
#spread price (out-sample)
spreadO <- oSPY - hr * oDIA

#compute statistics of the spread
meanO    <- as.numeric(mean(spreadO,na.rm=TRUE))
sdO      <- as.numeric(sd(spreadO,na.rm=TRUE))
upperThrO <- meanO + 1 * sdO
lowerThrO <- meanO - 1 * sdO

#visualize the in-sample spread + stats
plot(spreadO, main = "SPY vs. DIA spread (Out-of-sample period)")
abline(h = meanO, col = "red", lwd =2)
abline(h = meanO + 1 * sdO, col = "blue", lwd=2)
abline(h = meanO - 1 * sdO, col = "blue", lwd=2)

hist(spreadO, col = "blue", breaks = 100, main = "Spread Histogram (SPY vs. DIA)")
abline(v = meanO, col = "red", lwd = 2)

indSell <- which(spreadO >= meant + sdt)
indBuy  <- which(spreadO <= meant - sdt)

spreadL  <- length(spreadO)
pricesB  <- c(rep(NA,spreadL))
pricesS  <- c(rep(NA,spreadL))
sp       <- as.numeric(spreadO)
tradeQty <- 100
totalP   <- 0

for(i in 1:spreadL) {
  spTemp <- sp[i]
  if(spTemp < lowerThr) {
    if(totalP <= 0){
      totalP     <- totalP + tradeQty
      pricesB[i] <- spTemp
    }
  } else if(spTemp > upperThr) {
    if(totalP >= 0){
      totalP <- totalP - tradeQty
      pricesS[i] <- spTemp
    }
  }
}

plot(spreadO, main = "SPY vs. DIA spread (out-of-sample period)")
abline(h = meanO, col = "red", lwd =2)
abline(h = meanO + 1 * sdO, col = "blue", lwd = 2)
abline(h = meanO - 1 * sdO, col = "blue", lwd = 2)
points(xts(pricesB,index(spreadO)), col="green", cex=1.9, pch=19)
points(xts(pricesS,index(spreadO)), col="red", cex=1.9, pch=19)
                    