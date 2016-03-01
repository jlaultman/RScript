#new stuff
library(quantmod)
library(e1071)
library(ggplot2)

# load historical data, getSymbols from quantmod
getSymbols("SPY")
#SPY<-SPY['2010:']
#chartSeries(SPY)

#Data<-USDCAD
Data<-data.frame(date=index(SPYxts), coredata(SPYxts))
colnames(Data)[5]<-"Close"
#Our dataset
CCI20xts<-CCI(SPYxts[,3:5,n=20])
CCI20<-CCI(Data[,3:5],n=20)
#A 20-period Commodity Channel Index calculated of the High/Low/Close of our data
chartSeries(CCI20xts)

RSI3<-RSI(Cl(Data),n=3)
#A 3-period RSI calculated off the close
chartSeries(RSI3)

DEMA10<-DEMA(Cl(Data),n = 10, v = 1, wilder = FALSE)
DEMA10c<-Cl(Data) - DEMA10
#A 10-period Double Exponential Moving Average (DEMA), with standard parameters. And we will be looking at the difference between the price and the DEMA.
DEMA10c<-DEMA10c/.0001
#Convert into pips

Indicators<-data.frame(RSI3,DEMA10c,CCI20)
Indicators<-round(Indicators,2)
Indicators<-Indicators[-nrow(Data),]
#Removing the most recent data point

Price<-Cl(Data)-Op(Data)
Class<-ifelse(Price > 0 ,"UP","DOWN")
Class<-Class[-1]
#Remove the oldest data point to match up our predicted Class with the indicators.

DataSet<-data.frame(Indicators,Class)
DataSet<-DataSet[-c(1:19),]
#Remove the instances where the indicators are still being calculated.

Training<-DataSet[1:6000,];Test<-DataSet[6001:8000,];Validation<-DataSet[8001:10600,]
#Separate into a training set (60% of the data), test set (20% of the data), and validation set (20% of the data).


NB<-naiveBayes(Class ~ RSI3 + CCI20 + DEMA10c, data=Training)
#Using our three technical indicators to predict the class off the training set

table(predict(NB,Test,type="class"),Test[,4],dnn=list('predicted','actual'))

TrainingPredictions<-predict(NB,Training,type="class")
#Get a list of all our predictions
TrainingCorrect<-ifelse(TrainingPredictions==Training[,4],"Correct","Incorrect")
#See if these predictions are correct
TrainingData<-data.frame(Training,TrainingPredictions,TrainingCorrect)
#Build one data set we can use for all of our plots

ggplot(TrainingData,aes(x=RSI3,fill=TrainingPredictions))+geom_histogram(binwidth=15,position="fill")+labs(title="Training Predictions: RSI", x = "3-Period RSI", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))
ggplot(TrainingData,aes(x=CCI20,fill=TrainingPredictions))+geom_histogram(binwidth=15,position="fill")+labs(title="Training Predictions: CCI", x = "20-Period CCI", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))
ggplot(TrainingData,aes(x=DEMA10c,fill=TrainingPredictions))+geom_histogram(binwidth=15,position="fill")+labs(title="Training Predictions: DEMA", x = "10-Period DEMA", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))


TestPredictions<-predict(NB,Test,type="class")
#Get a list of all our predictions
TestCorrect<-ifelse(TestPredictions==Test[,4],"Correct","Incorrect")
#See if these predictions are correct
TestData<-data.frame(Test,TestPredictions,TestCorrect)
#Build one data set we can use for all of our plots

ggplot(TestData,aes(x=RSI3,fill=TestCorrect))+geom_histogram(binwidth=5,position="fill")+labs(title="Test Accuracy: RSI", x = "3-Period RSI", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))
ggplot(TestData,aes(x=CCI20,fill=TestCorrect))+geom_histogram(binwidth=5,position="fill")+labs(title="Test Accuracy: CCI", x = "20-Period CCI", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))
ggplot(TestData,aes(x=DEMA10c,fill=TestCorrect))+geom_histogram(binwidth=5,position="fill")+labs(title="Test Accuracy: DEMA10", x = "10-Period DEMA", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))



Long<-which(Validation$RSI3 < 30 & Validation$CCI > -290 & Validation$CCI < -100 & Validation$DEMA10c > -40 & Validation$DEMA10c < -20)
#Isolating the trades
LongTrades<-Validation[Long,]
#Creating a dataset of those trades
LongAcc<-ifelse(LongTrades[,4]=="UP",1,0)
#Testing the accuracy
(sum(LongAcc)/length(LongAcc))*100
#And our long accuracy
Short<-which(Validation$DEMA10c > 10 & Validation$DEMA10c < 40 & Validation$CCI > 185 & Validation$CCI < 325 & Validation$RSI3 > 50)
ShortTrades<-Validation[Short,]
ShortAcc<-ifelse(ShortTrades[,4]=="DOWN",1,0)
(sum(ShortAcc)/length(ShortAcc))*100
#Our short accuracy
length(LongAcc)+length(ShortAcc)
#Total number of trades
((sum(ShortAcc)+sum(LongAcc))/(length(LongAcc)+length(ShortAcc)))*100
#Total accuracy
