setSymbolLookup(VIX='yahoo',XVX='yahoo') 
getSymbols(c("^VIX","^VXV"))

Ratio<-Cl(VIX)/Cl(VXV)*100
Ratio1<-(Next(Cl(VIX))/Next(Cl(VXV)))*100
Ratio1<-data.frame(Ratio1)
Change<-Ratio1$Next-Ratio$VIX.Close
Data<-data.frame(Ratio,Change)
colnames(Data)[1] <- "Close"
colnames(Data)[2] <- "Change"
#Ratio<-Ratio["2010/"]

#Data<-VXX["2010/"]
##Data<-Ratio

#CCI13<-CCI(Data[,2:4],n=13)

uEMA15<-EMA(Cl(Data),n = 15, wilder = FALSE) + (EMA(Cl(Data),n = 15, wilder = FALSE) *0.03)
uEMA15c<-(Cl(Data)-uEMA15)/Cl(Data)*100

lEMA15<-EMA(Cl(Data),n = 15, wilder = FALSE) - (EMA(Cl(Data),n = 15, wilder = FALSE) *0.03)
lEMA15c<-(Cl(Data)-lEMA15)/Cl(Data)*100

Indicators<-data.frame(uEMA15c,lEMA15c)
#colnames(Indicators)[2] <- "CCI13"
colnames(Indicators)[1] <- "uEMA15c"
colnames(Indicators)[2] <- "lEMA15c"
Indicators<-round(Indicators,2)
#Indicators<-Indicators[-nrow(Data),]

#Price<-Cl(Data)-Op(Data)
Class<-ifelse(Data$Change > 0 ,"UP","DOWN")
#Class<-Class[-1]

DataSet<-data.frame(Indicators,Class)
DataSet<-na.omit(DataSet)
#colnames(DataSet)[5] <- "Class"

Training<-DataSet[1:1380,];Test<-DataSet[1381:1841,];Validation<-DataSet[1841:2300,]

NB<-naiveBayes(Class ~ lEMA15c + uEMA15c, data=Training)
table(predict(NB,Test,type="class"),Test[,3],dnn=list('predicted','actual'))

TrainingPredictions<-predict(NB,Training,type="class")
TrainingCorrect<-ifelse(TrainingPredictions==Training[,3],"Correct","Incorrect")
TrainingData<-data.frame(Training,TrainingPredictions,TrainingCorrect)

ggplot(TrainingData,aes(x=Ratio,fill=TrainingPredictions))+geom_histogram(binwidth=5,position="fill")+labs(title="Training Predictions: Ratio", x = "VIX/VXV Ratio", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))
ggplot(TrainingData,aes(x=CCI13,fill=TrainingPredictions))+geom_histogram(binwidth=15,position="fill")+labs(title="Training Predictions: CCI", x = "13-Period CCI", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))
ggplot(TrainingData,aes(x=uEMA15c,fill=TrainingPredictions))+geom_histogram(binwidth=1,position="fill")+labs(title="Training Predictions: MAEupper", x = "15-Period MAEupper", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))
ggplot(TrainingData,aes(x=lEMA15c,fill=TrainingPredictions))+geom_histogram(binwidth=1,position="fill")+labs(title="Training Predictions: MAElower", x = "15-Period MAElower", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))


TestPredictions<-predict(NB,Test,type="class")
TestCorrect<-ifelse(TestPredictions==Test[,3],"Correct","Incorrect")
TestData<-data.frame(Test,TestPredictions,TestCorrect)

ggplot(TestData,aes(x=Ratio,fill=TestCorrect))+geom_histogram(binwidth=5,position="fill")+labs(title="Test Accuracy: Ratio", x = "VIX/VXV Ratio", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))
ggplot(TestData,aes(x=CCI13,fill=TestCorrect))+geom_histogram(binwidth=15,position="fill")+labs(title="Test Accuracy: CCI", x = "13-Period CCI", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))
ggplot(TestData,aes(x=uEMA15c,fill=TestCorrect))+geom_histogram(binwidth=1,position="fill")+labs(title="Test Accuracy: MAEupper", x = "15-Period MAEupper", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))
ggplot(TestData,aes(x=lEMA15c,fill=TestCorrect))+geom_histogram(binwidth=1,position="fill")+labs(title="Test Accuracy: MAElower", x = "15-Period MAElower", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))


Long<-which(Validation$uEMA15c < -13 & Validation$uEMA15c < -6)
#Isolating the trades
LongTrades<-Validation[Long,]
#Creating a dataset of those trades
LongAcc<-ifelse(LongTrades[,3]=="UP",1,0)
#Testing the accuracy
(sum(LongAcc)/length(LongAcc))*100
#And our long accuracy
Short<-which(Validation$uEMA15c > 6 & Validation$lEMA15c > 11)
ShortTrades<-Validation[Short,]
ShortAcc<-ifelse(ShortTrades[,3]=="DOWN",1,0)
(sum(ShortAcc)/length(ShortAcc))*100
#Our short accuracy
length(LongAcc)+length(ShortAcc)
#Total number of trades
((sum(ShortAcc)+sum(LongAcc))/(length(LongAcc)+length(ShortAcc)))*100
#Total accuracy