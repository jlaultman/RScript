library('highfrequency')
library('chron')

data<-xts_data
data['2015-07-01 09:35:00/']
plot(data['2015-07-01 09:35:00/'])

data("sample_real5minprices")
plot(sample_real5minprices) 
#compute and plot intraday periodicity
out = spotvol(sample_real5minprices,P1=6,P2=4,periodicvol="TML",k=5, dummies=FALSE);

head(out);
plot(out)
