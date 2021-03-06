require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)

# Download some Data, e.g. the CBOE VIX 
getSymbols("SPY",src="yahoo")
spy.return<-dailyReturn(SPY,subset='2014::')

# Make a dataframe
dat<-data.frame(date=index(spy.return),spy.return)
dat$daily.returns<-dat$daily.returns*100

# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
# the month too 
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
# but turn months into ordered facors to control the appearance/ordering in the presentation
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# the day of week is again easily found
dat$weekday = as.POSIXlt(dat$date)$wday
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order

dat$weekdayf<-factor(dat$weekday,levels=(1:7),labels=(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
#dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)

# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
# then find the "week of year" for each day
dat$week <- as.numeric(format(dat$date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
P<-ggplot(dat, aes(weekdayf, monthweek, fill = dat$daily.returns)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + 
  scale_fill_gradient2(low = "red", high = "green") + 
  ggtitle("Time-Series Calendar Heatmap") +
  labs(x="Week of Month", y="")

  
P
head(spy.return)
plot(spy.return)
