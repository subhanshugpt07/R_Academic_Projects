







gold <- read.csv("Gold.csv")
View(gold)
ts.gold<-ts(gold)

ts.gold
gold.ts<-ts(gold$Value,start=c(1990,1),frequency = 12)
gold.ts
plot.ts(gold.ts,col="red")

gold.ts.d<-decompose(gold.ts)
plot(gold.ts.d)


oil.ts.d<-decompose(oil.ts)
plot(oil.ts.d)


oil<-read.csv("Oilmon.csv")
View(oil)
oil.ts<-ts(oil$VALUE,start=c(1990,1),frequency = 12)
oil.ts
plot.ts(oil.ts,col="green")






@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  par(mar=c(2, 10, 4, 8) + 0.1)
plot(gold.ts,col="blue",ylim=c(0,2000),axes=F,ylab="")
box()
axis(2,col = "blue")
mtext("gold",side=2,line=3)
par(new=T)
plot(oil.ts,axes=F,col="red",ylab="",ylim=c(0,250))
axis(4,col="red")
mtext("oil",side=4,line = 3)
axis(1,ylim=c(1990,2016))

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  gold.ts.d<-decompose(gold.ts)
plot(gold.ts.d)
oil.ts.d<-decompose(oil.ts)
plot(oil.ts.d)

gold.holt<-HoltWinters(gold$Value,gamma=T)  
oil.holt<-HoltWinters(oil$VALUE,gamma=T)
plot(gold.holt)
plot(oil.holt)  

library(forecast)
oil.for<-forecast.HoltWinters(oil.holt,h=12)
plot(oil.for)
gold.for<-forecast.HoltWinters(gold.holt,h=12)
gold.for
plot.forecast(gold.for)
names(gold.for)
gold.for$residuals
resi<-na.omit(gold.for$residuals)
?acf
acf(resi, lag.max = 60)#residuals are the forecast error which is the difference btwn observed vales minus forecast values 
Box.test(resi, lag=20, type="Ljung-Box")
# null hypthosis : No serial correlation upto 60 lags 
# value of p is less than 0.05 means we can reject the null hypothesis of serial correlation,  
plot.ts(resi)

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  url<-"http://chart.finance.yahoo.com/table.csv?s=^GSPC&a=0&b=1&c=1990&d=9&e=31&f=2016&g=m&ignore=.csv"
sp500new<-read.csv(url,header=T,stringsAsFactors=F)
View(sp500new)
sp500new.ts<-ts(sp500new$Open,start=c(1990),frequency = 12)
plot(sp500new.ts)
sp500new.ts.d<-decompose(sp500new.ts)
plot(sp500new.ts.d)
library(base)
library(zoo)

sp500new.sorted<-sp500new[order(as.Date(sp500new$Date, format="%Y-%m-%d")),] 
#sp500new.sorted<-sp500new[rev(order(as.Date(sp500new$Date))),]  ##################################

sp500new.sorted.ts<-ts(sp500new.sorted$Open,start=c(1990),frequency = 12)
str(sp500new.sorted)
plot(sp500new.sorted.ts)
sp500new.sorted.ts.d<-decompose(sp500new.sorted.ts)
plot(sp500new.sorted.ts.d)


sp500new.holt.T <- HoltWinters(sp500new.sorted.ts, gamma=TRUE)
sp500new.holt.T
sp500new.holt.F <- HoltWinters(sp500new.sorted.ts, gamma=FALSE)

plot(sp500new.holt.T)

plot(sp500new.holt.F)

sp500new.forecasts.T <- forecast.HoltWinters(sp500.holt.T, h=12)  # forecast 1 year (12 months)
sp500new.forecasts.F <- forecast.HoltWinters(sp500.holt.F, h=12)  # forecast 1 year (12 months)


plot.forecast(sp500new.forecasts.T)
plot.forecast(sp500new.forecasts.F)



sp500new.forecasts.F <- forecast.HoltWinters(sp500new.holt.F, h=12)  # forecast 1 year (12 months)
sp500new.forecasts.T <- forecast.HoltWinters(sp500new.holt.T, h=12)  # forecast 1 year (12 months)

plot.forecast(sp500.forecasts.F)
plot.forecast(sp500.forecasts.T)

?corrgram

?diff



#####FOR GOLD-ARIMA#####
plot.ts(gold.ts,col="red")
gold.ts.diff<-diff(gold.ts,differences = 2)
plot.ts(gold.ts.diff)
#d=2

acf(gold.ts.diff,lag.max = 20) # plot a correlogram
acf(gold.ts.diff,lag.max = 20,plot=FALSE) # get the autocorrelation values
#q=3 as it tails of to zero after lag 1 and lag 2
(0,3)

pacf(gold.ts.diff,lag.max = 20) # plot a partial correlogram
pacf(gold.ts.diff,lag.max = 20,plot=FALSE) # get the partial autocorrelation values
#p=5 as it tails of to zero after lag 4
(5,0)

(p,d,q)
(0,2,3)

gold.ts.arima<- arima (gold.ts,order=c(0,2,3))
gold.ts.arima

library(forecast)
gold.ts.arima.for<- forecast.Arima(gold.ts.arima,h=12)
gold.ts.arima.for
plot.forecast(gold.ts.arima.for)

#####FOR OIL-ARIMA#####

plot.ts(oil.ts,col="blue")
oil.ts.diff<-diff(oil.ts,differences = 2)
plot.ts(oil.ts.diff)
#d=2

acf(oil.ts.diff,lag.max = 20) # plot a correlogram
acf(oil.ts.diff,lag.max = 20,plot=FALSE) # get the autocorrelation values
#q=2 as it tails of to zero after lag 1
(0,2)

pacf(oil.ts.diff,lag.max = 20) # plot a partial correlogram
pacf(oil.ts.diff,lag.max = 20,plot=FALSE) # get the partial autocorrelation values
#p=4 as it tails of to zero after lag 3
(4,0)

(p,d,q)
(0,2,2)

oil.ts.arima<- arima (oil.ts,order=c(0,2,2))
oil.ts.arima

library(forecast)
oil.ts.arima.for<- forecast.Arima(oil.ts.arima,h=12)
oil.ts.arima.for
plot.forecast(oil.ts.arima.for)

#####FOR SP500-ARIMA#####
plot.ts(sp500.ts,col="green")
sp500.ts.diff<-diff(sp500.ts,differences = 1)
plot.ts(sp500.ts.diff)
#d=1

acf(sp500.ts.diff,lag.max = 20) # plot a correlogram
acf(sp500.ts.diff,lag.max = 20,plot=FALSE) # get the autocorrelation values
#q=1 as it tails of to zero after lag 0
(0,1)

pacf(sp500.ts.diff,lag.max = 20) # plot a partial correlogram
pacf(sp500.ts.diff,lag.max = 20,plot=FALSE) # get the partial autocorrelation values
#p= as it tails of to zero at lag 0
(0,0)

(p,d,q)
(0,0,0)

sp500.ts.arima<- arima (sp500.ts,order=c(0,0,0))
sp500.ts.arima

library(forecast)
sp500.ts.arima.for<- forecast.Arima(sp500.ts.arima,h=12)
sp500.ts.arima.for
plot.forecast(sp500.ts.arima.for)

