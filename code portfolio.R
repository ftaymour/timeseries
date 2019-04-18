#Required libraries for analysing timesereis data and some other explanatory data analysis
library(astsa)
library(ggplot2)
library(forecast)
library(dynlm)
library(xts)
library(corrplot)
library(forecast)


#Loading airquality Data that is New York Air Quality Measurements (timeseries data May to September 1973)
#viewing head of data
head(airquality)
tail(airquality)

#Creating timesereis for two variables Temp and Wind
## Create a daily index
inds <- seq(as.Date("1973-05-01"), as.Date("1973-09-30"), by = 1)
temp.xts = xts(airquality$Temp, order.by = inds)
wind.xts = xts(airquality$Wind, order.by = inds)


#Ploting timesereis variables (to see visially the stationarity)
autoplot(temp.xts) + ggtitle("Timeseres of New York Tempreture") + xlab("Time(day)") + ylab("Temperature (F)")
autoplot(wind.xts) + ggtitle("Timeseres of New York Windspeed") + xlab("Time(day)") + ylab("Speed (MPH)")

#Decompose into three main components (Decompose function only works with "ts" objects)
temp.ts = ts(airquality$Temp, start=1, frequency = 30)
plot(decompose(temp.ts))

wind.ts = ts(airquality$Wind, start=1, frequency = 30)
plot(decompose(wind.ts))




plot(jj, type="o")

View(jj)

#---------
w=rnorm(500,0,1)
v= filter(w, filter = rep(1/3, 3))
plot.ts(w)
plot.ts (v)

ggplot(data =data.frame(d=w, time= seq(1,length(w)) )) + geom_line(aes(x=time, y=d))
ggplot(data =data.frame(d=v, time= seq(1,length(v)) )) + geom_line(aes(x=time, y=d))

#energy=read.csv(file= "I:/INFO 590- Time Series/project/energydata_complete.csv", header=TRUE, sep=",")
energy=read.csv(file= "C:/Users/taymouri/Desktop/Indiana/Timeseries/Project/energydata_complete.csv", header=TRUE, sep=",")
energy[,2:3]
View(energy)
head(energy)
energy$date
as.POSIXlt(strptime(energy$date, "%Y-%m-%d %H:%M:%S"))
as.ts(energy$date)

energy_z=read.zoo(energy, index.column = 1, format = "")
energy_z
energy_z = as.xts(energy_z)
plot.ts(energy_z$T1)

energy_xts=xts(energy[,-1], order.by =  as.POSIXct(energy$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
head(energy_xts)
tail(energy_xts)
View(energy_xts)
nyears(energy_xts)
nmonths(energy_xts)
ndays(energy_xts)
periodicity(energy_xts)


corrplot(cor(energy_xts), type="upper")
hist(energy_xts[,3])
library(psych)
multi.hist(energy_xts[,-c('rv1', 'rv2')])
energy_xts[,! names(energy_xts) %in% c('rv1', 'rv2')]

decompose(energy_xts[,'RH_2'], type="additive", frequency = 10)
decompose(energy_xts[,13], type="additive")
uu=ts(coredata(energy_xts[,which(names(energy_xts) == "T7")]), start=1, frequency = 10)
plot(decompose(uu))

yy=lowess(energy_xts[,15])
ggplot(data=data.frame(real=coredata(energy_xts[,15])[,1], smooth = yy$y, time= time(energy_xts[,15])))+
  geom_line(aes(x=time, y=real), col="black") + geom_line(aes(x=time, y=smooth), col="red")

head(energy_xts[,15])
head(log(energy_xts[,15]))

T7.ts= ts(coredata(energy_xts[,which(names(energy_xts) == "T7")])[,1], start=1)
time(T7.ts)

T7.log=log(energy_xts[,which(names(energy_xts) == "T7")] )
T7.recip = 1/energy_xts[,which(names(energy_xts) == "T7")]

T7.transform=merge.xts(T7.log, T7.recip, energy_xts[,which(names(energy_xts) == "T7")])
colnames(T7.transform) = c('log', 'reciprocal', 'original')
autoplot(T7.transform)

T7.diff1=diff(energy_xts[,which(names(energy_xts) == "T7")])
autoplot(T7.diff1)+ylab("T7.diff1")


arima(energy_xts[,3], order=c(1,0,1))
auto.arima(energy_xts[,3])
acf(energy_xts[,which(names(energy_xts) == "T7")])
log.T7=log(energy_xts[,which(names(energy_xts) == "T7")])
acf2( log.T7 )

AR1=arima(energy_xts[,which(names(energy_xts) == "T7")], order=c(1,0,0) )
AR1


MA1=arima(energy_xts[,which(names(energy_xts) == "T7")], order=c(0,0,1) )
MA1

ARMA=arima(energy_xts[,which(names(energy_xts) == "T7")], order=c(1,0,1) )
ARMA

ARIMA=arima(energy_xts[,which(names(energy_xts) == "T7")], order=c(1,1,0) )
ARIMA

print(paste("AIC for different models:", "AR:",12, ",MA:",14))
round(AIC(AR1),1)
best=auto.arima(energy_xts[,which(names(energy_xts) == "T7")])
best$fitted

f.12=forecast(best, h=12)
autoplot.zoo(energy_xts[,which(names(energy_xts) == "T7")])+autolayer(f.12$fitted)
autoplot.zoo(f.12)

autoplot(energy_xts[,3])

plot.ts(energy_xts[,'RH_2'])

names(energy_xts)
which(names(energy_xts) == "T7")
ts.plot(energy_xts[,3:5])
frequency(as.ts(energy_xts[,5]))

coredata(energy_xts[,'RH_2'])
seq(1:dim(energy_xts)[1])
uu=ts(coredata(energy_xts[,'T6']), start = 1, end=dim(energy_xts)[1], frequency = 10)
plot(decompose(uu))

t1.ts=ts(energy$T1)
t1.ts
plot(t1.ts)
acf(t1.ts)
acf2(t1.ts)


t1.ts=ts(energy$T2)
t1.ts
plot(t1.ts)
acf(t1.ts)
acf2(t1.ts)

t1.ts=ts(energy$RH_3)
t1.ts
plot(t1.ts)
smooth.lowess=lowess(t1.ts)
smooth.kernel=ksmooth(x=time(t1.ts), y=t1.ts, kernel="normal", bandwidth = 100)
points(x=smooth.lowess$x, y=smooth.lowess$y, col="red")
lines(x=smooth.kernel$x, y=smooth.kernel$y, col="blue")

acf(t1.ts)
acf2(t1.ts)

