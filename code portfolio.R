#Required libraries for analysing timesereis data and some other explanatory data analysis
library(astsa)
library(tseries)
library(zoo)
library(ggplot2)
library(forecast)
library(dynlm)
library(xts)
library(corrplot)
library(forecast)

###################################  Reading and Basic Manipulation  #############################

#Loading airquality Data that is New York Air Quality Measurements (timeseries data May to September 1973)
data("airquality")
#viewing head of data
head(airquality)
tail(airquality)

#Creating timesereis for two variables Temp and Wind
## Create a daily index
inds <- seq(as.Date("1973-05-01"), as.Date("1973-09-30"), by = 1)
temp.xts = xts(airquality$Temp, order.by = inds , frequency = 30)
wind.xts = xts(airquality$Wind, order.by = inds, frequency = 30)
ozone.xts = xts(airquality$Ozone, order.by = inds, frequency = 30)

#Checking start and End dates, and the frequency of the timeseries
start(temp.xts)
end(temp.xts)
frequency(temp.xts)

#Filling missing values by two methods, Last Observation Carried Forward (LOCF) and interpolation
#(Manipulating Time Series Data in R with xts & zoo, Chapter3, DataCamp)
ozone.filled.xts = na.locf(ozone.xts, fromLast = TRUE)
ozone.filled.xts = na.approx(ozone.xts)


#Creating multiple timeseres (Manipulating Time Series Data in R with xts & zoo, Chapter3, DataCamp)
tot.xts = merge(temp.xts, wind.xts, ozone.filled.xts)
tot.xts

#Selecting a window of timeseres, (Manipulating Time Series Data in R with xts & zoo, Chapter2, DataCamp)
tot.xts['1973-05/',]           #selecting only for May
tot.xts['1973-05/1973-06',]   #selecting only for May and June
tot.xts[index(tot.xts)>'1973-05-21',]   #selecting values after '1973-05-21' 

###################################  Explaratory Data analysis  #############################

#Average of temperature based on days and months
aggregate(temp.xts, by = months(inds), FUN = mean)
aggregate(temp.xts, by = months(inds), FUN = mean)



#Ploting timesereis variables (to see visially the stationarity)
autoplot(temp.xts) + ggtitle("Timeseres of New York Tempreture") + xlab("Time(day)") + ylab("Temperature (F)")
autoplot(wind.xts) + ggtitle("Timeseres of New York Windspeed") + xlab("Time(day)") + ylab("Speed (MPH)")
autoplot(ozone.filled.xts) + ggtitle("Timeseres of New York") + xlab("Time(day)") + ylab("Ozone unit")

#Decomposing timesereis into three main components (Decompose() function only works with "ts" objects)
temp.ts = ts(airquality$Temp, start=1, frequency = 30)
plot(decompose(temp.ts))

wind.ts = ts(airquality$Wind, start=1, frequency = 30)
plot(decompose(wind.ts))


# Histogram and correaltion plots
ggplot(data=data.frame(ozone = coredata(ozone.filled.xts), time = time(temp.xts))) + 
  geom_histogram(aes(x=ozone))

# Correlation plot
corrplot(cor(tot.xts))

#Computing autocorrelation and partial autocorrlation
acf2(temp.xts)
acf2(wind.xts)
acf2(ozone.xts)

#computing cross-correlation function between time sereis
ccf2(as.ts(temp.xts),as.ts(wind.xts))

###################################  Timeseries Data analysis  #############################

# Estimating linear trend using linear regression and plotting them
trend.fit = lm(coredata(temp.xts) ~ time(temp.xts))
summary(trend.fit)
autoplot(temp.xts) + geom_abline(intercept = trend.fit$coefficients[1], slope = trend.fit$coefficients[2])


#Chekcing the stationarity for temperature timeseries, using Augmented Dickey–Fuller (ADF) t-statistic test
adf.test(temp.xts)

# Making Stationarity by detredning using linear regression (this only removes first order linear trend)
#R. Shumway (author), D. Stoffer book, chapter 2
res= temp.xts - trend.fit$fitted.values
autoplot(res)
adf.test(res)


#Making Stationarity using first order differencing (R. Shumway (author), D. Stoffer book, chapter 2)
temp.xts.diff1 = diff(temp.xts)
autoplot(temp.xts.diff1)
#Statistical test
adf.test(temp.xts.diff1[-1,])


#Smoothing timeseries (for temperature) using Locally Weight Regression and Moving Average methods
#R. Shumway (author), D. Stoffer book, chapter 2
temp.lowess = lowess(temp.xts)
temp.ma = ma(temp.xts, order = 10)  #Length of window is 10
#Plotting
ggplot(data = data.frame(temp = coredata(temp.xts), temp.lowess = temp.smooth$y, temp.ma =temp.ma, time = index(temp.xts)))+
  geom_line(aes(x=time, y = temp)) + geom_line(aes(x=time, y=temp.lowess), col="red", size=1) + 
  geom_line(aes(x=time, y=temp.ma), col= "blue", size =1)


###################################  Model Building, Chekcing and Diagnostic  #############################


#First Order autoRegressive model (using stationary timeseries)
AR.fit = arima(temp.xts.diff1, order = c(1, 0, 0))
summary(AR.fit)

#First order Moving Average model (using stationary timeseries)
MA.fit = arima(temp.xts.diff1, order = c(0, 0, 1))
summary(MA.fit)

#Fitting Autoregressive Moving Average model (using stationary timeseries)
ARMA.fit = arima(temp.xts.diff1, order = c(1,0,1))
summary(ARMA.fit)

#Fitting Autoregressive Integrated Moving Average model (using Nonstationary timeseries)
ARIMA.fit = arima(temp.xts, order = c(1,1,1))
summary(ARIMA.fit)


#Fitting ARIMA model with automatic parameter selection
Auto.fit=auto.arima(temp.xts)
summary(Auto.fit)

#Comparing models
print(cat("\n The AIC of AR.fit:", AIC(AR.fit), "\n The AIC of MA.fit:", AIC(MA.fit),
          "\n The AIC of ARMA.fit:", AIC(ARMA.fit), "\n The AIC of ARIMA.fit:", AIC(ARIMA.fit)))





###################################  Training, Evaluating and Forecasting Model  #############################

#Definfing a custom function
fun1= function(x,h){forecast(auto.arima(x), h=h)}

#Evaluating model by cross-validation (using one prediction ahead)
# Forecasting Using R, Chapter 2, DataCamp
errors= tsCV(temp.xts, forecastfunction = fun1, h=1)

print(cat("MSE is:", mean(errors^2 , na.rm = TRUE)))

#Forecasting 5 elements ahead
fc = forecast(Auto.fit, h =5)
autoplot(fc, size=1)




