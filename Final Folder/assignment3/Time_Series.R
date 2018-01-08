#import the dataset and make some changes
library(readr)
milk_production <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/assignment3/milk-production(1).csv")
milk_production<-milk_production[,2]

milk_production_timeseries<-ts(milk_production)

#contains monthly milk productions for January 1970-Decemeber 1983
mp_TS<-ts(milk_production,frequency = 12,start=c(1970,1))

#plotting time series

plot.ts(mp_TS)
lines(mp_TS,col="blue")

## a. ##
#***Simple Moving Average(SMA)***# ->it is used to smooth time series data 
#install.packages("TTR")
library("TTR")
mp_TS3<-SMA(mp_TS,n=5)
plot.ts(mp_TS3)
lines(mp_TS3,col="purple")


mp_TS5<-SMA(mp_TS,n=11)
plot.ts(mp_TS5)
lines(mp_TS5,col="red")

mp_TS8<-SMA(mp_TS,n=20)
plot.ts(mp_TS8)
lines(mp_TS5,col="green")

#To estimate the trend component and seasonal component of a seasonal time series that can be described using an additive model, we can use the "decompose()" function in R. This function estimates the trend, seasonal, and irregular components of a time series that can be described using an additive model.
mp_decompose=decompose(mp_TS)
plot(mp_decompose) #The plot above shows the original time series (top), the estimated trend component (second from top), the estimated seasonal component (third from top), and the estimated irregular component (bottom)

## b. ##
#****Forecasts suing Exponential Smoothing***###

mp_exp=HoltWinters(mp_TS)
plot(mp_exp) #The plot shows the original time series in black, and the forecasts as a red line. The time series of forecasts is much smoother than the time series of the original data here.
lines(mp_exp$fitted[,1],col="green")#same as above but in green 
mp_exp$SSE
#install.packages("forecast")
library(forecast)
forecast_holtwinter=forecast(mp_exp)
forecast_holtwinter

accuracy(forecast_holtwinter)
plot(forecast_holtwinter)

count=168
x=mean(mp_TS3[-(1:7)])
for (k in 8:count) {
  mean_abs_dev3=mean(abs(mp_TS3[k]-x))
  mfe3=mean(mp_TS3[k]-x)
  mad3=mad(mp_TS3[k], centre, constant = 1.4826, na.rm = FALSE,  low = FALSE, high = FALSE)
}
y=mean(mp_TS5[-(1:12)])
for (k in 13:count) {
  mean_abs_dev4=mean(abs(mp_TS5[k]-y))
  mfe4=mean(mp_TS5[k]-y)
  mad4=mad(mp_TS5[k], centre, constant = 1.4826, na.rm = FALSE,  low = FALSE, high = FALSE)
}
z=mean(mp_TS8[-(1:23)])
for (k in 24:count) {
  mean_abs_dev5=mean(abs(mp_TS8[k]-z))
  mfe5=mean(mp_TS8[k]-z)
  mad(mp_TS8[k], centre, constant = 1.4826, na.rm = FALSE,  low = FALSE, high = FALSE)
}





