rm(list =ls(all = TRUE))

library(zoo) # Provides Infrastructure for regular and irregular spaced Time Series

library(xts) # For Uniform handling of R's different time based data classes

library(forecast) # For Arima function


setwd("C:/Users/bvkka/Desktop/edureka/module9_class9_Timeseries")

read.csv("M9_NewYork_Births.txt")

births <- scan("M9_NewYork_Births.txt")

births

birth.ts <- ts(births, start = c(1946,1), frequency = 12)

birth.ts

AirPassengers

AP <- AirPassengers

AP

plot(AP,ylab = "Passengers (1000's)")

par(mfrow=c(1,2))

plot(AP)

plot(diff(AP))

# Auto Arima

par(mfrow=c(1,1))
y <- auto.arima(AP)
plot(forecast(y,h=6))
points(1:length(AP), fitted(y), type = "l", col = "red")
plot(forecast(y,h=6))

# Exponential Smoothing, Holts Exponential Smoothing Holt Winters Exponential Smoothing

# Simple Exponential Smoothing

plot(AP,ylab = "Passengers (1000's)")
plot(decompose(AP))

fit2 <- HoltWinters(AP)

# Forecasting the next 6 months using Holt Winters
AP_forecast <- forecast(fit2, h=6)

# Plotting the Forecast
plot(AP_forecast)

# Holt Winters
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/HoltWinters

# ARIMA 
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/HoltWinters
# http://people.duke.edu/~rnau/arimrule.htm



#