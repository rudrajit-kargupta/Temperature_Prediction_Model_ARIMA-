#Importing The Data
GlobalLandTemperaturesByCountry <- read.csv("climate-change-earth-surface-temperature-data/GlobalLandTemperaturesByCountry.csv")
UK<-ts(GlobalLandTemperaturesByCountry$AverageTemperature[548513:551632], frequency=12)

#Decomposing the data
components<- decompose(UK)
plot(components)
UK_nonSeasonal<- UK-components$seasonal
plot.ts(UK_nonSeasonal)

#Test the data for stationarity
library(tseries)
adf.test(UK_nonSeasonal,alternative="stationary", k=0)
library(aTSA)
stationary.test(UK_nonSeasonal, method ="kpss")
stationary.test(UK_nonSeasonal, method ="pp")
#The data turned out to be non-stationary.

#Making the data stationary by differencing
UK_diff1<- diff(UK_nonSeasonal, differences = 1)
ts.plot(UK_diff1)
library(tseries)
adf.test(UK_diff1, alternative="stationary", k=0)
library(aTSA)
stationary.test(UK_diff1, method="kpss")
stationary.test(UK_diff1, method="pp")

#Choosing the correct ARIMA model
acf(UK_diff1)
pacf(UK_diff1)
library('forecast')
auto.arima(UK_nonSeasonal)

#Making Forecasts
pred_model<- arima(UK_nonSeasonal, order=c(1,1,1))
library(forecast)
pred_vals<- forecast(pred_model, level=c(95), h=24)
plot(pred_vals)
pred_vals

#Checking the validity of the forecasts
acf(pred_vals$residuals)
hist(pred_vals$residuals)
Box.test(pred_vals$residuals, lag=24, type="Ljung-Box")




