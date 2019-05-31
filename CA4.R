# To get the working directory
getwd()
# Seting the working diretory
setwd("C:/Users/JAYACHANDRAN/CA4")

# To load the data as CSV file for crime dataset 
crimedata <- read.csv("C:/Users/JAYACHANDRAN/CA4/crime_new.csv",stringsAsFactors = FALSE)

View(crimedata)


#Subset for northern Ireland data
df1 <- subset(crimedata, select =c(Calendar_Year,Count))
View(df1)

#Subset for Belfast data
df2 <- subset(crimedata, select =c(Calendar_Year,Count))
View(df2)

# Filtering Northern Ireland District in Crime data
library(dplyr)
NrthIre <- df1[which(crimedata$Policing_District == "Northern Ireland"),]

# Filtering Belfast District in Crime data
library(dplyr)
Belfast <- df2[which(crimedata$Policing_District == "Belfast City"),]

# structure of the dataset
str(NrthIre)
str(Belfast)


# showing the crime rates over the years for NrthIre using ggplot
ggseasonplot(ts_NrthIre)

# showing the crime rates over the years for Belfast using ggplot
ggseasonplot(ts_Belfast)


# Importing the required libraries
library('ggplot2')
library('forecast')
library('tseries')

# to plot the time series object of Northern Ireland
ts_NrthIre <- ts(NrthIre$Count, start=c(2001, 1), end=c(2018), frequency=12)
ts_NrthIre
# In this plot there are seasonal effects and random fluctuations,
# but no overall trend from a horizontal line

plot(ts_NrthIre)

start(ts_NrthIre)
end(ts_NrthIre)
frequency(ts_NrthIre)


# to plot the time series of Belfast
View(Belfast)
library('tseries')
ts_Belfast <- ts(Belfast$Count, start=c(2001, 1), end=c(2018), frequency=12)
ts_Belfast

# In this plot there are seasonal effects and random fluctuations,
# but no overall trend from a horizontal line
plot(ts_Belfast)

start(ts_Belfast)
end(ts_Belfast)
frequency(ts_Belfast)

View(ts_Belfast)

# Implementing Lag function 
# ACF series for Belfast
library(forecast)
library(tseries)
acf_val <- Acf(ts_Belfast) # autocorrelation
acf_val


#acf(NrthIre$Count, lag.max = 100)

# partial autocorrelation plot for NRTHIRE
pacf_result <- Pacf(ts_NrthIre)

# Test if a time series is stationary for NRTHIRE
library(tseries)
adf.test(ts_NrthIre)

# partial autocorrelation plot for belfast
pacf_result <- Pacf(ts_Belfast)

# Test if a time series is stationary for Belfast
library(tseries)
adf.test(ts_Belfast)


# hence the TS is a stationary

# Arima model for Northern Ireland
arima_model <- Arima(ts_NrthIre,  order = c(0,2,4))
arima_model

accuracy(arima_model)
forecast(arima_model)

# Auto Arima modl for Northern Irelan d
auto_arema_model <- auto.arima(ts_NrthIre)
auto_arema_model

accuracy(auto_arema_model)

# ARIMA model for Belfast

arima_model <- Arima(ts_Belfast,  order = c(0,1,4))
arima_model

accuracy(arima_model)

# Auto -ARIMA model for Belfast
auto_arema_model <- auto.arima(ts_Belfast)
auto_arema_model

accuracy(auto_arema_model)





# AIC value is best for auto arima model

# qq plot FOR fiiting line for Northern Ireland
qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# QQ plot for fitting the line for BElfast
#AIC value is best for auto arima model

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# Box plot for finding the value fits or not for the model of Northern Ireland

Box.test(arima_model$residuals,type = "Ljung-Box")
forecast(arima_model,15)

plot(forecast(arima_model, 15), xlab = "Year", ylab = "Crime Rates in Ireland")


# Box plot model for Belfast
Box.test(arima_model$residuals,type = "Ljung-Box")
forecast(arima_model,15)

plot(forecast(arima_model, 15), xlab = "Year", ylab = "crime rates in Belfast")

autoplot(ts_Belfast) + geom_forecast(h=15)

