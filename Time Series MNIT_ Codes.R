# Time series data
data("AirPassengers")
AP <- AirPassengers
str(AP)
tail(AP)
head(AP)

ts(AP, frequency = 12, start=c(1949,1))

attributes(AP)
plot(AP)

# Log transform
AP <- log(AP)
plot(AP)



#Test of stationary

#Method 1: Using URCA Package
install.packages(urca)
library(urca)

x<- ur.df(AP)
summary(x)

#ADF Test Value is 0.674
#If the test statistics is less than the critical value, 
#test is not significant. We will accept the null. 
# Null: The series is NS. 


#First Difference Series
AP_D<- diff(AP, differences = 1)
head(AP)
head(AP_D)
plot(AP_D)

x1<- ur.df(AP_D)
summary(x1)


#ADF Test Value is -8.8157
#If the test statistics is greater than the critical value, 
#test is significant. We will reject the null. 
# Null: The series is stationary. 


# Decomposition of time series
decomp <- decompose(AP)
plot(decomp)



# ARIMA - Autoregressive Integrated Moving Average
library(forecast)
model <- auto.arima(AP)
model
attributes(model)

# ACF and PACF plots
acf(model$residuals, main = 'Correlogram')
pacf(model$residuals, main = 'Partial Correlogram' )

# Ljung-Box test
Box.test(model$residuals, lag=20, type = 'Ljung-Box')

# Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(model$residuals))

# Forecast
f <- forecast(model, 48)
library(ggplot2)
autoplot(f)
accuracy(f)


