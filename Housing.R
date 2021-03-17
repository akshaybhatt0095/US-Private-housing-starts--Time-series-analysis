

#PART 1


## USE FORECAST and Zoo LIBRARY.
library(forecast)
library(zoo)

library(ggplot2)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/aksha/Desktop/Time series analytics/Project")

# Create data frame.
Housing.data <- read.csv("US Housing Starts.csv")

# See the first 6 records of the file.
head(Housing.data)


#PART 2

## CREATE TIME SERIES DATA SET.
## PLOT TIME SERIES DATA. 

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# Arguments start and end are pairs: (season number, period number).
Housing.ts <- ts(Housing.data$Housing.Starts, 
                   start = c(1959, 1), end = c(2017, 12), freq = 12)

## Use plot() to plot time series data  
plot(Housing.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)", 
     ylim = c(100, 3000), main = "US Housing starts", col = "blue")



#PART 3- TIME SERIES COMPONENTS

## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Housing.stl <- stl(Housing.ts, s.window = "periodic")
autoplot(Housing.stl, main = "Housting starts Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(Housing.ts, lag.max = 12, main = "Autocorrelation for US Housing Starts")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)




#PART 4- Visualize the data using linear and quadratic trend line


## DEVELOP REGRESSION MODELS FOR TIME SERIES DATA.
## PLOT TIMES SERIES DATA WITH REGRESSION TRENDLINES.

# Use tslm() function to create linear trend (Housing.lin) and 
# quadratic trend (Housing.quad) for time series data. 
Housing.lin <- tslm(Housing.ts ~ trend)
Housing.quad <- tslm(Housing.ts ~ trend + I(trend^2))

# Use plot() function to create plot with linear trendline. 
plot(Housing.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)",
     ylim = c (100, 3000), main = "US Housing starts with Linear Trendline", 
     col="blue")
lines(Housing.lin$fitted, lwd = 2)

# Use plot() function to create plot with quadratic trendline. 
plot(Housing.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)",
     ylim = c (100, 3000), main = "US Housing starts with Quadratic Trendline", 
     col="blue")
lines(Housing.quad$fitted, lwd = 2)


#PART 5- Data partition


# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
#Taking 18 years data as validation data(216 months)
nValid <- 216
nTrain <- length(Housing.ts) - nValid
train.ts <- window(Housing.ts, start = c(1959, 1), end = c(1959, nTrain))
valid.ts <- window(Housing.ts, start = c(1959, nTrain + 1), 
                   end = c(1959, nTrain + nValid))
train.ts
valid.ts


# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2030), main = "", lwd = 2) 
axis(1, at = seq(1959, 2030, 1), labels = format(seq(1959, 2030, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 17, 2017 - 17), c(0, 3000))
lines(c(2018, 2018), c(0, 3000))
text(1979, 2700, "Training")
text(2009, 2700, "Validation")
text(2024, 2700, "Future")
arrows(2017 - 18, 2600, 1960, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 16, 2600, 2017, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 2600, 2029, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)



#PART 6- Fit regression model with linear and quadratic trend

## FIT REGRESSION MODEL TO TIME SERIES.
## FORECAST USING VALIDATION SET.
## PLOT FORECASTS.

# Use tslm() function to fit a regression model (equation) to the time series 
# with linear trend (Housing.lin) and quadratic trend model (Housing.quad).
Housing.lin <- tslm(train.ts ~ trend)
Housing.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of forecasting equation and associated parameters.
summary(Housing.lin)
summary(Housing.quad)

# Apply forecast() function to make predictions for ts data in
# training and validation sets.  
Housing.lin.pred <- forecast(Housing.lin, h = nValid, level = c(80, 95))
Housing.quad.pred <- forecast(Housing.quad, h = nValid, level = c(80, 95))

# Plot predictions for linear trend forecast.
plot(Housing.lin.pred$mean, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2020), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(0, at = seq(1959, 2020, 1), labels = format(seq(1959, 2020, 1)) )
lines(Housing.lin$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot predictions for quadratic trend forecast.
plot(Housing.quad.pred$mean, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2020), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1959, 2020, 1), labels = format(seq(1959, 2020, 1)) )
lines(Housing.quad$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)


## IDENTIFY FORECAST ACCURACY

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(Housing.lin.pred$mean, valid.ts), 3)
round(accuracy(Housing.quad.pred$mean, valid.ts), 3)




# PART 7- Naive and seasonal naive forecast

## IDENTIFY NAIVE AND SEASONAL NAIVE FORECASTS.

# Use naive() to make naive forecast (Housing.naive.pred) 

# Use snaive() to make seasonal naive forecast (Housing.snaive.pred) for 

Housing.naive.pred <- naive(Housing.ts)
Housing.snaive.pred <- snaive(Housing.ts)





## IDENTIFY FORECAST ACCURACY FOR NAIVE and SEASONAL NAIVAE FORECASTS.

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(Housing.naive.pred$fitted, Housing.ts), 3)
round(accuracy(Housing.snaive.pred$fitted, Housing.ts), 3)



#PART 8 - SMOOTHING METHODS

# A - MOVING AVERAGE


## CREATE TRAILING MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS).
## SHOW FIRST SIX AND LAST SIX VALUES OF TRAILING MA.
## COMBINE ORIGINAL DATA AND TRAILING MA IN TABLE.
## IDENTIFY FORECAST ACCURACY FOR TRAILING MA FORECASTS.

# Create trailing moving average with window (number of periods) k = 2, 5, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(Housing.ts, k = 2, align = "right")
ma.trailing_5 <- rollmean(Housing.ts, k = 5, align = "right")
ma.trailing_12 <- rollmean(Housing.ts, k = 12, align = "right")

# Combine Housing.ts and ma.trailing in one data table.
ma_trailing_tab <- cbind(Housing.ts, ma.trailing_2, ma.trailing_5, ma.trailing_12)
ma_trailing_tab

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trailing_2, Housing.ts), 3)
round(accuracy(ma.trailing_5, Housing.ts), 3)
round(accuracy(ma.trailing_12, Housing.ts), 3)


# Plot original data and trailing MA.
plot(Housing.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2020), main = "Trailing Moving Average") 
axis(0, at = seq(1959, 2020, 1), labels = format(seq(1959, 2020, 1)))
lines(ma.trailing_2, col = "brown", lwd = 2, lty = 1)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 5)
legend(2000,3300, legend = c("Housing starts", "Trailing MA, k=2", 
                             "Trailing MA, k=12"), 
       col = c("black", "brown", "blue"), 
       lty = c(1, 1, 5), lwd =c(1, 2, 2), bty = "n")




# TWO LEVEL FORECASTING


## DE-TRENDING and DE-SEASONALIZING TIME SERIES USING REGRESSION
## CREATE TRAILING MA USING RESIDUALS.
## FORECAST USING REGRESSION AND TRAILING MA INTO FUTURE PERIODS.

# Fit a regression model with quadratic trend and seasonality.
reg.trend.seas <- tslm(Housing.ts ~ trend + I(trend^2) + season)
summary(reg.trend.seas)


# Create forecast for the 60 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 60, level = 0)
reg.trend.seas.pred

# Identify and display residuals for time series based on the regression
# (differences between actual and regression values in the same periods).
reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

# Apply trailing MA with 12 periods in the window to residuals.
ma.trailing.res_12 <- rollmean(reg.trend.seas.res, k = 12, align = "right")
ma.trailing.res_12

# Create forecast for residuals for the 60 periods into the future.
ma.trailing.res_12.pred <- forecast(ma.trailing.res_12, h = 60, level = 0)
ma.trailing.res_12.pred


# To develop real forecast for 60 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.60 <- reg.trend.seas.pred$mean + ma.trailing.res_12.pred$mean
ts.forecast.60

# Create a table with regression forecast, trailing MA for residuals
# and total forecast for 60 months into the future.
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_12.pred$mean, 
                                ts.forecast.60)
total.reg.ma.pred

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(reg.trend.seas.pred$fitted, Housing.ts), 3)
round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_12, Housing.ts), 3)




## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS INTO
## 60 PERIODS IN THE FUTUrE.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALs, AND 
## TRAILING MA FORECAST INTO 60 PERIODS IN THE FUTURE.

# Plot original Housing starts time series data and regression model.
plot(Housing.ts, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2025), lwd =2,
     main = "Housing starts Series and Regression with Trend and Seasonality") 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "green", lty =5, lwd = 2)
legend(2000,3300, legend = c("Housing starts", "Regression",
                             "Regression Forecast for 60 Periods into Future"), 
       col = c("black", "brown" , "green"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")



# Plot regression residuals data and trailing MA based on residuals.
plot(reg.trend.seas.res, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(-2000, 2000), bty = "l",
     xaxt = "n", xlim = c(1959, 2025), lwd =2, 
     main = "Regression Residuals and Trailing MA for Residuals, k =12") 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(ma.trailing.res_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trailing.res_12.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2000, 3300, legend = c("Regresssion Residuals", "Trailing MA for Residuals, k=12", 
                             "Trailing MA Forecast for 60 Periods into Future"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")






#PART 9- SIMPLE EXPONENTIAL SMOOTHING

## SIMPLE EXPONENTIAL SMOOTHING (SES) WITH ORIGINAL DATA, ALPHA = 0.2.

# Create simple exponential smoothing (SES) for Housing data with alpha = 0.2.
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N),
# & no seasonality (N). Use alpha = 0.2 to fit SES over the original data.
ses.orig <- ets(Housing.ts, model = "ANN", alpha = 0.2)
ses.orig

# Use forecast() function to make predictions using this SES model with alpha = 0.2 
# and 60 periods into the future. 
# Show predictions in tabular format.
ses.orig.pred <- forecast(ses.orig, h = 60, level = 0)
ses.orig.pred


## SIMPLE EXPONENTIAL SMOOTHING WITH ORIGINAL DATA AND OPTIMAL ALPHA.

# Create simple exponential smoothing (SES) for Housing data with optimal alpha.
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N),
# & no seasonality (N). Use optimal alpha to fit SES over the original data.
ses.opt <- ets(Housing.ts, model = "ANN")
ses.opt

# Use forecast() function to make predictions using this SES model with optimal alpha
# and 60 periods into the future.
# Show predictions in tabular format
ses.opt.pred <- forecast(ses.opt, h = 60, level = 0)
ses.opt.pred


# Plot ses predictions for original data and optimal alpha.
plot(ses.opt.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2025), lwd = 2,
     main = "Original Data and SES Optimal Forecast, Alpha = 0.6534", 
     flty = 5) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(ses.opt.pred$fitted, col = "blue", lwd = 2)


#Accuracy
round(accuracy(ses.opt.pred$fitted, Housing.ts),3)





#PART 10- ADVANCED EXPONENTIAL SMOOTHING
#HOLT WINTER'S MODEL

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 60 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Housing data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(Housing.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, Ad, N), with alpha = 0.5663 and gamma = 0.0802.

# Use forecast() function to make predictions using this HW model for
# 60 month into the future.
length <- length(Housing.ts)
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 60 , level = c(0, 95))
HW.ZZZ.pred
HW.ZZZ.pred$fitted

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2025), lwd = 2,
     main = "Holt-Winter's Model with Automated Selection of Model Options and Forecast for Future Periods", 
     flty = 5) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)


# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, Housing.ts), 3)



# PART 10- MULTIPLE REGRESSION 


## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

# Plot ts data, linear trend and forecast for validation period.
plot(train.lin.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Linear Trend for Training and Validation Data", flty = 2) 
axis(0, at = seq(1959, 2020, 1), labels = format(seq(1959, 2020, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,3100, legend = c("Housing starts Time Series", "Linear Regression for Training Data",
                             "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")



# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred, valid.ts), 3)


#accuracy on entire dataset
round(accuracy(train.lin$fitted, Housing.ts), 3)





## FIT REGRESSION MODEL WITH EXPONENTIAL TREND: MODEL 2. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create regression model with exponential trend.
# If lambda = 0, tslm function applies Box-Cox transformation
# for log(y) - exponential trend.
# If lambda = 1, tslm function will just have a linear trend
# (the same as the original regression with linear trend, train.lin).
train.expo <- tslm(train.ts ~ trend, lambda = 0)

# See summary of exponential trend model and associated parameters.
summary(train.expo)

# Apply forecast() function to make forecast using exponential  
# trend for validation period.  
train.expo.pred <- forecast(train.expo, h = nValid, level = 0)

# Plot ts data, exponential and linear trends, and 
# respective forecasts for validation period.
plot(train.expo.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Linear and Exponential Regression Trends") 
axis(0, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(train.expo.pred$fitted, col = "blue", lwd = 2)
lines(train.lin.pred$fitted, col = "brown", lwd = 2, lty = 3)
lines(train.lin.pred$mean, col = "brown", lwd = 2, lty = 3)
lines(valid.ts, col = "black", lty = 1)
lines(train.ts, col = "black", lty = 1)
legend(2000,3100, legend = c("Housing starts Time Series", 
                             "Exponentail Trend for Training and Validdation Data", 
                             "Linear Trend for Training and Validation Data"), 
       col = c("black", "blue" , "brown"), 
       lty = c(1, 1, 3), lwd =c(2, 2, 2), bty = "n")


#accuracy on validation data
round(accuracy(train.expo.pred, valid.ts), 3)


#accuracy on entire dataset
round(accuracy(train.expo$fitted, Housing.ts), 3)




## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 3. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

# Plot ts data, regression with quadratic trend and forecast for validation period.
plot(train.quad.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Quadratic Trend for Training and Validation Data", 
     flty = 2) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,3100, legend = c("Housing starts Time Series", "Quadratic Trend for Training Data",
                             "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# Use accuracy() function to identify common accuracy measures
round(accuracy(train.quad.pred, valid.ts), 3)


#Accuracy on entire data set 
round(accuracy(train.quad$fitted, Housing.ts), 3)





## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)


# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Model with Seasonality for Training and Validation Data", 
     flty = 5) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,3100, legend = c("Housing starts Time Series", "Seasonality Model for Training Data",
                             "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")



# Use accuracy() function to identify common accuracy measures
round(accuracy(train.season.pred, valid.ts), 3)


#Accuracy on entire data set 
round(accuracy(train.season$fitted, Housing.ts), 3)






## FIT REGRESSION MODEL WITH linear TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.ltrend.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.ltrend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.ltrend.season.pred <- forecast(train.ltrend.season, h = nValid, level = 0)

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.ltrend.season.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Model with Quadratic Trend and Monthly Seasonality", 
     flty = 5, lwd = 2) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(train.ltrend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1, lwd = 2)
legend(2000,3100, legend = c("Housing starts Time Series", 
                             "Trend and Seasonality Model for Training Data",
                             "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# Use accuracy() function to identify common accuracy measures
round(accuracy(train.ltrend.season.pred, valid.ts),3)


#Accuracy on entire data set 
round(accuracy(train.ltrend.season$fitted, Housing.ts), 3)






## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 6. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.trend.season.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xlim = c(1959, 2025), main = "Model with Quadratic Trend and Monthly Seasonality", 
     flty = 5, lwd = 2) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(train.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1, lwd = 2)
legend(2000,3100, legend = c("Housing starts Time Series", 
                             "Trend and Seasonality Model for Training Data",
                             "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# Use accuracy() function to identify common accuracy measures
round(accuracy(train.trend.season.pred, valid.ts),3)


#Accuracy on entire data set 
round(accuracy(train.trend.season$fitted, Housing.ts), 3)


#Accuracy comparison 

round(accuracy(train.lin$fitted, Housing.ts), 3)

round(accuracy(train.expo$fitted, Housing.ts), 3)

round(accuracy(train.quad$fitted, Housing.ts), 3)

round(accuracy(train.season$fitted, Housing.ts), 3)

round(accuracy(train.ltrend.season$fitted, Housing.ts), 3)

round(accuracy(train.trend.season$fitted, Housing.ts), 3)




#PART 11- Autoregressive and ARIMA models

#AR1 model on entire dataset

# Use Arima() function to fit AR(1) model for historical data. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
his.ar1 <- Arima(Housing.ts, order = c(1,0,0))
summary(his.ar1)



## FIT SEASONAL ARIMA FOR ENTIRE DATA SET. 

# Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(Housing.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for future 12 months 

arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred


## FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(Housing.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 months. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred



#Accuracy 

round(accuracy(arima.seas.pred$fitted, Housing.ts), 3)
round(accuracy(auto.arima.pred$fitted, Housing.ts), 3)



# Model implementation - forecast for 24 periods using trailing MA model with k=2

ma.trailing_2.pred <- forecast(ma.trailing_2, h=24, level = 0)
ma.trailing_2.pred


#Plot the trailing MA forecast

plot(ma.trailing_2.pred, 
     xlab = "Time", ylab = "Housing starts (in 000s)", ylim = c(100, 3000), bty = "l",
     xaxt = "n", xlim = c(1959, 2025), lwd = 2,
     main = "Trailing MA Model with window width 2 Forecast for Future Periods", 
     flty = 5) 
axis(1, at = seq(1959, 2025, 1), labels = format(seq(1959, 2025, 1)))
lines(ma.trailing_2.pred$fitted, col = "blue", lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 17, 2017 - 17), c(0, 3000))
lines(c(2018, 2018), c(0, 3000))
text(1979, 2700, "Training")
text(2009, 2700, "Validation")
text(2024, 2700, "Future")
arrows(2017 - 18, 2600, 1960, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 16, 2600, 2017, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 2600, 2029, 2600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

































