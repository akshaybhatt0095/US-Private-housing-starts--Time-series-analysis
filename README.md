# US-Private-housing-starts--Time-series-analysis
Housing starts are the number of new residential construction projects that have begun in a month. US Housing starts is a non-stationary data, and we aim to forecast the housing starts for the future periods using different approaches and find the best model for our time series.

<img src="https://github.com/akshaybhatt0095/US-Private-housing-starts--Time-series-analysis/blob/main/title.png" width="1100" height="300">
The goal of this study is to perform statistical analysis and Forecasting on the US Housing starts data which consists of monthly Housing start data from 1959 to 2017. The properties of the data are described, and basic time series techniques are applied to the data. Plots of the series, autocorrelation function and the forecast graphs are some of the graphical tools used to analyze the series. We also aim to fit different models to the data to make credible forecasts from the model. The data was downloaded from the Forecast Chart website, (https://www.forecast-chart.com/chart-housing-starts.html), from 1st January 1959 to 1st December 2017.
A year of data is 12 months data which equals to 708 data points overall for 59 years. Here are the steps followed to finalize the best forecasting model for the dataset.
• Define the Goal
• Get Data
• Explore and visualize the series
• Preprocess data
• Data Partition
• Apply Forecasting methods
• Evaluating and comparing model performance
• Implement Forecasts/system

The following activities were accomplished:
The 8 steps of time series forecasting are applied to the Housing starts data to extract the best forecasting method. The goal is to select the best time series forecasting model to predict the Housing starts by utilizing the data from the past.
Several methods of forecasting like the Naive Forecast, moving average, Simple Exponential smoothing, Advanced exponential smoothing, multiple regression and Arima models are applied on the dataset and it was found that the trailing moving average model did better than the other models in predicting the future housing starts.
The tech stack used in this project involves R, R studio and excel.

Forecasting methods used:
o Naïve Forecast
o Moving average
o Two level Forecasting
o Simple Exponential smoothing
o Multiple regression models
o Autoregressive and ARIMA models

Model performance:
<img src="https://github.com/akshaybhatt0095/US-Private-housing-starts--Time-series-analysis/blob/main/Model%20performance.png" width="1100" height="500">

Observations:
• From the above accuracy measures table for all the models we can conclude that the trailing MA model with window width of 2 performed very well in capturing most of the data and could be the best model to forecast Housing starts in US
• The trailing MA model with window width of 2 has the lowest RMSE and MAPE score of 57.13 and 3.147 respectively
• Other than the trailing MA model, the exponential smoothing models and the ARIMA models did well in fitting the data but had a RMSE score almost twice the trailing MA model

Conclusion:
In this US Housing starts time series forecasting, we conducted time series analysis and forecasted housing starts using various time series forecasting models. From the results, it was observed that the simple model like the trailing MA average model outperformed the advanced models like the ARIMA models and advanced exponential smoothing models. The trailing MA model had a MAPE of 3.147 percent and RMSE of 57.13 and when compared to the ARIMA and Holt’s winter model the performance was found to be almost 50% efficient. This suggests that we should explore simple forecasting models in predicting the Housing starts. However, we also must be aware of the model overfitting in the training sets. In our case the model was consistent when tried on the entire dataset as well. As we often assume that past patterns repeat in the future, we can use this model confidently. Also, we can make use of ensembles techniques like the weighted voting, simple averaging, weighted averaging etc. to get precise forecasts.
This forecasting model can be further built as an interactive application which will help the real estate industry and other stakeholders in the real estate market.
