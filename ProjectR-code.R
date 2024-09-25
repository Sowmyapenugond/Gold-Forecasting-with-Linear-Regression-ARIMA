##### Clear memory
rm(list=ls())
######################Data Loading and visualization
# Install and load necessary packages

library(ggplot2)
library(dplyr)
library(readxl)
library(corrplot)
library(forecast)

golds <- read_excel("~/Desktop/GoldUP.xlsx")
View(golds)
# view the data
head(golds)

# Convert Date column to Date type
golds$Date <- as.Date(golds$Date, format = "%d-%m-%Y")

# Extract year from Date column
golds$Year <- lubridate::year(golds$Date)

# Check unique years
unique_years <- unique(golds$Year)
print(unique_years)

# Plotting gold prices
ggplot(golds, aes(x = Date, y = Gold_Price, group = 1)) + 
  geom_line() +
  labs(title = "Gold Price Over Time", x = "Date", y = "Price")


# Descriptive statistics
summary(golds$Gold_Price)


# Check correlation between Gold_Price and other variables
correlation <- cor(golds[, -1])  # excluding Date column
print(correlation)

# Check if gold_data is indeed a data frame
print(is.data.frame(golds))

#Plotting Correlations Between Variables
correlations <- cor(golds[, c("Gold_Price", "Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")], use = "complete.obs")
corrplot(correlations, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


# Fit a simple linear regression model
lm_model <- lm(Gold_Price ~ Crude_Oil + Interest_Rate + USD_INR + Sensex + CPI + USD_Index, data = golds)

# Summary of the linear regression model
summary(lm_model)

###Regression Model Coefficients Visualization
coefficients <- broom::tidy(lm_model)
ggplot(coefficients, aes(x = term, y = estimate, fill = estimate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Impact of Economic Indicators on Gold Prices", x = "Coefficients", y = "Estimate")


# Forecasting Gold_Price using the linear regression model
future_data <- data.frame(Crude_Oil = 100, Interest_Rate = 7, USD_INR = 47, Sensex = 4000, CPI = 38, USD_Index = 120)  # Example future data
predicted_gold_price <- predict(lm_model, newdata = future_data)

print(predicted_gold_price)

######Actual vs. Predicted Values Plot

actual_vs_predicted <- data.frame(Actual = golds$Gold_Price, Predicted = fitted(lm_model))
ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, col = "blue") +
  labs(title = "Actual vs. Predicted Gold Prices", x = "Actual Prices", y = "Predicted Prices")

# Make predictions with the linear regression model
predicted_values_lm <- predict(lm_model)

# Calculate residuals for linear regression
residuals_lm <- golds$Gold_Price - predicted_values_lm

# Calculate accuracy metrics for linear regression
accuracy_metrics_lm <- list(
  MAE = mean(abs(residuals_lm)),
  MSE = mean(residuals_lm^2),
  RMSE = sqrt(mean(residuals_lm^2)),
  MAPE = mean(abs(residuals_lm / golds$Gold_Price))
)

# Print accuracy metrics for linear regression
cat("Accuracy Metrics for Linear Regression Model:\n")
print(accuracy_metrics_lm)


# Create time series object
golds_ts <- ts(golds$Gold_Price, frequency = 12)

# Plot the original time series
autoplot(golds_ts, main = "Gold Price Over Time")

# Decomposition of the time series
decomposed <- decompose(golds_ts)

# Plot the decomposition
autoplot(decomposed)

# ARIMA model fitting
arima_model <- auto.arima(golds_ts)

# Summary of the ARIMA model
summary(arima_model)

# Forecasting
forecast_values <- forecast(arima_model, h = 12)  # Forecasting the next 12 periods

# Plotting forecast
autoplot(forecast_values, main = "Forecast of Gold Prices")

# Overlaying the actual data on the forecast plot
autoplot(golds_ts) +
  autolayer(forecast_values, series = "Forecast") +
  xlab("Date") + ylab("Gold Price") + ggtitle("Gold Price Forecast with ARIMA")

# Calculate residuals for ARIMA
residuals_arima <- residuals(forecast_values)

# Calculate accuracy metrics for ARIMA
accuracy_metrics_arima <- list(
  MAE = mean(abs(residuals_arima)),
  MSE = mean(residuals_arima^2),
  RMSE = sqrt(mean(residuals_arima^2)),
  MAPE = mean(abs(residuals_arima / golds$Gold_Price))
)

# Print accuracy metrics for ARIMA
cat("\nAccuracy Metrics for ARIMA Model:\n")
print(accuracy_metrics_arima)