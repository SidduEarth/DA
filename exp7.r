# Install and load required packages
if (!require(forecast)) install.packages("forecast", dependencies = TRUE)
if (!require(tseries)) install.packages("tseries", dependencies = TRUE)
library(forecast)
library(tseries)

# Load time series data
ts_data <- AirPassengers

# 📌 Plot original time series
dev.new()
plot(ts_data, main = "AirPassengers Time Series", ylab = "Passengers", col = "blue")

# 📌 ADF test for stationarity
adf_result <- adf.test(ts_data)
print(adf_result)

# 📌 ACF & PACF plots
dev.new(); acf(ts_data, main = "ACF Plot")
dev.new(); pacf(ts_data, main = "PACF Plot")

# 📌 Apply differencing if needed
ts_diff <- if (adf_result$p.value > 0.05) {
  print("Differencing applied.")
  diff(ts_data)
} else ts_data

# 📌 Re-check ADF test
print(adf.test(ts_diff, na.action = na.omit))

# 📌 Fit best ARIMA model
best_arima <- auto.arima(ts_data)
summary(best_arima)

# 📌 Forecast next 12 months
fc <- forecast(best_arima, h = 12)

# 📌 Plot forecast
dev.new(); plot(fc, main = "ARIMA Forecast", col = "blue")

# 📌 Forecast values
print(fc)

# 📌 Residual diagnostics
checkresiduals(best_arima)
