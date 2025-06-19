

# Install package (run once)
install.packages("lubridate")

# Load library
library(lubridate)

# Load dataset into df
df <- read.csv("D:/R programming/weather_data.csv", stringsAsFactors = FALSE)

# Convert date using lubridate
df$date <- dmy(df$date)

# Plot all features
plot(df, main = "Weather Data")


# Build linear regression model
model <- lm(temperature ~ humidity + pressure + rain, data = df)

# Summary of model
cat("----- Model Summary -----\n")
print(summary(model))

# Predict temperature for existing data
pred_temp <- predict(model, df)

# Calculate RMSE
rmse <- sqrt(mean((df$temperature - pred_temp)^2))
cat("\nRMSE:", round(rmse, 2), "\n")

# Plot: Temperature over time
plot(df$date, df$temperature, type = "l", col = "red",
     main = "Temperature Over Time", xlab = "Date", ylab = "Temperature")
  
plot(df$temperature, pred_temp, col = "blue",
     xlab = "Actual Temperature", ylab = "Predicted Temperature",
     main = "Actual vs Predicted Temperature")
abline(0, 1, col = "red")

# Forecast next 7 days using average values
future_data <- data.frame(
  humidity = rep(mean(df$humidity), 7),
  pressure = rep(mean(df$pressure), 7),
  rain = rep(mean(df$rain), 7)
)
future_temp <- predict(model, future_data)
future_dates <- seq(max(df$date) + 1, by = "day", length.out = 7)

# Print next 7-day forecast
cat("\n----- 7-Day Temperature Forecast -----\n")
forecast <- data.frame(Date = future_dates, Predicted_Temperature = round(future_temp, 2))
print(forecast)


