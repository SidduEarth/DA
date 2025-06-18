install.packages("lubridate")
install.packages("e1071")

library(lubridate)
library(e1071)

# Load & preprocess
weather_data <- read.csv("D:/R programming/weather_data.csv", stringsAsFactors = FALSE)
weather_data$date <- dmy(weather_data$date)
weather_data$day_of_year <- yday(weather_data$date)
weather_data$month <- month(weather_data$date)
weather_data$weekday <- wday(weather_data$date)
weather_data$rain_label <- as.factor(ifelse(weather_data$rain > 0, "Yes", "No"))

# 🔹 Initial dataset plot
dev.new()
plot(weather_data, main = "Weather Dataset", col = "green")

# Split data
set.seed(123)
idx <- sample(1:nrow(weather_data), size = 0.8 * nrow(weather_data))
train <- weather_data[idx, ]; test <- weather_data[-idx, ]

# Train models
lm_temp <- lm(temperature ~ humidity + pressure + day_of_year + month + weekday, data = train)
train$predicted_temp <- predict(lm_temp, newdata = train)
svm_rain <- svm(rain_label ~ humidity + predicted_temp + pressure + day_of_year + month + weekday,
                data = train, type = "C-classification", kernel = "radial")

# Evaluate
pred_temp <- predict(lm_temp, newdata = test)
cat("RMSE:", round(sqrt(mean((test$temperature - pred_temp)^2)), 2), "\n")
summary(lm_temp)

# 🔹 Plot actual vs predicted
dev.new()
plot(test$temperature, pred_temp, col = "blue", pch = 16,
     main = "Actual vs Predicted Temperature",
     xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red", lwd = 2)

# Forecast next 7 days
future_dates <- seq(max(weather_data$date) + 1, by = "day", length.out = 7)
future <- data.frame(
  date = future_dates,
  day_of_year = yday(future_dates),
  month = month(future_dates),
  weekday = wday(future_dates),
  humidity = mean(train$humidity),
  pressure = mean(train$pressure)
)
future$predicted_temp <- predict(lm_temp, newdata = future)
future$predicted_temperature <- round(future$predicted_temp, 2)
future$rain_prediction <- ifelse(as.character(predict(svm_rain, newdata = future)) == "Yes", "RAIN=YES", "RAIN=NO")

# 🔹 Show forecast
cat("\nNext 7 Days Forecast:\n")
print(future[, c("date", "predicted_temperature", "rain_prediction")])

# 🔹 Forecast plot
dev.new()
plot(future$date, future$predicted_temperature, type = "o", col = "red", lwd = 4, pch = 5,
     main = "7-Day Forecast: Temperature & Rain",
     xlab = "Date", ylab = "Temperature (°C)")
text(future$date, future$predicted_temperature + 0.4, labels = future$predicted_temperature, col = "red")
rain_colors <- ifelse(future$rain_prediction == "RAIN=YES", "blue", "magenta")
text(future$date, future$predicted_temperature + 1, labels = future$rain_prediction, col = rain_colors, font = 2)
legend("topright", legend = c("Temperature", "RAIN=YES", "RAIN=NO"),
       col = c("red", "blue", "magenta"), pch = 16, bty = "n")

# 🔹 Raw temperature with rain points
dev.new()
plot(weather_data$date, weather_data$temperature, type = "o", col = "magenta", lwd = 2, pch = 16,
     main = "Temperature Over Time", xlab = "Date", ylab = "Temperature (°C)")
points(weather_data$date[weather_data$rain > 0],
       weather_data$temperature[weather_data$rain > 0],
       col = "blue", pch = 17, cex = 1.2)
legend("topright", legend = c("Temperature", "Rainy Days"),
       col = c("magenta", "blue"), pch = c(16, 17), bty = "n")
