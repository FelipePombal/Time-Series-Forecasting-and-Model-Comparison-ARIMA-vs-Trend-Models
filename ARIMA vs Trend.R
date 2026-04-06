# Load required packages
library(forecast)
library(tseries)  # For dm.test
library(readxl)

# Convert your data to a time series object.
# Adjust the 'start' parameter as needed.
data <- read_xlsx("../Project 1/Project1_EMP_Data.xlsx")
# EMPTPA <- ts(data$EMPTPA, start = c(2000,1), frequency = 12)
EMPTPA <- ts(data$EMPTPA, start = c(1980, 1), frequency = 12)

# ===== 1. Expanding Window Forecasts =====
# Define forecasting functions for ARIMA and TSLM
forecast_arima <- function(x, h) {
  forecast(auto.arima(x, max.p = 12, max.q = 12, seasonal = F, ic = "bic"), h = h)
}

forecast_tslm <- function(x, h) {
  forecast(tslm(x ~ trend + I(trend^2)), h = h)
}

# Drop first 59 observations ?
# EMPTPA_2 <- window(EMPTPA, start = time(EMPTPA)[60])  # Start at observation 60


# Compute cross-validated forecast errors (expanding window)
# ???? INITIAL 59 ????
# 'initial = 59' means the first forecast is based on observations 1 to 59.
errors_arima_exp <- tsCV(EMPTPA, forecastfunction = forecast_arima, h = 12, initial = 59)
errors_tslm_exp  <- tsCV(EMPTPA, forecastfunction = forecast_tslm,  h = 12, initial = 59)
# ???? INITIAL 59 ????

# Compute RMSE for each horizon (1 to 12)
rmse_arima_exp <- apply(errors_arima_exp, 2, function(x) sqrt(mean(x^2, na.rm = TRUE)))
rmse_tslm_exp  <- apply(errors_tslm_exp,  2, function(x) sqrt(mean(x^2, na.rm = TRUE)))

# Display RMSE results for expanding window forecasts
print("Expanding Window RMSE for ARIMA:")
print(rmse_arima_exp)
print("Expanding Window RMSE for TSLM:")
print(rmse_tslm_exp)


# ===== 2. Rolling Window Forecasts =====
# For a fixed window size (e.g., 60), specify the 'window' argument.
roll_window <- 60

# Redefine the EMPTPA
EMPTPA <- ts(EMPTPA, start = c(1980, 1), frequency = 12)

errors_arima_roll <- tsCV(EMPTPA, forecastfunction = forecast_arima, h = 12, 
                          initial = roll_window, window = roll_window)
errors_tslm_roll  <- tsCV(EMPTPA, forecastfunction = forecast_tslm,  h = 12, 
                          initial = roll_window, window = roll_window)

rmse_arima_roll <- apply(errors_arima_roll, 2, function(x) sqrt(mean(x^2, na.rm = TRUE)))
rmse_tslm_roll  <- apply(errors_tslm_roll,  2, function(x) sqrt(mean(x^2, na.rm = TRUE)))

# Display RMSE results for rolling window forecasts
print("Rolling Window RMSE for ARIMA:")
print(rmse_arima_roll)
print("Rolling Window RMSE for TSLM:")
print(rmse_tslm_roll)


# ===== 3. Diebold-Mariano (DM) Tests =====
# Compare forecast errors at specific horizons using dm.test.
# For example, test at horizon 1 and horizon 12.

# Expanding window DM tests
dm_1_exp  <- dm.test(errors_arima_exp[, 1], errors_tslm_exp[, 1], alternative = "two.sided", h = 1, power = 2)
dm_12_exp <- dm.test(errors_arima_exp[, 12], errors_tslm_exp[, 12], alternative = "two.sided", h = 12, power = 2)

# Rolling window DM tests
dm_1_roll  <- dm.test(errors_arima_roll[, 1], errors_tslm_roll[, 1], alternative = "two.sided", h = 1, power = 2)
dm_12_roll <- dm.test(errors_arima_roll[, 12], errors_tslm_roll[, 12], alternative = "two.sided", h = 12, power = 2)

# Rolling VX Expanding ARIMA
dm_1_roll  <- dm.test(errors_arima_roll[, 1], errors_arima_exp[, 1], alternative = "two.sided", h = 1, power = 2)
dm_12_roll <- dm.test(errors_arima_roll[, 12], errors_arima_exp[, 12], alternative = "two.sided", h = 12, power = 2)
dm_1_roll  
dm_12_roll 

# Print DM test results
print("Diebold-Mariano Test (Expanding Window, 1-step):")
print(dm_1_exp)
print("Diebold-Mariano Test (Expanding Window, 12-step):")
print(dm_12_exp)
print("Diebold-Mariano Test (Rolling Window, 1-step):")
print(dm_1_roll)
print("Diebold-Mariano Test (Rolling Window, 12-step):")
print(dm_12_roll)