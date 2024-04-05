# Data Import -------------------------------------------------------------
# Set working directory (modify file path to location of dataset)
setwd("~/Documents/ACTL4001/SOA/Materials")

# Load necessary libraries
library(forecast)
library(readr)
library(TSstudio)

# Load the data from the CSV file
econ <- read_csv("srcsc-2024-lumaria-economic-data.csv", skip=5)

# View the structure of the dataset
str(econ)

# Data Cleaning -----------------------------------------------------------
# Identify blank columns
blank_columns <- which(colSums(!is.na(econ) & econ != "") == 0)

# Remove blank columns
econ <- econ[,-blank_columns]

# Check data
summary(econ)

# Convert character variables to numeric
econ$Inflation <- as.numeric(sub("%","",econ$Inflation))/100
econ$`Government of Lumaria Overnight Rate` <- as.numeric(sub("%","",econ$`Government of Lumaria Overnight Rate`))/100
econ$`1-yr Risk Free Annual Spot Rate` <- as.numeric(sub("%","",econ$`1-yr Risk Free Annual Spot Rate`))/100
econ$`10-yr Risk Free Annual Spot Rate` <- as.numeric(sub("%","",econ$`10-yr Risk Free Annual Spot Rate`))/100

# Data Analysis -----------------------------------------------------------
# Let's focus on the 10-yr Risk Free Annual Spot Rate
# Ensure the year column is treated as a Date or time object for the time series
econ$Year <- as.Date(paste0(as.character(econ$Year),"-12-31"))

# The ts() function converts a numeric vector into an R time series object
# We are starting from 1962, and the frequency is 1 as the data is annual
ts_data1 <- ts(econ$`10-yr Risk Free Annual Spot Rate`, start = 1962, frequency = 1)

# Explore the data
ts_info(ts_data1)
ts_plot(ts_data1, title = "10-Year Risk Free Annual Spot Rate")

# Fit an ARIMA model
# auto.arima automatically selects the best parameters for the model
fit1 <- auto.arima(ts_data1)

# Forecast the next 60 years
forecasted_values1 <- forecast(fit1, h = 60)

# Plot the forecast
plot(forecasted_values1, main = "Forecasted 10-Year Risk Free Annual Spot Rate")

# The forecasted_values object contains the predictions, lower and upper confidence intervals, etc.
# To access the forecasted rates directly:
print(forecasted_values1$mean)


# Let's focus on the Inflation Rate
# The ts() function converts a numeric vector into an R time series object
# We are starting from 1962, and the frequency is 1 as the data is annual
ts_data2 <- ts(econ$Inflation, start = 1962, frequency = 1)

# Explore the data
ts_info(ts_data2)
ts_plot(ts_data2, title = "Inflation")

# Fit an ARIMA model
# auto.arima automatically selects the best parameters for the model
fit2 <- auto.arima(ts_data2)

# Forecast the next 60 years
forecasted_values2 <- forecast(fit2, h = 60)

# Plot the forecast
plot(forecasted_values2, main = "Forecasted Inflation")

# The forecasted_values object contains the predictions, lower and upper confidence intervals, etc.
# To access the forecasted rates directly:
print(forecasted_values2$mean)


# Let's focus on the Government of Lumaria Overnight Rate
# The ts() function converts a numeric vector into an R time series object
# We are starting from 1962, and the frequency is 1 as the data is annual
ts_data3 <- ts(econ$`Government of Lumaria Overnight Rate`, start = 1962, frequency = 1)

# Explore the data
ts_info(ts_data3)
ts_plot(ts_data3, title = "Government of Lumaria Overnight Rate")

# Fit an ARIMA model
# auto.arima automatically selects the best parameters for the model
fit3 <- auto.arima(ts_data3)

# Forecast the next 60 years
forecasted_values3 <- forecast(fit3, h = 60)

# Plot the forecast
plot(forecasted_values3, main = "Forecasted Government of Lumaria Overnight Rate")

# The forecasted_values object contains the predictions, lower and upper confidence intervals, etc.
# To access the forecasted rates directly:
print(forecasted_values3$mean)


# Let's focus on the 1-yr Risk Free Annual Spot Rate
# The ts() function converts a numeric vector into an R time series object
# We are starting from 1962, and the frequency is 1 as the data is annual
ts_data4 <- ts(econ$`1-yr Risk Free Annual Spot Rate`, start = 1962, frequency = 1)

# Explore the data
ts_info(ts_data4)
ts_plot(ts_data4, title = "1-yr Risk Free Annual Spot Rate")

# Fit an ARIMA model
# auto.arima automatically selects the best parameters for the model
fit4 <- auto.arima(ts_data4)

# Forecast the next 60 years
forecasted_values4 <- forecast(fit4, h = 60)

# Plot the forecast
plot(forecasted_values4, main = "Forecasted 1-yr Risk Free Annual Spot Rate")

# The forecasted_values object contains the predictions, lower and upper confidence intervals, etc.
# To access the forecasted rates directly:
print(forecasted_values4$mean)

