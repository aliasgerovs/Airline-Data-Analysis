
# Homework Week 10 (Week 12 Day 2)

library(data.table)
library(tidyverse)
library(inspectdf)
library(tidymodels)
library(modeltime)
library(lubridate)
library(timetk)
library(rsample)
library(forecast)


# Use arima_boost(), exp_smoothing(), prophet_reg() models;

# Load the data

my_data <- read.csv("AirPassengers.csv")

my_data %>% str()

my_data %>% glimpse()

my_data %>% is.na()

my_data %>% inspect_na()

#Trying Model Arima Boost

interactive <- FALSE

splits <- initial_time_split(data, prop = 0.8)

my_data %>% names()

# Model 1: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Passengers ~ Month + as.numeric(Month) + factor(month(Month, label = TRUE), ordered = F),
      data = training(splits))

# Model 2 : ETS

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Passengers ~ Month, data = training(splits))

# Model 3: Prophet

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Passengers ~ Month, data = training(splits))

# 2 Compare RMSE scores on test set 

models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = F
  )

# 3. Make forecast on lowest RMSE score model; 


calibration_tbl <- model_fit_arima_boosted %>%
  modeltime_calibrate(new_data = testing(splits))



calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive = T
  ) 

#4 

refit_tbl <- calibration_tbl %>%
modeltime_refit(data = data)

refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = data) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive = T
  )

