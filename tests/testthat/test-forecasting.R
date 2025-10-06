library(testthat)

# Source the functions
source("../../R/forecasting.R")

# Create sample data for testing
sample_values <- c(100, 102, 99, 101, 103, 98, 105, 107, 104, 106, 108, 110)

test_that("forecast_moving_average works correctly", {
  forecast_result <- forecast_moving_average(sample_values, n_periods = 3, window = 3)
  
  expect_type(forecast_result, "list")
  expect_true("forecast" %in% names(forecast_result))
  expect_true("lower_ci" %in% names(forecast_result))
  expect_true("upper_ci" %in% names(forecast_result))
  expect_true("method" %in% names(forecast_result))
  
  expect_length(forecast_result$forecast, 3)
  expect_length(forecast_result$lower_ci, 3)
  expect_length(forecast_result$upper_ci, 3)
  expect_equal(forecast_result$method, "Moving Average")
  
  # Check that confidence intervals make sense
  expect_true(all(forecast_result$lower_ci <= forecast_result$forecast))
  expect_true(all(forecast_result$forecast <= forecast_result$upper_ci))
})

test_that("forecast_moving_average handles edge cases", {
  # Test with insufficient data
  expect_error(forecast_moving_average(c(100, 101), n_periods = 3, window = 3))
  
  # Test with minimum data
  result <- forecast_moving_average(c(100, 101, 102), n_periods = 1, window = 3)
  expect_length(result$forecast, 1)
})

test_that("forecast_exponential_smoothing works correctly", {
  forecast_result <- forecast_exponential_smoothing(sample_values, n_periods = 3, alpha = 0.3)
  
  expect_type(forecast_result, "list")
  expect_true("forecast" %in% names(forecast_result))
  expect_true("lower_ci" %in% names(forecast_result))
  expect_true("upper_ci" %in% names(forecast_result))
  expect_true("fitted" %in% names(forecast_result))
  expect_true("residuals" %in% names(forecast_result))
  expect_true("method" %in% names(forecast_result))
  
  expect_length(forecast_result$forecast, 3)
  expect_length(forecast_result$fitted, length(sample_values))
  expect_length(forecast_result$residuals, length(sample_values))
  expect_equal(forecast_result$method, "Exponential Smoothing")
})

test_that("forecast_exponential_smoothing handles edge cases", {
  expect_error(forecast_exponential_smoothing(c(100), n_periods = 3))
  
  result <- forecast_exponential_smoothing(c(100, 101), n_periods = 1)
  expect_length(result$forecast, 1)
})

test_that("forecast_linear_trend works correctly", {
  forecast_result <- forecast_linear_trend(sample_values, n_periods = 3)
  
  expect_type(forecast_result, "list")
  expect_true("forecast" %in% names(forecast_result))
  expect_true("lower_ci" %in% names(forecast_result))
  expect_true("upper_ci" %in% names(forecast_result))
  expect_true("model" %in% names(forecast_result))
  expect_true("r_squared" %in% names(forecast_result))
  expect_true("method" %in% names(forecast_result))
  
  expect_length(forecast_result$forecast, 3)
  expect_equal(forecast_result$method, "Linear Trend")
  expect_true(forecast_result$r_squared >= 0 && forecast_result$r_squared <= 1)
})

test_that("forecast_linear_trend handles edge cases", {
  expect_error(forecast_linear_trend(c(100, 101), n_periods = 3))
  
  result <- forecast_linear_trend(c(100, 101, 102), n_periods = 1)
  expect_length(result$forecast, 1)
})

test_that("forecast_seasonal_decompose works correctly", {
  # Create longer series for seasonal decomposition
  long_series <- rep(sample_values, 3)  # 36 points
  
  forecast_result <- forecast_seasonal_decompose(long_series, frequency = 12, n_periods = 3)
  
  expect_type(forecast_result, "list")
  expect_true("forecast" %in% names(forecast_result))
  expect_true("lower_ci" %in% names(forecast_result))
  expect_true("upper_ci" %in% names(forecast_result))
  expect_true("decomposition" %in% names(forecast_result))
  expect_true("method" %in% names(forecast_result))
  
  expect_length(forecast_result$forecast, 3)
  expect_equal(forecast_result$method, "Seasonal Decomposition")
})

test_that("forecast_seasonal_decompose handles insufficient data", {
  # Should fall back to linear trend
  result <- forecast_seasonal_decompose(sample_values, frequency = 12, n_periods = 3)
  expect_equal(result$method, "Linear Trend")
})

test_that("forecast_ensemble works correctly", {
  forecast_result <- forecast_ensemble(sample_values, n_periods = 3, 
                                     methods = c("ma", "es", "trend"))
  
  expect_type(forecast_result, "list")
  expect_true("forecast" %in% names(forecast_result))
  expect_true("lower_ci" %in% names(forecast_result))
  expect_true("upper_ci" %in% names(forecast_result))
  expect_true("individual_forecasts" %in% names(forecast_result))
  expect_true("weights" %in% names(forecast_result))
  expect_true("method" %in% names(forecast_result))
  
  expect_length(forecast_result$forecast, 3)
  expect_equal(forecast_result$method, "Ensemble")
  expect_length(forecast_result$individual_forecasts, 3)
  expect_equal(sum(forecast_result$weights), 1, tolerance = 1e-10)
})

test_that("forecast_ensemble with custom weights works", {
  custom_weights <- c(0.5, 0.3, 0.2)
  forecast_result <- forecast_ensemble(sample_values, n_periods = 3, 
                                     methods = c("ma", "es", "trend"),
                                     weights = custom_weights)
  
  expect_equal(forecast_result$weights, custom_weights)
})

test_that("calculate_forecast_accuracy works correctly", {
  actual <- c(100, 102, 99, 101)
  predicted <- c(101, 103, 98, 102)
  
  accuracy <- calculate_forecast_accuracy(actual, predicted)
  
  expect_type(accuracy, "list")
  expect_true("mae" %in% names(accuracy))
  expect_true("mse" %in% names(accuracy))
  expect_true("rmse" %in% names(accuracy))
  expect_true("mape" %in% names(accuracy))
  expect_true("bias" %in% names(accuracy))
  expect_true("r_squared" %in% names(accuracy))
  
  expect_true(accuracy$mae >= 0)
  expect_true(accuracy$mse >= 0)
  expect_true(accuracy$rmse >= 0)
  expect_true(accuracy$mape >= 0)
  expect_true(accuracy$r_squared >= 0 && accuracy$r_squared <= 1)
})

test_that("calculate_forecast_accuracy handles mismatched lengths", {
  expect_error(calculate_forecast_accuracy(c(1, 2, 3), c(1, 2)))
})

test_that("generate_forecast_scenarios works correctly", {
  base_forecast <- c(100, 102, 104)
  volatility <- 5
  
  scenarios <- generate_forecast_scenarios(base_forecast, volatility, confidence_level = 0.8)
  
  expect_type(scenarios, "list")
  expect_true("optimistic" %in% names(scenarios))
  expect_true("realistic" %in% names(scenarios))
  expect_true("pessimistic" %in% names(scenarios))
  expect_true("confidence_level" %in% names(scenarios))
  
  expect_length(scenarios$optimistic, 3)
  expect_length(scenarios$realistic, 3)
  expect_length(scenarios$pessimistic, 3)
  expect_equal(scenarios$confidence_level, 0.8)
  
  # Check ordering
  expect_true(all(scenarios$pessimistic <= scenarios$realistic))
  expect_true(all(scenarios$realistic <= scenarios$optimistic))
})

test_that("all forecast methods return consistent structure", {
  methods <- list(
    forecast_moving_average(sample_values, 3),
    forecast_exponential_smoothing(sample_values, 3),
    forecast_linear_trend(sample_values, 3)
  )
  
  required_fields <- c("forecast", "lower_ci", "upper_ci", "method")
  
  for (method_result in methods) {
    expect_true(all(required_fields %in% names(method_result)))
    expect_length(method_result$forecast, 3)
    expect_length(method_result$lower_ci, 3)
    expect_length(method_result$upper_ci, 3)
  }
})
