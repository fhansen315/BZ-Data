library(testthat)
library(dplyr)

# Source the functions
source("../../R/financial_analytics.R")

# Create sample data for testing
sample_returns <- c(0.01, -0.02, 0.015, -0.01, 0.005, -0.008, 0.02, -0.015, 0.01, 0.003)
sample_values <- c(100, 102, 99.98, 101.48, 100.47, 100.97, 100.16, 102.16, 100.63, 101.63)

test_that("calculate_var works correctly", {
  var_hist <- calculate_var(sample_returns, 0.95, "historical")
  var_param <- calculate_var(sample_returns, 0.95, "parametric")
  
  expect_type(var_hist, "double")
  expect_type(var_param, "double")
  expect_true(var_hist < 0)  # VaR should be negative
  expect_true(var_param < 0)
  expect_length(var_hist, 1)
  expect_length(var_param, 1)
})

test_that("calculate_var handles edge cases", {
  expect_true(is.na(calculate_var(numeric(0), 0.95, "historical")))
  expect_type(calculate_var(c(0.01), 0.95, "historical"), "double")
})

test_that("calculate_expected_shortfall works correctly", {
  es <- calculate_expected_shortfall(sample_returns, 0.95)
  
  expect_type(es, "double")
  expect_length(es, 1)
  expect_true(es < 0)  # Expected shortfall should be negative
})

test_that("calculate_rolling_volatility works correctly", {
  rolling_vol <- calculate_rolling_volatility(sample_values, window = 3, annualize = FALSE)
  
  expect_type(rolling_vol, "double")
  expect_length(rolling_vol, length(sample_values))
  expect_true(is.na(rolling_vol[1]))  # First value should be NA
  expect_true(all(rolling_vol[!is.na(rolling_vol)] >= 0))  # Volatility should be non-negative
})

test_that("calculate_sharpe_ratio works correctly", {
  sharpe <- calculate_sharpe_ratio(sample_returns, risk_free_rate = 0.02)
  
  expect_type(sharpe, "double")
  expect_length(sharpe, 1)
  expect_false(is.na(sharpe))
})

test_that("calculate_max_drawdown works correctly", {
  cumulative_values <- cumprod(1 + sample_returns) * 100
  dd_result <- calculate_max_drawdown(cumulative_values)
  
  expect_type(dd_result, "list")
  expect_true("max_dd" %in% names(dd_result))
  expect_true("start" %in% names(dd_result))
  expect_true("end" %in% names(dd_result))
  expect_true(dd_result$max_dd >= 0)  # Max drawdown should be positive
})

test_that("calculate_correlation_matrix works correctly", {
  test_data <- data.frame(
    asset1 = sample_returns[1:8],
    asset2 = sample_returns[2:9],
    asset3 = sample_returns[3:10]
  )
  
  corr_result <- calculate_correlation_matrix(test_data)
  
  expect_type(corr_result, "list")
  expect_true("correlation" %in% names(corr_result))
  expect_true("p_values" %in% names(corr_result))
  expect_equal(dim(corr_result$correlation), c(3, 3))
  expect_true(all(diag(corr_result$correlation) == 1))  # Diagonal should be 1
})

test_that("detect_anomalies works correctly", {
  test_values <- c(sample_values, 200, 50)  # Add obvious outliers
  
  anomalies_zscore <- detect_anomalies(test_values, method = "zscore", threshold = 2)
  anomalies_iqr <- detect_anomalies(test_values, method = "iqr")
  
  expect_type(anomalies_zscore, "logical")
  expect_type(anomalies_iqr, "logical")
  expect_length(anomalies_zscore, length(test_values))
  expect_length(anomalies_iqr, length(test_values))
  expect_true(any(anomalies_zscore))  # Should detect the outliers
  expect_true(any(anomalies_iqr))
})

test_that("calculate_portfolio_metrics works correctly", {
  returns_matrix <- matrix(sample_returns[1:6], nrow = 3, ncol = 2)
  weights <- c(0.6, 0.4)
  
  portfolio_metrics <- calculate_portfolio_metrics(weights, returns_matrix)
  
  expect_type(portfolio_metrics, "list")
  expect_true("expected_return" %in% names(portfolio_metrics))
  expect_true("volatility" %in% names(portfolio_metrics))
  expect_true("variance" %in% names(portfolio_metrics))
  expect_true("sharpe_ratio" %in% names(portfolio_metrics))
  expect_true(portfolio_metrics$volatility >= 0)
  expect_true(portfolio_metrics$variance >= 0)
})

test_that("calculate_beta works correctly", {
  asset_returns <- sample_returns[1:8]
  market_returns <- sample_returns[2:9]
  
  beta <- calculate_beta(asset_returns, market_returns)
  
  expect_type(beta, "double")
  expect_length(beta, 1)
  expect_false(is.na(beta))
})

test_that("calculate_information_ratio works correctly", {
  portfolio_returns <- sample_returns[1:8]
  benchmark_returns <- sample_returns[2:9]
  
  info_ratio <- calculate_information_ratio(portfolio_returns, benchmark_returns)
  
  expect_type(info_ratio, "double")
  expect_length(info_ratio, 1)
  expect_false(is.na(info_ratio))
})

test_that("functions handle empty inputs gracefully", {
  expect_true(is.na(calculate_var(numeric(0))))
  expect_true(is.na(calculate_expected_shortfall(numeric(0))))
  expect_equal(length(calculate_rolling_volatility(numeric(0))), 0)
  expect_true(is.na(calculate_sharpe_ratio(numeric(0))))
  expect_equal(length(detect_anomalies(numeric(0))), 0)
})

test_that("functions handle single value inputs", {
  single_return <- 0.01
  single_value <- 100
  
  expect_type(calculate_var(single_return), "double")
  expect_type(calculate_expected_shortfall(single_return), "double")
  expect_length(calculate_rolling_volatility(single_value, window = 1), 1)
  expect_type(calculate_sharpe_ratio(single_return), "double")
  expect_length(detect_anomalies(single_value), 1)
})
