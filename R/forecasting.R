#' Time Series Forecasting for FX Data
#'
#' This module provides forecasting capabilities using various methods
#' including ARIMA, exponential smoothing, and trend analysis.

#' Simple moving average forecast
#'
#' @param values Vector of historical values
#' @param n_periods Number of periods to forecast
#' @param window Moving average window size
#' @return List with forecasts and confidence intervals
#' @export
forecast_moving_average <- function(values, n_periods = 12, window = 3) {
  if (length(values) < window) {
    stop("Not enough data points for moving average")
  }
  
  # Calculate last moving average
  last_ma <- mean(tail(values, window), na.rm = TRUE)
  
  # Simple forecast: repeat last moving average
  forecasts <- rep(last_ma, n_periods)
  
  # Calculate historical volatility for confidence intervals
  historical_volatility <- sd(values, na.rm = TRUE)
  
  # Simple confidence intervals (assuming normal distribution)
  lower_ci <- forecasts - 1.96 * historical_volatility
  upper_ci <- forecasts + 1.96 * historical_volatility
  
  list(
    forecast = forecasts,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    method = "Moving Average"
  )
}

#' Exponential smoothing forecast
#'
#' @param values Vector of historical values
#' @param n_periods Number of periods to forecast
#' @param alpha Smoothing parameter (0 < alpha < 1)
#' @return List with forecasts and confidence intervals
#' @export
forecast_exponential_smoothing <- function(values, n_periods = 12, alpha = 0.3) {
  if (length(values) < 2) {
    stop("Not enough data points for exponential smoothing")
  }
  
  # Initialize
  smoothed <- numeric(length(values))
  smoothed[1] <- values[1]
  
  # Calculate exponentially smoothed values
  for (i in 2:length(values)) {
    smoothed[i] <- alpha * values[i] + (1 - alpha) * smoothed[i-1]
  }
  
  # Forecast: repeat last smoothed value
  last_smoothed <- tail(smoothed, 1)
  forecasts <- rep(last_smoothed, n_periods)
  
  # Calculate residuals for confidence intervals
  residuals <- values - smoothed
  residual_sd <- sd(residuals, na.rm = TRUE)
  
  # Confidence intervals
  lower_ci <- forecasts - 1.96 * residual_sd
  upper_ci <- forecasts + 1.96 * residual_sd
  
  list(
    forecast = forecasts,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    fitted = smoothed,
    residuals = residuals,
    method = "Exponential Smoothing"
  )
}

#' Linear trend forecast
#'
#' @param values Vector of historical values
#' @param n_periods Number of periods to forecast
#' @return List with forecasts and confidence intervals
#' @export
forecast_linear_trend <- function(values, n_periods = 12) {
  if (length(values) < 3) {
    stop("Not enough data points for trend analysis")
  }
  
  # Create time index
  time_index <- 1:length(values)
  
  # Fit linear model
  model <- lm(values ~ time_index)
  
  # Generate future time points
  future_time <- (length(values) + 1):(length(values) + n_periods)
  
  # Predict
  predictions <- predict(model, newdata = data.frame(time_index = future_time), 
                        interval = "prediction", level = 0.95)
  
  list(
    forecast = predictions[, "fit"],
    lower_ci = predictions[, "lwr"],
    upper_ci = predictions[, "upr"],
    model = model,
    r_squared = summary(model)$r.squared,
    method = "Linear Trend"
  )
}

#' Seasonal decomposition and forecast
#'
#' @param values Vector of historical values
#' @param frequency Seasonal frequency (e.g., 12 for monthly data)
#' @param n_periods Number of periods to forecast
#' @return List with decomposition and forecasts
#' @export
forecast_seasonal_decompose <- function(values, frequency = 12, n_periods = 12) {
  if (length(values) < 2 * frequency) {
    warning("Not enough data for reliable seasonal decomposition")
    return(forecast_linear_trend(values, n_periods))
  }
  
  # Create time series object
  ts_data <- ts(values, frequency = frequency)
  
  # Decompose
  decomp <- decompose(ts_data, type = "additive")
  
  # Extract components
  trend <- as.numeric(decomp$trend)
  seasonal <- as.numeric(decomp$seasonal)
  
  # Simple forecast: extend trend and repeat seasonal pattern
  last_trend <- tail(trend[!is.na(trend)], 1)
  trend_slope <- (tail(trend[!is.na(trend)], 1) - head(tail(trend[!is.na(trend)], 2), 1))
  
  # Forecast trend
  future_trend <- last_trend + trend_slope * (1:n_periods)
  
  # Forecast seasonal component (repeat pattern)
  seasonal_pattern <- tail(seasonal, frequency)
  future_seasonal <- rep(seasonal_pattern, length.out = n_periods)
  
  # Combine forecasts
  forecasts <- future_trend + future_seasonal
  
  # Simple confidence intervals based on residual variance
  residuals <- as.numeric(decomp$random)
  residual_sd <- sd(residuals, na.rm = TRUE)
  
  lower_ci <- forecasts - 1.96 * residual_sd
  upper_ci <- forecasts + 1.96 * residual_sd
  
  list(
    forecast = forecasts,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    decomposition = decomp,
    trend = trend,
    seasonal = seasonal,
    method = "Seasonal Decomposition"
  )
}

#' Ensemble forecast combining multiple methods
#'
#' @param values Vector of historical values
#' @param n_periods Number of periods to forecast
#' @param methods Vector of methods to combine
#' @param weights Vector of weights for each method (optional)
#' @return List with ensemble forecasts
#' @export
forecast_ensemble <- function(values, n_periods = 12, 
                             methods = c("ma", "es", "trend"), 
                             weights = NULL) {
  
  forecasts_list <- list()
  
  # Generate forecasts from each method
  if ("ma" %in% methods) {
    forecasts_list$ma <- forecast_moving_average(values, n_periods)
  }
  
  if ("es" %in% methods) {
    forecasts_list$es <- forecast_exponential_smoothing(values, n_periods)
  }
  
  if ("trend" %in% methods) {
    forecasts_list$trend <- forecast_linear_trend(values, n_periods)
  }
  
  if ("seasonal" %in% methods) {
    forecasts_list$seasonal <- forecast_seasonal_decompose(values, n_periods = n_periods)
  }
  
  # Extract forecasts
  forecast_matrix <- sapply(forecasts_list, function(x) x$forecast)
  
  # Set equal weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/length(forecasts_list), length(forecasts_list))
  }
  
  # Weighted average
  ensemble_forecast <- as.numeric(forecast_matrix %*% weights)
  
  # Combine confidence intervals (simple average)
  lower_matrix <- sapply(forecasts_list, function(x) x$lower_ci)
  upper_matrix <- sapply(forecasts_list, function(x) x$upper_ci)
  
  ensemble_lower <- as.numeric(lower_matrix %*% weights)
  ensemble_upper <- as.numeric(upper_matrix %*% weights)
  
  list(
    forecast = ensemble_forecast,
    lower_ci = ensemble_lower,
    upper_ci = ensemble_upper,
    individual_forecasts = forecasts_list,
    weights = weights,
    method = "Ensemble"
  )
}

#' Calculate forecast accuracy metrics
#'
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return List of accuracy metrics
#' @export
calculate_forecast_accuracy <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have the same length")
  }
  
  errors <- actual - predicted
  abs_errors <- abs(errors)
  pct_errors <- abs_errors / abs(actual) * 100
  
  list(
    mae = mean(abs_errors, na.rm = TRUE),  # Mean Absolute Error
    mse = mean(errors^2, na.rm = TRUE),    # Mean Squared Error
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),  # Root Mean Squared Error
    mape = mean(pct_errors, na.rm = TRUE), # Mean Absolute Percentage Error
    bias = mean(errors, na.rm = TRUE),     # Bias
    r_squared = cor(actual, predicted, use = "complete.obs")^2
  )
}

#' Generate forecast scenarios (optimistic, pessimistic, realistic)
#'
#' @param base_forecast Base forecast values
#' @param volatility Historical volatility
#' @param confidence_level Confidence level for scenarios
#' @return List with scenario forecasts
#' @export
generate_forecast_scenarios <- function(base_forecast, volatility, confidence_level = 0.8) {
  z_score <- qnorm((1 + confidence_level) / 2)
  
  list(
    optimistic = base_forecast + z_score * volatility,
    realistic = base_forecast,
    pessimistic = base_forecast - z_score * volatility,
    confidence_level = confidence_level
  )
}
