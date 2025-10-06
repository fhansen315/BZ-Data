#' Advanced Financial Analytics for FX Data
#'
#' This module provides sophisticated financial analysis functions including
#' risk metrics, volatility analysis, and performance calculations.

#' Calculate Value at Risk (VaR)
#'
#' @param returns Vector of returns
#' @param confidence Confidence level (default: 0.95)
#' @param method Method: "historical", "parametric", or "monte_carlo"
#' @return VaR value
#' @export
calculate_var <- function(returns, confidence = 0.95, method = "historical") {
  if (length(returns) == 0) return(NA)
  
  switch(method,
    "historical" = {
      quantile(returns, 1 - confidence, na.rm = TRUE)
    },
    "parametric" = {
      mean_return <- mean(returns, na.rm = TRUE)
      sd_return <- sd(returns, na.rm = TRUE)
      qnorm(1 - confidence) * sd_return + mean_return
    },
    "monte_carlo" = {
      # Simple Monte Carlo simulation
      n_sim <- 10000
      mean_return <- mean(returns, na.rm = TRUE)
      sd_return <- sd(returns, na.rm = TRUE)
      simulated <- rnorm(n_sim, mean_return, sd_return)
      quantile(simulated, 1 - confidence)
    }
  )
}

#' Calculate Expected Shortfall (Conditional VaR)
#'
#' @param returns Vector of returns
#' @param confidence Confidence level (default: 0.95)
#' @return Expected Shortfall value
#' @export
calculate_expected_shortfall <- function(returns, confidence = 0.95) {
  if (length(returns) == 0) return(NA)
  
  var_threshold <- quantile(returns, 1 - confidence, na.rm = TRUE)
  mean(returns[returns <= var_threshold], na.rm = TRUE)
}

#' Calculate rolling volatility
#'
#' @param values Vector of values
#' @param window Rolling window size (default: 30)
#' @param annualize Whether to annualize volatility (default: TRUE)
#' @return Vector of rolling volatilities
#' @export
calculate_rolling_volatility <- function(values, window = 30, annualize = TRUE) {
  if (length(values) < window) return(rep(NA, length(values)))
  
  returns <- diff(log(values))
  rolling_vol <- zoo::rollapply(returns, width = window, FUN = sd, na.rm = TRUE, 
                                fill = NA, align = "right")
  
  if (annualize) {
    rolling_vol <- rolling_vol * sqrt(252)  # Assuming daily data
  }
  
  c(NA, rolling_vol)  # Add NA for first observation
}

#' Calculate Sharpe ratio
#'
#' @param returns Vector of returns
#' @param risk_free_rate Risk-free rate (default: 0.02)
#' @return Sharpe ratio
#' @export
calculate_sharpe_ratio <- function(returns, risk_free_rate = 0.02) {
  if (length(returns) == 0) return(NA)
  
  excess_returns <- returns - risk_free_rate / 252  # Daily risk-free rate
  mean(excess_returns, na.rm = TRUE) / sd(excess_returns, na.rm = TRUE) * sqrt(252)
}

#' Calculate maximum drawdown
#'
#' @param values Vector of cumulative values
#' @return List with max drawdown, start date, and end date indices
#' @export
calculate_max_drawdown <- function(values) {
  if (length(values) == 0) return(list(max_dd = NA, start = NA, end = NA))
  
  cummax_values <- cummax(values)
  drawdowns <- (values - cummax_values) / cummax_values
  
  max_dd <- min(drawdowns, na.rm = TRUE)
  max_dd_idx <- which.min(drawdowns)
  
  # Find the peak before the max drawdown
  peak_idx <- which.max(cummax_values[1:max_dd_idx])
  
  list(
    max_dd = abs(max_dd),
    start = peak_idx,
    end = max_dd_idx,
    value = max_dd
  )
}

#' Calculate correlation matrix with significance tests
#'
#' @param data Data frame with numeric columns
#' @param method Correlation method: "pearson", "spearman", "kendall"
#' @return List with correlation matrix and p-values
#' @export
calculate_correlation_matrix <- function(data, method = "pearson") {
  if (ncol(data) < 2) return(NULL)
  
  cor_matrix <- cor(data, use = "complete.obs", method = method)
  
  # Calculate p-values
  n <- nrow(data)
  p_values <- matrix(NA, ncol(data), ncol(data))
  colnames(p_values) <- colnames(data)
  rownames(p_values) <- colnames(data)
  
  for (i in 1:ncol(data)) {
    for (j in 1:ncol(data)) {
      if (i != j) {
        test_result <- cor.test(data[, i], data[, j], method = method)
        p_values[i, j] <- test_result$p.value
      } else {
        p_values[i, j] <- 0
      }
    }
  }
  
  list(
    correlation = cor_matrix,
    p_values = p_values,
    significant = p_values < 0.05
  )
}

#' Detect anomalies using statistical methods
#'
#' @param values Vector of values
#' @param method Method: "zscore", "iqr", or "isolation_forest"
#' @param threshold Threshold for anomaly detection
#' @return Logical vector indicating anomalies
#' @export
detect_anomalies <- function(values, method = "zscore", threshold = 3) {
  if (length(values) == 0) return(logical(0))
  
  switch(method,
    "zscore" = {
      z_scores <- abs(scale(values))
      z_scores > threshold
    },
    "iqr" = {
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      values < lower_bound | values > upper_bound
    },
    "modified_zscore" = {
      median_val <- median(values, na.rm = TRUE)
      mad_val <- mad(values, na.rm = TRUE)
      modified_z <- 0.6745 * (values - median_val) / mad_val
      abs(modified_z) > threshold
    }
  )
}

#' Calculate portfolio metrics
#'
#' @param weights Vector of portfolio weights
#' @param returns Matrix of returns (assets in columns)
#' @param cov_matrix Covariance matrix (optional)
#' @return List of portfolio metrics
#' @export
calculate_portfolio_metrics <- function(weights, returns, cov_matrix = NULL) {
  if (is.null(cov_matrix)) {
    cov_matrix <- cov(returns, use = "complete.obs")
  }
  
  portfolio_return <- sum(weights * colMeans(returns, na.rm = TRUE))
  portfolio_variance <- t(weights) %*% cov_matrix %*% weights
  portfolio_volatility <- sqrt(portfolio_variance)
  
  list(
    expected_return = portfolio_return,
    volatility = as.numeric(portfolio_volatility),
    variance = as.numeric(portfolio_variance),
    sharpe_ratio = portfolio_return / as.numeric(portfolio_volatility)
  )
}

#' Calculate beta coefficient
#'
#' @param asset_returns Vector of asset returns
#' @param market_returns Vector of market returns
#' @return Beta coefficient
#' @export
calculate_beta <- function(asset_returns, market_returns) {
  if (length(asset_returns) != length(market_returns)) {
    stop("Asset and market returns must have the same length")
  }
  
  cov(asset_returns, market_returns, use = "complete.obs") / 
    var(market_returns, na.rm = TRUE)
}

#' Calculate information ratio
#'
#' @param portfolio_returns Vector of portfolio returns
#' @param benchmark_returns Vector of benchmark returns
#' @return Information ratio
#' @export
calculate_information_ratio <- function(portfolio_returns, benchmark_returns) {
  active_returns <- portfolio_returns - benchmark_returns
  mean(active_returns, na.rm = TRUE) / sd(active_returns, na.rm = TRUE)
}
