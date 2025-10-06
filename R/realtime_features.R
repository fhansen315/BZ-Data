#' Real-time Features and Live Data Simulation
#'
#' This module provides real-time data simulation, streaming capabilities,
#' and live dashboard features for FX analytics.

#' Simulate real-time FX data stream
#'
#' @param base_data Base historical data
#' @param n_new_points Number of new points to generate
#' @param volatility_multiplier Multiplier for volatility in new data
#' @param trend_continuation Whether to continue existing trends
#' @return Data frame with new simulated data points
#' @export
simulate_realtime_data <- function(base_data, n_new_points = 1, 
                                  volatility_multiplier = 1.2, 
                                  trend_continuation = TRUE) {
  
  if (nrow(base_data) == 0) {
    stop("Base data cannot be empty")
  }
  
  # Get the latest date and values
  latest_date <- max(base_data$date)
  
  # Generate new dates (assuming monthly data)
  new_dates <- seq.Date(from = latest_date + months(1), 
                       length.out = n_new_points, 
                       by = "month")
  
  result <- data.frame()
  
  # Generate data for each institution-modality combination
  combinations <- base_data %>%
    select(institution, modality) %>%
    distinct()
  
  for (i in 1:nrow(combinations)) {
    inst <- combinations$institution[i]
    mod <- combinations$modality[i]
    
    # Get historical data for this combination
    hist_data <- base_data %>%
      filter(institution == inst, modality == mod) %>%
      arrange(date)
    
    if (nrow(hist_data) == 0) next
    
    # Calculate recent trend and volatility
    recent_values <- tail(hist_data$value, min(6, nrow(hist_data)))
    recent_volatility <- sd(recent_values, na.rm = TRUE)
    
    # Calculate trend
    if (trend_continuation && length(recent_values) >= 3) {
      trend_slope <- (tail(recent_values, 1) - head(recent_values, 1)) / length(recent_values)
    } else {
      trend_slope <- 0
    }
    
    # Generate new values
    last_value <- tail(hist_data$value, 1)
    new_values <- numeric(n_new_points)
    
    for (j in 1:n_new_points) {
      # Base value with trend
      base_val <- last_value + trend_slope * j
      
      # Add volatility
      noise <- rnorm(1, 0, recent_volatility * volatility_multiplier)
      
      # Ensure non-negative values
      new_values[j] <- max(0, base_val + noise)
      last_value <- new_values[j]  # Update for next iteration
    }
    
    # Create new data frame
    new_data <- data.frame(
      date = new_dates,
      institution = inst,
      modality = mod,
      value = round(new_values, 0),
      is_realtime = TRUE  # Flag to identify real-time data
    )
    
    result <- rbind(result, new_data)
  }
  
  return(result)
}

#' Create live dashboard data with alerts
#'
#' @param current_data Current data frame
#' @param alert_thresholds List of alert thresholds
#' @return List with dashboard data and alerts
#' @export
create_live_dashboard_data <- function(current_data, 
                                      alert_thresholds = list(
                                        volume_change = 0.2,
                                        volatility_spike = 2.0,
                                        anomaly_score = 3.0
                                      )) {
  
  # Calculate key metrics
  latest_date <- max(current_data$date)
  previous_date <- sort(unique(current_data$date), decreasing = TRUE)[2]
  
  # Current vs previous period comparison
  current_metrics <- current_data %>%
    filter(date == latest_date) %>%
    group_by(institution, modality) %>%
    summarize(current_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  previous_metrics <- current_data %>%
    filter(date == previous_date) %>%
    group_by(institution, modality) %>%
    summarize(previous_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Combine and calculate changes
  comparison <- current_metrics %>%
    left_join(previous_metrics, by = c("institution", "modality")) %>%
    mutate(
      previous_value = coalesce(previous_value, current_value),
      pct_change = (current_value - previous_value) / previous_value,
      abs_change = current_value - previous_value
    )
  
  # Generate alerts
  alerts <- list()
  
  # Volume change alerts
  volume_alerts <- comparison %>%
    filter(abs(pct_change) > alert_thresholds$volume_change) %>%
    mutate(
      alert_type = "Volume Change",
      alert_message = paste0(institution, " - ", modality, ": ", 
                           ifelse(pct_change > 0, "↑", "↓"), " ",
                           round(abs(pct_change) * 100, 1), "%"),
      severity = ifelse(abs(pct_change) > 0.5, "high", 
                       ifelse(abs(pct_change) > 0.3, "medium", "low")),
      timestamp = Sys.time()
    )
  
  if (nrow(volume_alerts) > 0) {
    alerts$volume_changes <- volume_alerts
  }
  
  # Calculate rolling volatility for volatility alerts
  volatility_data <- current_data %>%
    arrange(institution, modality, date) %>%
    group_by(institution, modality) %>%
    mutate(
      rolling_vol = calculate_rolling_volatility(value, window = 3, annualize = FALSE)
    ) %>%
    filter(!is.na(rolling_vol)) %>%
    group_by(institution, modality) %>%
    summarize(
      current_vol = tail(rolling_vol, 1),
      avg_vol = mean(rolling_vol, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(vol_ratio = current_vol / avg_vol)
  
  # Volatility spike alerts
  vol_alerts <- volatility_data %>%
    filter(vol_ratio > alert_thresholds$volatility_spike) %>%
    mutate(
      alert_type = "Volatility Spike",
      alert_message = paste0(institution, " - ", modality, ": Volatility ", 
                           round(vol_ratio, 1), "x normal"),
      severity = ifelse(vol_ratio > 3, "high", "medium"),
      timestamp = Sys.time()
    )
  
  if (nrow(vol_alerts) > 0) {
    alerts$volatility_spikes <- vol_alerts
  }
  
  # Anomaly detection alerts
  anomaly_data <- current_data %>%
    group_by(date) %>%
    summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    mutate(
      z_score = abs(scale(total_value)),
      is_anomaly = z_score > alert_thresholds$anomaly_score
    ) %>%
    filter(is_anomaly, date == max(date))
  
  if (nrow(anomaly_data) > 0) {
    anomaly_alerts <- data.frame(
      alert_type = "Anomaly Detected",
      alert_message = paste0("Total volume anomaly detected: Z-score = ", 
                           round(anomaly_data$z_score, 2)),
      severity = "high",
      timestamp = Sys.time()
    )
    alerts$anomalies <- anomaly_alerts
  }
  
  # Combine all alerts
  all_alerts <- do.call(rbind, alerts)
  if (is.null(all_alerts)) {
    all_alerts <- data.frame(
      alert_type = character(0),
      alert_message = character(0),
      severity = character(0),
      timestamp = as.POSIXct(character(0))
    )
  }
  
  # Create dashboard summary
  dashboard_summary <- list(
    total_volume = sum(current_metrics$current_value, na.rm = TRUE),
    total_change = sum(comparison$abs_change, na.rm = TRUE),
    avg_change_pct = mean(comparison$pct_change, na.rm = TRUE),
    active_institutions = length(unique(current_data$institution)),
    active_modalities = length(unique(current_data$modality)),
    last_update = latest_date,
    alert_count = nrow(all_alerts),
    high_severity_alerts = sum(all_alerts$severity == "high", na.rm = TRUE)
  )
  
  return(list(
    summary = dashboard_summary,
    comparison = comparison,
    alerts = all_alerts,
    volatility_data = volatility_data
  ))
}

#' Generate market status indicators
#'
#' @param current_data Current market data
#' @param historical_data Historical data for comparison
#' @return List with market status indicators
#' @export
generate_market_status <- function(current_data, historical_data) {
  
  # Calculate current market metrics
  current_total <- sum(current_data$value, na.rm = TRUE)
  historical_avg <- mean(tapply(historical_data$value, historical_data$date, sum), na.rm = TRUE)
  
  # Market sentiment based on volume
  volume_ratio <- current_total / historical_avg
  
  market_sentiment <- case_when(
    volume_ratio > 1.2 ~ "Bullish",
    volume_ratio > 0.8 ~ "Neutral",
    TRUE ~ "Bearish"
  )
  
  # Market volatility status
  current_vol <- sd(current_data$value, na.rm = TRUE)
  historical_vol <- sd(historical_data$value, na.rm = TRUE)
  vol_ratio <- current_vol / historical_vol
  
  volatility_status <- case_when(
    vol_ratio > 1.5 ~ "High",
    vol_ratio > 0.7 ~ "Normal",
    TRUE ~ "Low"
  )
  
  # Market activity level
  activity_level <- case_when(
    volume_ratio > 1.3 ~ "Very Active",
    volume_ratio > 1.1 ~ "Active",
    volume_ratio > 0.9 ~ "Normal",
    volume_ratio > 0.7 ~ "Quiet",
    TRUE ~ "Very Quiet"
  )
  
  # Risk level assessment
  risk_score <- (vol_ratio * 0.6) + (abs(1 - volume_ratio) * 0.4)
  
  risk_level <- case_when(
    risk_score > 1.5 ~ "High",
    risk_score > 1.0 ~ "Medium",
    TRUE ~ "Low"
  )
  
  return(list(
    sentiment = market_sentiment,
    volatility = volatility_status,
    activity = activity_level,
    risk = risk_level,
    volume_ratio = volume_ratio,
    volatility_ratio = vol_ratio,
    risk_score = risk_score,
    timestamp = Sys.time()
  ))
}

#' Create real-time performance metrics
#'
#' @param data Current data
#' @param window_size Rolling window size for calculations
#' @return Data frame with performance metrics
#' @export
calculate_realtime_performance <- function(data, window_size = 6) {
  
  performance_metrics <- data %>%
    arrange(institution, modality, date) %>%
    group_by(institution, modality) %>%
    mutate(
      returns = (value - lag(value)) / lag(value),
      rolling_return = zoo::rollapply(returns, width = window_size, FUN = mean, 
                                     na.rm = TRUE, fill = NA, align = "right"),
      rolling_vol = zoo::rollapply(returns, width = window_size, FUN = sd, 
                                  na.rm = TRUE, fill = NA, align = "right"),
      rolling_sharpe = rolling_return / rolling_vol * sqrt(12),  # Annualized
      momentum = (value - lag(value, window_size)) / lag(value, window_size)
    ) %>%
    filter(!is.na(rolling_return)) %>%
    group_by(institution, modality) %>%
    summarize(
      latest_return = tail(rolling_return, 1),
      latest_volatility = tail(rolling_vol, 1),
      latest_sharpe = tail(rolling_sharpe, 1),
      latest_momentum = tail(momentum, 1),
      trend_direction = ifelse(tail(momentum, 1) > 0, "Up", "Down"),
      .groups = "drop"
    )
  
  return(performance_metrics)
}

#' Generate real-time alerts based on thresholds
#'
#' @param metrics Performance metrics
#' @param thresholds Alert thresholds
#' @return Data frame with alerts
#' @export
generate_realtime_alerts <- function(metrics, 
                                    thresholds = list(
                                      high_volatility = 0.15,
                                      low_sharpe = 0.5,
                                      high_momentum = 0.2
                                    )) {
  
  alerts <- data.frame()
  
  # High volatility alerts
  high_vol <- metrics %>%
    filter(latest_volatility > thresholds$high_volatility) %>%
    mutate(
      alert_type = "High Volatility",
      message = paste0(institution, " - ", modality, ": Volatility = ", 
                      round(latest_volatility * 100, 2), "%"),
      severity = "medium"
    )
  
  # Low Sharpe ratio alerts
  low_sharpe <- metrics %>%
    filter(latest_sharpe < thresholds$low_sharpe, !is.na(latest_sharpe)) %>%
    mutate(
      alert_type = "Low Risk-Adjusted Return",
      message = paste0(institution, " - ", modality, ": Sharpe = ", 
                      round(latest_sharpe, 2)),
      severity = "low"
    )
  
  # High momentum alerts (both positive and negative)
  high_momentum <- metrics %>%
    filter(abs(latest_momentum) > thresholds$high_momentum, !is.na(latest_momentum)) %>%
    mutate(
      alert_type = "High Momentum",
      message = paste0(institution, " - ", modality, ": ", 
                      ifelse(latest_momentum > 0, "Strong upward", "Strong downward"),
                      " momentum (", round(latest_momentum * 100, 1), "%)"),
      severity = ifelse(abs(latest_momentum) > 0.3, "high", "medium")
    )
  
  # Combine all alerts
  all_alerts <- rbind(
    high_vol %>% select(institution, modality, alert_type, message, severity),
    low_sharpe %>% select(institution, modality, alert_type, message, severity),
    high_momentum %>% select(institution, modality, alert_type, message, severity)
  )
  
  if (nrow(all_alerts) > 0) {
    all_alerts$timestamp <- Sys.time()
  }
  
  return(all_alerts)
}
