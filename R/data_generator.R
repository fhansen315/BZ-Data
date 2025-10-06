#' Advanced Data Generation for FX Analytics
#'
#' This module provides functions to generate realistic FX data with
#' various patterns, seasonality, and market conditions.

#' Generate realistic FX data with trends and volatility
#'
#' @param start_date Start date for data generation
#' @param end_date End date for data generation
#' @param institutions Vector of institution names
#' @param modalities Vector of modality names
#' @param base_volumes Named vector of base volumes by modality
#' @param volatility Overall volatility level (0-1)
#' @param trend_strength Trend strength (-1 to 1)
#' @param seasonality Whether to include seasonal patterns
#' @return Data frame with generated FX data
#' @export
generate_fx_data <- function(start_date = as.Date("2023-01-01"),
                            end_date = as.Date("2024-12-31"),
                            institutions = c("Bank_Alpha", "Bank_Beta", "Bank_Gamma", "Bank_Delta"),
                            modalities = c("Spot", "Forward", "Swap", "Options"),
                            base_volumes = c(Spot = 150000, Forward = 100000, Swap = 180000, Options = 50000),
                            volatility = 0.15,
                            trend_strength = 0.05,
                            seasonality = TRUE) {
  
  # Generate date sequence
  dates <- seq.Date(from = start_date, to = end_date, by = "month")
  n_periods <- length(dates)
  
  # Initialize result data frame
  result <- data.frame()
  
  # Generate data for each institution and modality
  for (inst in institutions) {
    for (mod in modalities) {
      
      # Base volume for this modality
      base_vol <- base_volumes[mod]
      
      # Institution-specific multiplier (some banks are larger)
      inst_multiplier <- switch(inst,
        "Bank_Alpha" = 1.0,
        "Bank_Beta" = 0.8,
        "Bank_Gamma" = 1.2,
        "Bank_Delta" = 0.9,
        1.0  # default
      )
      
      # Modality-specific growth trends
      mod_trend <- switch(mod,
        "Spot" = trend_strength * 0.8,
        "Forward" = trend_strength * 1.2,
        "Swap" = trend_strength * 1.0,
        "Options" = trend_strength * 1.5,
        trend_strength  # default
      )
      
      # Generate time series
      values <- numeric(n_periods)
      
      for (i in 1:n_periods) {
        # Base value with institution multiplier
        base_value <- base_vol * inst_multiplier
        
        # Add trend component
        trend_component <- base_value * mod_trend * (i - 1) / n_periods
        
        # Add seasonal component
        seasonal_component <- 0
        if (seasonality) {
          month <- as.numeric(format(dates[i], "%m"))
          # Different seasonal patterns by modality
          if (mod == "Spot") {
            seasonal_component <- base_value * 0.1 * sin(2 * pi * month / 12)
          } else if (mod == "Forward") {
            seasonal_component <- base_value * 0.05 * cos(2 * pi * month / 12)
          } else if (mod == "Swap") {
            seasonal_component <- base_value * 0.08 * sin(2 * pi * month / 12 + pi/4)
          } else if (mod == "Options") {
            seasonal_component <- base_value * 0.12 * cos(2 * pi * month / 12 + pi/2)
          }
        }
        
        # Add random component
        random_component <- base_value * volatility * rnorm(1)
        
        # Combine components
        values[i] <- max(0, base_value + trend_component + seasonal_component + random_component)
      }
      
      # Create data frame for this combination
      temp_df <- data.frame(
        date = dates,
        institution = inst,
        modality = mod,
        value = round(values, 0)
      )
      
      result <- rbind(result, temp_df)
    }
  }
  
  return(result)
}

#' Generate market stress scenario data
#'
#' @param base_data Base data frame to modify
#' @param stress_start Start date of stress period
#' @param stress_end End date of stress period
#' @param stress_magnitude Magnitude of stress (0-1)
#' @return Modified data frame with stress scenario
#' @export
generate_stress_scenario <- function(base_data, 
                                   stress_start = as.Date("2024-06-01"),
                                   stress_end = as.Date("2024-08-31"),
                                   stress_magnitude = 0.3) {
  
  result <- base_data
  
  # Apply stress to data within stress period
  stress_mask <- result$date >= stress_start & result$date <= stress_end
  
  if (sum(stress_mask) > 0) {
    # Different stress impacts by modality
    for (mod in unique(result$modality)) {
      mod_mask <- result$modality == mod & stress_mask
      
      if (sum(mod_mask) > 0) {
        stress_factor <- switch(mod,
          "Spot" = 1 - stress_magnitude * 0.8,      # Spot less affected
          "Forward" = 1 - stress_magnitude * 1.2,   # Forward more affected
          "Swap" = 1 - stress_magnitude * 1.0,      # Swap moderately affected
          "Options" = 1 - stress_magnitude * 1.5,   # Options most affected
          1 - stress_magnitude  # default
        )
        
        result$value[mod_mask] <- result$value[mod_mask] * stress_factor
      }
    }
  }
  
  return(result)
}

#' Generate high-frequency intraday data
#'
#' @param date Single date for intraday data
#' @param institution Institution name
#' @param modality Modality name
#' @param base_volume Base daily volume
#' @param n_transactions Number of transactions per day
#' @return Data frame with intraday transactions
#' @export
generate_intraday_data <- function(date, institution, modality, base_volume, n_transactions = 50) {
  
  # Generate random transaction times throughout the day
  hours <- runif(n_transactions, min = 9, max = 17)  # 9 AM to 5 PM
  minutes <- runif(n_transactions, min = 0, max = 59)
  
  timestamps <- as.POSIXct(paste(date, sprintf("%02d:%02d:00", floor(hours), floor(minutes))))
  timestamps <- sort(timestamps)
  
  # Generate transaction sizes (log-normal distribution)
  transaction_sizes <- rlnorm(n_transactions, 
                             meanlog = log(base_volume / n_transactions), 
                             sdlog = 0.5)
  
  # Adjust to match daily total approximately
  transaction_sizes <- transaction_sizes * base_volume / sum(transaction_sizes)
  
  data.frame(
    timestamp = timestamps,
    date = date,
    institution = institution,
    modality = modality,
    value = round(transaction_sizes, 0),
    transaction_id = paste0(institution, "_", modality, "_", date, "_", 1:n_transactions)
  )
}

#' Generate correlated multi-asset data
#'
#' @param n_periods Number of time periods
#' @param assets Vector of asset names
#' @param correlation_matrix Correlation matrix between assets
#' @param base_values Named vector of base values
#' @param volatilities Named vector of volatilities
#' @return Data frame with correlated asset data
#' @export
generate_correlated_data <- function(n_periods = 100,
                                   assets = c("USD_EUR", "USD_GBP", "USD_JPY"),
                                   correlation_matrix = NULL,
                                   base_values = c(USD_EUR = 1.1, USD_GBP = 1.3, USD_JPY = 110),
                                   volatilities = c(USD_EUR = 0.1, USD_GBP = 0.12, USD_JPY = 0.08)) {
  
  n_assets <- length(assets)
  
  # Default correlation matrix if not provided
  if (is.null(correlation_matrix)) {
    correlation_matrix <- matrix(0.3, n_assets, n_assets)
    diag(correlation_matrix) <- 1
    rownames(correlation_matrix) <- assets
    colnames(correlation_matrix) <- assets
  }
  
  # Generate correlated random numbers
  L <- chol(correlation_matrix)
  random_matrix <- matrix(rnorm(n_periods * n_assets), n_periods, n_assets)
  correlated_random <- random_matrix %*% L
  
  # Generate price paths
  result <- data.frame(period = 1:n_periods)
  
  for (i in 1:n_assets) {
    asset <- assets[i]
    base_val <- base_values[asset]
    vol <- volatilities[asset]
    
    # Generate returns
    returns <- vol * correlated_random[, i]
    
    # Generate price path
    prices <- base_val * cumprod(1 + returns)
    
    result[[asset]] <- prices
  }
  
  return(result)
}

#' Generate regime-switching data
#'
#' @param n_periods Number of periods
#' @param regimes List of regime parameters
#' @param transition_probs Transition probability matrix
#' @return Data frame with regime-switching data
#' @export
generate_regime_switching_data <- function(n_periods = 100,
                                         regimes = list(
                                           bull = list(mean = 0.02, vol = 0.1),
                                           bear = list(mean = -0.01, vol = 0.2),
                                           normal = list(mean = 0.005, vol = 0.15)
                                         ),
                                         transition_probs = matrix(c(0.9, 0.05, 0.05,
                                                                   0.1, 0.8, 0.1,
                                                                   0.05, 0.05, 0.9), 
                                                                 nrow = 3, byrow = TRUE)) {
  
  n_regimes <- length(regimes)
  regime_names <- names(regimes)
  
  # Initialize
  current_regime <- 1  # Start in first regime
  regime_sequence <- numeric(n_periods)
  returns <- numeric(n_periods)
  
  for (i in 1:n_periods) {
    regime_sequence[i] <- current_regime
    
    # Generate return based on current regime
    regime_params <- regimes[[current_regime]]
    returns[i] <- rnorm(1, regime_params$mean, regime_params$vol)
    
    # Transition to next regime
    if (i < n_periods) {
      current_regime <- sample(1:n_regimes, 1, prob = transition_probs[current_regime, ])
    }
  }
  
  # Generate price path
  prices <- 100 * cumprod(1 + returns)
  
  data.frame(
    period = 1:n_periods,
    regime = regime_names[regime_sequence],
    return = returns,
    price = prices
  )
}

#' Add realistic market microstructure noise
#'
#' @param clean_data Clean price data
#' @param noise_level Noise level (0-1)
#' @return Data with added microstructure noise
#' @export
add_microstructure_noise <- function(clean_data, noise_level = 0.01) {
  
  result <- clean_data
  
  # Add bid-ask spread simulation
  spread_pct <- noise_level * 0.5
  result$bid <- result$value * (1 - spread_pct/2)
  result$ask <- result$value * (1 + spread_pct/2)
  
  # Add random noise to observed prices
  noise <- rnorm(nrow(result), 0, result$value * noise_level)
  result$observed_value <- pmax(0, result$value + noise)
  
  return(result)
}
