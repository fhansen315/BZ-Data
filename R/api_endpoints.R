#' API Endpoints for BZ-Data
#'
#' This module provides REST API endpoints for accessing FX data
#' and analytics programmatically using plumber.

#' Create plumber API for BZ-Data
#'
#' @return Plumber API object
#' @export
create_bz_api <- function() {
  
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("plumber package is required for API functionality")
  }
  
  # Source required functions
  source("R/data_loader.R")
  source("R/financial_analytics.R")
  source("R/forecasting.R")
  source("R/realtime_features.R")
  
  pr <- plumber::plumber$new()
  
  # Enable CORS
  pr$filter("cors", function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  })
  
  # Health check endpoint
  pr$handle("GET", "/health", function() {
    list(
      status = "healthy",
      timestamp = Sys.time(),
      version = "1.0.0"
    )
  })
  
  # Get all data
  pr$handle("GET", "/api/data", function(institution = NULL, modality = NULL, 
                                        start_date = NULL, end_date = NULL) {
    tryCatch({
      df <- load_fx_data()
      
      # Apply filters
      if (!is.null(institution)) {
        df <- filter_by_institution(df, institution)
      }
      if (!is.null(modality)) {
        df <- filter_by_modality(df, modality)
      }
      if (!is.null(start_date) || !is.null(end_date)) {
        df <- filter_by_date_range(df, start_date, end_date)
      }
      
      list(
        success = TRUE,
        data = df,
        count = nrow(df),
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get summary statistics
  pr$handle("GET", "/api/summary", function(group_by = "institution") {
    tryCatch({
      df <- load_fx_data()
      
      valid_groups <- c("institution", "modality", "date")
      if (!group_by %in% valid_groups) {
        group_by <- "institution"
      }
      
      summary_data <- summarize_data(df, group_by = group_by)
      
      list(
        success = TRUE,
        data = summary_data,
        group_by = group_by,
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get risk metrics
  pr$handle("GET", "/api/risk", function(confidence = 0.95, method = "historical") {
    tryCatch({
      df <- load_fx_data()
      
      # Calculate returns
      risk_data <- df %>%
        arrange(institution, modality, date) %>%
        group_by(institution, modality) %>%
        mutate(returns = (value - lag(value)) / lag(value)) %>%
        filter(!is.na(returns)) %>%
        summarize(
          var = calculate_var(returns, as.numeric(confidence), method),
          expected_shortfall = calculate_expected_shortfall(returns, as.numeric(confidence)),
          volatility = sd(returns, na.rm = TRUE),
          sharpe_ratio = calculate_sharpe_ratio(returns),
          .groups = "drop"
        )
      
      list(
        success = TRUE,
        data = risk_data,
        parameters = list(confidence = confidence, method = method),
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get forecast
  pr$handle("GET", "/api/forecast", function(periods = 6, method = "ensemble") {
    tryCatch({
      df <- load_fx_data()
      
      # Prepare data for forecasting
      forecast_data <- df %>%
        group_by(date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(date) %>%
        pull(total_value)
      
      # Generate forecast
      forecast_result <- switch(method,
        "ensemble" = forecast_ensemble(forecast_data, as.numeric(periods)),
        "ma" = forecast_moving_average(forecast_data, as.numeric(periods)),
        "es" = forecast_exponential_smoothing(forecast_data, as.numeric(periods)),
        "trend" = forecast_linear_trend(forecast_data, as.numeric(periods)),
        forecast_ensemble(forecast_data, as.numeric(periods))  # default
      )
      
      list(
        success = TRUE,
        forecast = forecast_result$forecast,
        lower_ci = forecast_result$lower_ci,
        upper_ci = forecast_result$upper_ci,
        method = forecast_result$method,
        parameters = list(periods = periods, method = method),
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get anomalies
  pr$handle("GET", "/api/anomalies", function(method = "zscore", threshold = 2.5) {
    tryCatch({
      df <- load_fx_data()
      
      # Calculate anomalies
      anomaly_data <- df %>%
        group_by(date) %>%
        summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(date) %>%
        mutate(
          anomaly = detect_anomalies(total_value, method = method, 
                                   threshold = as.numeric(threshold))
        ) %>%
        filter(anomaly)
      
      list(
        success = TRUE,
        data = anomaly_data,
        count = nrow(anomaly_data),
        parameters = list(method = method, threshold = threshold),
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get real-time dashboard data
  pr$handle("GET", "/api/dashboard", function() {
    tryCatch({
      df <- load_fx_data()
      
      # Get latest data point
      latest_date <- max(df$date)
      current_data <- df %>% filter(date == latest_date)
      
      # Create dashboard data
      dashboard_data <- create_live_dashboard_data(df)
      market_status <- generate_market_status(current_data, df)
      
      list(
        success = TRUE,
        dashboard = dashboard_data,
        market_status = market_status,
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Post new data (for real-time updates)
  pr$handle("POST", "/api/data", function(req) {
    tryCatch({
      # Parse JSON body
      new_data <- jsonlite::fromJSON(req$postBody)
      
      # Validate required fields
      required_fields <- c("date", "institution", "modality", "value")
      if (!all(required_fields %in% names(new_data))) {
        return(list(
          success = FALSE,
          error = paste("Missing required fields:", 
                       paste(setdiff(required_fields, names(new_data)), collapse = ", ")),
          timestamp = Sys.time()
        ))
      }
      
      # Here you would typically save to database
      # For now, just validate and return success
      
      list(
        success = TRUE,
        message = "Data received successfully",
        records = nrow(new_data),
        timestamp = Sys.time()
      )
    }, error = function(e) {
      list(
        success = FALSE,
        error = as.character(e),
        timestamp = Sys.time()
      )
    })
  })
  
  # Get API documentation
  pr$handle("GET", "/api/docs", function() {
    list(
      title = "BZ-Data API",
      version = "1.0.0",
      description = "REST API for FX modality analytics",
      endpoints = list(
        "GET /health" = "Health check",
        "GET /api/data" = "Get FX data with optional filters",
        "GET /api/summary" = "Get summary statistics",
        "GET /api/risk" = "Get risk metrics (VaR, volatility, etc.)",
        "GET /api/forecast" = "Get forecasts",
        "GET /api/anomalies" = "Get anomaly detection results",
        "GET /api/dashboard" = "Get real-time dashboard data",
        "POST /api/data" = "Submit new data",
        "GET /api/docs" = "This documentation"
      ),
      timestamp = Sys.time()
    )
  })
  
  return(pr)
}

#' Start the BZ-Data API server
#'
#' @param port Port number (default: 8000)
#' @param host Host address (default: "127.0.0.1")
#' @export
start_api_server <- function(port = 8000, host = "127.0.0.1") {
  
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("plumber package is required. Install with: install.packages('plumber')")
  }
  
  api <- create_bz_api()
  
  cat("Starting BZ-Data API server...\n")
  cat("API Documentation: http://", host, ":", port, "/api/docs\n", sep = "")
  cat("Health Check: http://", host, ":", port, "/health\n", sep = "")
  cat("Press Ctrl+C to stop the server\n\n")
  
  api$run(host = host, port = port)
}

#' Create API client functions
#'
#' @param base_url Base URL of the API
#' @return List of client functions
#' @export
create_api_client <- function(base_url = "http://127.0.0.1:8000") {
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package is required for API client")
  }
  
  list(
    get_data = function(institution = NULL, modality = NULL, start_date = NULL, end_date = NULL) {
      params <- list()
      if (!is.null(institution)) params$institution <- institution
      if (!is.null(modality)) params$modality <- modality
      if (!is.null(start_date)) params$start_date <- start_date
      if (!is.null(end_date)) params$end_date <- end_date
      
      response <- httr::GET(paste0(base_url, "/api/data"), query = params)
      httr::content(response, as = "parsed")
    },
    
    get_summary = function(group_by = "institution") {
      response <- httr::GET(paste0(base_url, "/api/summary"), query = list(group_by = group_by))
      httr::content(response, as = "parsed")
    },
    
    get_risk = function(confidence = 0.95, method = "historical") {
      response <- httr::GET(paste0(base_url, "/api/risk"), 
                           query = list(confidence = confidence, method = method))
      httr::content(response, as = "parsed")
    },
    
    get_forecast = function(periods = 6, method = "ensemble") {
      response <- httr::GET(paste0(base_url, "/api/forecast"), 
                           query = list(periods = periods, method = method))
      httr::content(response, as = "parsed")
    },
    
    get_anomalies = function(method = "zscore", threshold = 2.5) {
      response <- httr::GET(paste0(base_url, "/api/anomalies"), 
                           query = list(method = method, threshold = threshold))
      httr::content(response, as = "parsed")
    },
    
    get_dashboard = function() {
      response <- httr::GET(paste0(base_url, "/api/dashboard"))
      httr::content(response, as = "parsed")
    },
    
    post_data = function(data) {
      response <- httr::POST(paste0(base_url, "/api/data"), 
                            body = jsonlite::toJSON(data), 
                            httr::content_type_json())
      httr::content(response, as = "parsed")
    }
  )
}
