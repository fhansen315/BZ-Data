#' Format large numbers with commas
#'
#' @param x Numeric vector
#' @return Character vector with formatted numbers
#' @export
format_number <- function(x) {
  scales::comma(x)
}

#' Format currency values
#'
#' @param x Numeric vector
#' @param currency Currency symbol (default: "$")
#' @return Character vector with formatted currency
#' @export
format_currency <- function(x, currency = "$") {
  paste0(currency, scales::comma(x, accuracy = 0.01))
}

#' Calculate percentage change
#'
#' @param x Numeric vector
#' @return Numeric vector of percentage changes
#' @export
pct_change <- function(x) {
  (x / dplyr::lag(x) - 1) * 100
}

#' Calculate moving average
#'
#' @param x Numeric vector
#' @param n Window size
#' @return Numeric vector with moving average
#' @export
moving_average <- function(x, n = 3) {
  stats::filter(x, rep(1/n, n), sides = 2)
}

#' Source all R files in R/ directory
#'
#' @param path Path to R directory (default: "R")
#' @export
source_r_files <- function(path = "R") {
  if (!dir.exists(path)) {
    warning("R directory not found at: ", path)
    return(invisible(NULL))
  }
  
  r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) {
    source(f)
  }
  invisible(NULL)
}

#' Validate data frame structure
#'
#' @param df Data frame to validate
#' @param required_cols Required column names
#' @return TRUE if valid, throws error otherwise
#' @export
validate_data <- function(df, required_cols = c("date", "institution", "modality", "value")) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(df) == 0) {
    warning("Data frame is empty")
  }
  
  return(TRUE)
}

