#' Load FX data from various sources
#'
#' @param path Path to data file (CSV, Excel, etc.)
#' @param source Type of source: "csv", "excel", "auto" (default)
#' @return Tibble with columns: date, institution, modality, value
#' @export
load_fx_data <- function(path = NULL, source = "auto") {
  if (is.null(path)) {
    path <- Sys.getenv("BZ_DATA_PATH", unset = "data/raw/sample_fx_data.csv")
  }
  
  if (!file.exists(path)) {
    warning("Data file not found at: ", path, ". Returning empty tibble.")
    return(tibble::tibble(
      date = as.Date(character()),
      institution = character(),
      modality = character(),
      value = numeric()
    ))
  }
  
  # Auto-detect source
  if (source == "auto") {
    ext <- tolower(tools::file_ext(path))
    source <- switch(ext,
      "csv" = "csv",
      "xlsx" = "excel",
      "xls" = "excel",
      "csv"  # default
    )
  }
  
  # Load data
  df <- switch(source,
    "csv" = readr::read_csv(path, show_col_types = FALSE),
    "excel" = readxl::read_excel(path),
    stop("Unsupported source type: ", source)
  )
  
  # Validate and standardize
  required_cols <- c("date", "institution", "modality", "value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  df <- df %>%
    dplyr::mutate(
      date = as.Date(date),
      institution = as.character(institution),
      modality = as.character(modality),
      value = as.numeric(value)
    ) %>%
    dplyr::select(date, institution, modality, value)
  
  return(df)
}

#' Get unique institutions from data
#' @param df Data frame with institution column
#' @return Sorted character vector of institutions
#' @export
get_institutions <- function(df) {
  sort(unique(df$institution))
}

#' Get unique modalities from data
#' @param df Data frame with modality column
#' @return Sorted character vector of modalities
#' @export
get_modalities <- function(df) {
  sort(unique(df$modality))
}

#' Filter data by institution
#' @param df Data frame
#' @param institution Institution name(s)
#' @return Filtered data frame
#' @export
filter_by_institution <- function(df, institution) {
  df %>% dplyr::filter(institution %in% !!institution)
}

#' Filter data by modality
#' @param df Data frame
#' @param modality Modality name(s)
#' @return Filtered data frame
#' @export
filter_by_modality <- function(df, modality) {
  df %>% dplyr::filter(modality %in% !!modality)
}

#' Filter data by date range
#' @param df Data frame
#' @param start_date Start date
#' @param end_date End date
#' @return Filtered data frame
#' @export
filter_by_date_range <- function(df, start_date = NULL, end_date = NULL) {
  if (!is.null(start_date)) {
    df <- df %>% dplyr::filter(date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    df <- df %>% dplyr::filter(date <= as.Date(end_date))
  }
  return(df)
}

