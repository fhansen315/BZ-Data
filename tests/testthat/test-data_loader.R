library(testthat)
library(dplyr)

# Source the functions
source("../../R/data_loader.R")

test_that("load_fx_data loads sample data correctly", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true(all(c("date", "institution", "modality", "value") %in% names(df)))
  expect_s3_class(df$date, "Date")
  expect_type(df$value, "double")
})

test_that("load_fx_data handles missing file gracefully", {
  expect_warning(df <- load_fx_data("nonexistent.csv"))
  expect_equal(nrow(df), 0)
})

test_that("get_institutions returns sorted unique institutions", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  institutions <- get_institutions(df)
  
  expect_type(institutions, "character")
  expect_true(length(institutions) > 0)
  expect_equal(institutions, sort(unique(df$institution)))
})

test_that("get_modalities returns sorted unique modalities", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  modalities <- get_modalities(df)
  
  expect_type(modalities, "character")
  expect_true(length(modalities) > 0)
  expect_equal(modalities, sort(unique(df$modality)))
})

test_that("filter_by_institution works correctly", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  institutions <- get_institutions(df)
  
  filtered <- filter_by_institution(df, institutions[1])
  
  expect_true(all(filtered$institution == institutions[1]))
  expect_true(nrow(filtered) > 0)
})

test_that("filter_by_modality works correctly", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  modalities <- get_modalities(df)
  
  filtered <- filter_by_modality(df, modalities[1])
  
  expect_true(all(filtered$modality == modalities[1]))
  expect_true(nrow(filtered) > 0)
})

test_that("filter_by_date_range works correctly", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  
  start_date <- min(df$date)
  end_date <- max(df$date)
  mid_date <- start_date + as.numeric(end_date - start_date) / 2
  
  filtered <- filter_by_date_range(df, start_date, mid_date)
  
  expect_true(all(filtered$date >= start_date))
  expect_true(all(filtered$date <= mid_date))
  expect_true(nrow(filtered) < nrow(df))
})

test_that("validate_data catches missing columns", {
  df <- tibble::tibble(date = as.Date("2024-01-01"), value = 100)
  
  expect_error(validate_data(df), "Missing required columns")
})

test_that("validate_data passes with correct structure", {
  df <- load_fx_data("data/raw/sample_fx_data.csv")
  
  expect_true(validate_data(df))
})

