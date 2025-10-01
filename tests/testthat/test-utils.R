library(testthat)

# Source the functions
source("../../R/utils.R")

test_that("format_number formats correctly", {
  expect_equal(format_number(1000), "1,000")
  expect_equal(format_number(1000000), "1,000,000")
})

test_that("format_currency formats correctly", {
  result <- format_currency(1000)
  expect_true(grepl("\\$", result))
  expect_true(grepl("1,000", result))
})

test_that("pct_change calculates correctly", {
  x <- c(100, 110, 121)
  result <- pct_change(x)
  
  expect_equal(result[2], 10, tolerance = 0.01)
  expect_equal(result[3], 10, tolerance = 0.01)
  expect_true(is.na(result[1]))
})

test_that("moving_average calculates correctly", {
  x <- c(1, 2, 3, 4, 5)
  result <- moving_average(x, n = 3)
  
  expect_equal(length(result), length(x))
  expect_equal(result[3], 2, tolerance = 0.01)
})

