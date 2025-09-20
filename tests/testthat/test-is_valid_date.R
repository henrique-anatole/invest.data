test_that("is_valid_date correctly identifies valid and invalid dates", {
  # Single valid and invalid dates
  expect_equal(is_valid_date("2023-10-15"), TRUE)
  expect_equal(is_valid_date("2023-02-30"), FALSE)
  expect_equal(is_valid_date("15-10-2023"), FALSE)
  
  # Vector input
  dates <- c("2023-01-01", "2023-02-30", "2023-12-31", "2023-13-01")
  expect_equal(is_valid_date(dates), c(TRUE, FALSE, TRUE, FALSE))
  
  # NA input
  expect_equal(is_valid_date(NA), FALSE)
  expect_equal(is_valid_date(c("2023-01-01", NA, "2023-02-30")), c(TRUE, FALSE, FALSE))
  
  # Leading/trailing spaces (optional: fails if not trimmed)
  expect_equal(is_valid_date(c(" 2023-01-01", "2023-01-01 ")), c(FALSE, FALSE))
  
  # Non-character input should throw an error
  expect_error(is_valid_date(20230101), "`date_str` must be a character vector")
})
