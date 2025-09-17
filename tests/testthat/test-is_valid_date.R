test_that("Test if a Date is in the right format ", {
  expect_equal(is_valid_date("2023-10-15"), TRUE)  # TRUE
  expect_equal(is_valid_date("2023-02-30"), FALSE)  # FALSE (invalid date)
  expect_equal(is_valid_date("15-10-2023"), FALSE)  # FALSE (wrong format)
})

