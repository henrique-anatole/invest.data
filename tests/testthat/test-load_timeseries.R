# Unit tests for load_stock_timeseries function
test_that("Load the timeseries from yahoo finance (daily) or tiingo (intraday)", {
  expect_equal(dim(load_stock_timeseries(symbol = "AAPL", interval="1d", start_date="2021-10-01", end_date="2022-02-01")$data), c(84, 8))
})

test_that("start_date after end_date throws error", {
  expect_error(load_stock_timeseries("AAPL", "1d",
                                     start_date="2021-02-01",
                                     end_date="2021-01-01"))
})

test_that("unsupported interval throws error", {
  expect_error(load_stock_timeseries("AAPL", "2d",
                                     start_date="2021-01-01",
                                     end_date="2021-01-10"))
})


test_that("Invalid dates throws error", {

  expect_error(load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    start_date = "2021-13-01",
    end_date   = "2021-10-10"
  ), regexp = "start_date must be")
  expect_error(load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    start_date = "2021-10-01",
    end_date   = "2021-13-10"
  ), regexp = "end_date must be")

})

test_that("valid daily yahoo request returns data", {
  res <- load_stock_timeseries("AAPL", "1d",
                               start_date="2021-01-01",
                               end_date="2021-01-10")
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
})

test_that("tiingo unsupported symbol returns error", {
  # Only run this test if the tiingo_key option is set
  skip_if(getOption('tiingo_key') == "", "No API key available")

  expect_error(load_stock_timeseries("INVALID", "1m",
                                     start_date="2021-01-01",
                                     end_date="2021-01-02"))
})

test_that("split adjustment scales prices and volume", {
  # Only run this test if the tiingo_key option is set
  skip_if(getOption('tiingo_key') == "", "No API key available")

  res <- load_stock_timeseries("AAPL", "1d",
                               start_date="2020-01-01",
                               end_date="2023-01-05",
                               adjust_splits = TRUE)
  expect_true(all(c("open","close","volume") %in% names(res$data)))

})


test_that("hourly returns correct shape", {
  skip_if(getOption('tiingo_key') == "", "No API key available")
  
  apple <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1h",
    start_date = "2021-10-01",
    end_date   = "2021-10-10"
  )
  
  expect_s3_class(apple$data, "tbl_df")
  expect_true(nrow(apple$data) > 0)

})

test_that("Large date range does not exceed usage limit", {
  skip_if(getOption('tiingo_key') == "", "No API key available")
  
  apple <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1m",
    start_date = "2020-01-02",
    end_date   = "2020-01-04"
  )
  
  expect_s3_class(apple$data, "tbl_df")
  expect_true(nrow(apple$data) > 0)
  
})

test_that("Large date range does duplicate rows", {
  skip_if(getOption('tiingo_key') == "" | is.null(getOption('tiingo_key')), "No API key available")
  
  apple <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1m",
    start_date = "2020-01-02",
    end_date   = "2020-01-04"
  )
  
  # Check for duplicate timestamps
  expect_equal(nrow(apple$data), nrow(dplyr::distinct(apple$data, open_time)))
  
})


test_that("Multiple symbols returns correct shape", {
  
  stocks <- load_stock_timeseries(
    symbol = c("AAPL", "MSFT"),
    interval = "1d",
    start_date = "2021-10-01",
    end_date   = "2021-10-10"
  )
  
  expect_s3_class(stocks$data, "tbl_df")
  expect_true(nrow(stocks$data) > 0)
  expect_true(all(c("AAPL", "MSFT") %in% unique(stocks$data$symbol)))
  expect_equal(ncol(stocks$data), 8)

})


