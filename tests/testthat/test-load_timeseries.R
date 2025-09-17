# Unit tests for load_stock_timeseries function
test_that("Load the timeseries from yahoo finance (daily) or tiingo (intraday)", {
  expect_equal(dim(load_stock_timeseries(symbol = "AAPL", interval="1d", start_date="2021-10-01", end_date="2022-02-01")$data), c(84, 8))
  expect_error(load_stock_timeseries(
    symbol = "AAPL",
    interval = "1h",
    start_date = "2022-10-01",
    end_date   = "2021-10-10"
  ), "start_date must be earlier than end_date")
  expect_error(load_stock_timeseries(
    symbol = "AAPL",
    interval = "2d",
    start_date = "2021-10-01",
    end_date   = "2021-10-10"
  ), regexp = "Please, select an interval between")
  expect_error(load_stock_timeseries(
    symbol = "AAPL",
    interval = "1h",
    start_date = "2021-13-01",
    end_date   = "2021-10-10"
  ), "start_date is not a valid date. Please use YYYY-MM-DD format.")

})

test_that("AAPL hourly returns correct shape", {
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
