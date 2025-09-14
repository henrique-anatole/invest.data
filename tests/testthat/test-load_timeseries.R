# Unit tests for load_stock_timeseries function
test_that("Load the timeseries from yahoo finance (daily) or tiingo (intraday)", {
  expect_equal(dim(load_stock_timeseries(symbol = "AAPL", interval="1d", start_date="2021-10-01", end_date="2022-02-01")$data), c(84, 8))
})

