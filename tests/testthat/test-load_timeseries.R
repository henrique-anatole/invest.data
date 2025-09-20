# Unit tests for load_stock_timeseries function
test_that("stock: start_date after end_date throws error", {
  expect_error(load_stock_timeseries("AAPL", "1d",
                                     start_date="2021-02-01",
                                     end_date="2021-01-01")
              , regexp = "start_date must be")
})

test_that("stock: unsupported interval throws error", {
  expect_error(load_stock_timeseries("AAPL", "2d",
                                     start_date="2021-01-01",
                                     end_date="2021-01-10"),
             regexp = "Invalid interval")
})

test_that("stock: Invalid dates throws error", {

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

test_that("stock: valid daily yahoo request returns data", {
  res <- load_stock_timeseries("AAPL", "1d",
                               start_date="2021-01-01",
                               end_date="2021-01-10")
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_equal(ncol(res$data), 8)
})

test_that("stock: split adjustment scales prices and volume", {
  # Only run this test if the tiingo_key option is set
  skip_if(getOption('tiingo_key') == "", "No API key available")

  res <- load_stock_timeseries("AAPL", "1d",
                               start_date="2020-01-01",
                               end_date="2023-01-05",
                               adjust_splits = TRUE)
  expect_true(all(c("open","close","volume") %in% names(res$data)))

})

test_that("stock: hourly returns correct shape", {
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

test_that("stock: Large date range does not exceed usage limit", {
  skip_if(getOption('tiingo_key') == "", "No API key available")
  
  apple <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1m",
    start_date = "2020-01-02",
    end_date   = "2020-01-04"
  )
  
  expect_s3_class(apple$data, "tbl_df")
  expect_true(nrow(apple$data) > 0)
  # Check for duplicate timestamps
  expect_equal(nrow(apple$data), nrow(dplyr::distinct(apple$data, open_time)))

})

test_that("stock: Multiple symbols returns correct shape", {
  
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

test_that("stock: Empty result returns tibble with 0 rows", {
  suppressWarnings(
    res <- load_stock_timeseries(symbol = c("AAPL", "TSLA", "HAHAUSDT"), 
                                interval = "1d",
                                start_date = "2022-01-01",
                                end_date   = "2022-01-02")
  )
  expect_true(nrow(res$errors) > 0)
  expect_equal(res$data, NULL)

  suppressWarnings(
    res <- load_stock_timeseries(symbol = c("AAPL", "TSLA", "HAHAUSDT"), 
                                interval = "1h",
                                start_date = "2022-01-03",
                                end_date   = "2022-01-04")
  )
  expect_true(nrow(res$errors) > 0)
  expect_true(nrow(res$data) > 0)

})

test_that("stock: Data has expected column names", {
  suppressWarnings(
    res <- load_stock_timeseries(symbol = "AAPL", 
                                interval = "1d",
                                start_date = "2022-01-01",
                                end_date   = "2022-01-04")
  )
  expect_equal(names(res$data),
               c("open_time","open","high","low","close","volume","symbol","adjusted"))
})

#### Cryptocurrency tests ####
# Unit tests for load_crypto_timeseries function

test_that("crypto: Load the timeseries from binance (daily)", {
  res <- load_crypto_timeseries(
    pair = "ETHUSDT",
    interval = "1d",
    start_date = "2021-01-01",
    end_date   = "2021-01-10"
  )
  expect_equal(ncol(res$data), 13)  # OHLCV + open_time + symbol
  expect_true(nrow(res$data) > 0)
})

test_that("crypto: start_date after end_date throws error", {
  expect_error(load_crypto_timeseries("ETHUSDT", "1d",
                                      start_date = "2021-02-01",
                                      end_date   = "2021-01-01")
                                    , regexp = "start_date must be")
})

test_that("crypto: Unsupported interval throws error", {
expect_error(load_crypto_timeseries("ETHUSDT", "2d",
                                    start_date="2021-01-01",
                                    end_date="2021-01-10"),
             regexp = "Invalid interval")
})

test_that("crypto: Invalid dates throw error", {
  expect_error(load_crypto_timeseries(
    pair = "ETHUSDT",
    interval = "1d",
    start_date = "2021-13-01",
    end_date   = "2021-01-10"
  ), regexp = "start_date must be")

  expect_error(load_crypto_timeseries(
    pair = "ETHUSDT",
    interval = "1d",
    start_date = "2021-01-01",
    end_date   = "2021-13-10"
  ), regexp = "end_date must be")

    expect_error(load_crypto_timeseries(
    pair = "ETHUSDT",
    interval = "1d",
    start_date = "2099-01-01",
    end_date   = "2099-10-10"
  ), regexp = "start_date cannot be in")

})

test_that("crypto: valid binance request returns data", {
  res <- load_crypto_timeseries("BTCUSDT", "1h",
                                start_date = "2021-01-01",
                                end_date   = "2021-01-02")
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_equal(ncol(res$data), 13)
})

test_that("crypto: Large date range chunks correctly", {
  res <- load_crypto_timeseries("ETHUSDT", "1m",
                                start_date = "2021-01-01",
                                end_date   = "2021-01-03")
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_equal(nrow(res$data), nrow(dplyr::distinct(res$data, open_time)))
})

test_that("crypto: Multiple pairs returns correct shape", {
  pair <- c("ETHUSDT", "BTCUSDT")
  res <- load_crypto_timeseries(
      pair = pair,
      interval = "1d",
      start_date = "2021-01-01",
      end_date   = "2021-01-05"
    )

  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_true(all(c("ETHUSDT", "BTCUSDT") %in% unique(res$data$symbol)))
  expect_equal(ncol(res$data), 13)
})

test_that("crypto: Empty result, including Invalid pairs, returns null data", {
  res <- load_crypto_timeseries("HAHAUSDT", "1d",
                                start_date = "2022-01-01",
                                end_date   = "2022-01-10")
  expect_true(nrow(res$errors) > 0)
  expect_equal(res$data, NULL)
})

test_that("crypto: Data has expected column names", {
  res <- load_crypto_timeseries("ETHUSDT", "1d",
                                start_date="2021-01-01",
                                end_date="2021-01-02")
  expect_equal(names(res$data),
               c("open_time","open","high","low","close","volume","symbol","adjusted",
                 "close_time","quote_asset_volume","trades","taker_buy_base_asset_volume","taker_buy_quote_asset_volume"))
})

test_that("crypto: Timeseries is ordered by open_time", {
  res <- load_crypto_timeseries("ETHUSDT", "1h",
                                start_date="2021-01-01",
                                end_date="2021-01-03")
  expect_true(all(diff(res$data$open_time) >= 0))
})

## Dividends tests
# Unit tests for load_yahoo_dividends function

test_that("dividends: start_date after end_date throws error", {
  expect_error(
    load_yahoo_dividends(symbols = "AAPL",
                         start_date = "2021-02-01",
                         end_date   = "2021-01-01"),
    regexp = "start_date must be"
  )
})

test_that("dividends: invalid dates throws error", {
  expect_error(
    load_yahoo_dividends(symbols = "AAPL",
                         start_date = "2021-13-01",
                         end_date   = "2021-01-10"),
    regexp = "start_date must be"
  )
  expect_error(
    load_yahoo_dividends(symbols = "AAPL",
                         start_date = "2021-01-01",
                         end_date   = "2021-13-10"),
    regexp = "end_date must be"
  )
})

test_that("dividends: single symbol returns data", {
  res <- load_yahoo_dividends(symbols = "AAPL",
                              start_date = "2020-01-01",
                              end_date   = "2022-01-01")
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_true("symbol" %in% names(res$data))
})

test_that("dividends: multiple symbols returns combined data", {
  res <- load_yahoo_dividends(symbols = c("AAPL", "MSFT"),
                              start_date = "2020-01-01",
                              end_date   = "2022-01-01")
  expect_s3_class(res$data, "tbl_df")
  expect_true(all(c("AAPL", "MSFT") %in% unique(res$data$symbol)))
})

test_that("dividends: non-existent symbol captured in errors", {
  res <- suppressWarnings(
    load_yahoo_dividends(symbols = c("AAPL", "NONEXISTENT"),
                         start_date = "2020-01-01",
                         end_date   = "2022-01-01")
  )
  expect_true(!is.null(res$errors))
  expect_true("NONEXISTENT" %in% res$errors$symbol)
})

test_that("dividends: data preserves input symbol order", {
  symbols <- c("MSFT", "AAPL")
  res <- load_yahoo_dividends(symbols = symbols,
                              start_date = "2020-01-01",
                              end_date   = "2022-01-01")
  expect_equal(levels(res$data$symbol), symbols)
})

test_that("dividends: empty result returns NULL data and non-empty errors", {
  res <- suppressWarnings(
    load_yahoo_dividends(symbols = "NONEXISTENT",
                         start_date = "2020-01-01",
                         end_date   = "2022-01-01")
  )
  expect_null(res$data)
  expect_true(nrow(res$errors) > 0)
})
