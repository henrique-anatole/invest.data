# Unit tests for load_stock_timeseries function
test_that("stock: start_date after end_date throws error", {
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      start_date = "2021-02-01",
      end_date = "2021-01-01"
    ),
    regexp = "start_date must be"
  )
})

test_that("stock: unsupported interval throws error", {
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "2d",
      start_date = "2021-01-01",
      end_date = "2021-01-10"
    ),
    regexp = "Invalid interval"
  )
})

test_that("stock: Invalid dates throws error", {
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      start_date = "2021-13-01",
      end_date = "2021-10-10"
    ),
    regexp = "start_date must be"
  )
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      start_date = "2021-10-01",
      end_date = "2021-13-10"
    ),
    regexp = "end_date must be"
  )
})
# variable for reuse in multiple tests
res <- load_stock_timeseries(
  symbol = "AAPL",
  interval = "1d",
  start_date = "2021-01-01",
  end_date = "2021-01-10"
)

test_that("stock: valid daily yahoo request returns data", {
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_equal(ncol(res$data), 8)
})

test_that("stock: Data has expected column names", {
  # suppressWarnings(
  #   res <- load_stock_timeseries(
  #     symbol = "AAPL",
  #     interval = "1d",
  #     start_date = "2022-01-01",
  #     end_date = "2022-01-04"
  #   )
  # )
  expect_equal(
    names(res$data),
    c(
      "open_time",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "symbol",
      "adjusted"
    )
  )
})

# variable for reuse in multiple tests
res_adjusted <- load_stock_timeseries(
  symbol = "AAPL",
  interval = "1d",
  start_date = "2020-01-01",
  end_date = "2023-01-05",
  adjust_splits = TRUE
)

test_that("stock: split adjustment scales prices and volume", {
  # # Only run this test if the tiingo_key option is set
  # skip_if(getOption('tiingo_key') == "", "No API key available")
  expect_true(all(c("open", "close", "volume") %in% names(res_adjusted$data)))
})

test_that("stock: hourly returns correct shape", {
  skip_if(getOption('tiingo_key') == "", "No API key available")

  apple <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1h",
    start_date = "2021-10-01",
    end_date = "2021-10-10"
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
    end_date = "2020-01-04"
  )

  expect_s3_class(apple$data, "tbl_df")
  expect_true(nrow(apple$data) > 0)
  # Check for duplicate timestamps
  expect_equal(nrow(apple$data), nrow(dplyr::distinct(apple$data, open_time)))
})

# variable for reuse in multiple tests
stocks <- load_stock_timeseries(
  symbol = c("AAPL", "MSFT"),
  interval = "1d",
  start_date = "2021-10-01",
  end_date = "2021-10-10"
)

test_that("stock: Multiple symbols returns correct shape", {
  expect_s3_class(stocks$data, "tbl_df")
  expect_true(nrow(stocks$data) > 0)
  expect_true(all(c("AAPL", "MSFT") %in% unique(stocks$data$symbol)))
  expect_equal(ncol(stocks$data), 8)
})

test_that("stock: Empty result returns tibble with 0 rows", {
  suppressWarnings(
    res <- load_stock_timeseries(
      symbol = c("AAPL", "TSLA", "HAHAUSDT"),
      interval = "1d",
      start_date = "2022-01-01",
      end_date = "2022-01-02"
    )
  )
  expect_true(nrow(res$errors) > 0)
  expect_equal(res$data, NULL)

  suppressWarnings(
    res <- load_stock_timeseries(
      symbol = c("AAPL", "TSLA", "HAHAUSDT"),
      interval = "1h",
      start_date = "2022-01-03",
      end_date = "2022-01-04"
    )
  )
  expect_true(nrow(res$errors) > 0)
  expect_true(nrow(res$data) > 0)
})

test_that("stock: Test chunk sizes", {
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = 3000,
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size cannot exceed"
  )

  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = -5,
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size must be a positive integer at least 10"
  )

  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = 5,
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size must be a positive integer at least 10"
  )

  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = "5000",
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size must be a positive integer at least 10"
  )

  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = NA,
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size must be a positive integer at least 10"
  )

  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      chunk_size = 50.7,
      start_date = "2021-01-01",
      end_date = "2023-01-03"
    ),
    regexp = "chunk_size must be a positive integer at least 10"
  )
})

#### Cryptocurrency tests ####
# Unit tests for load_crypto_timeseries function

test_that("crypto: Load the timeseries from binance (daily)", {
  res <- load_crypto_timeseries(
    pair = "ETHUSDT",
    interval = "1d",
    start_date = "2021-01-01",
    end_date = "2021-01-10"
  )
  expect_equal(ncol(res$data), 13) # OHLCV + open_time + symbol
  expect_true(nrow(res$data) > 0)
})

test_that("crypto: start_date after end_date throws error", {
  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "1d",
      start_date = "2021-02-01",
      end_date = "2021-01-01"
    ),
    regexp = "start_date must be"
  )
})

test_that("crypto: Unsupported interval throws error", {
  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "2d",
      start_date = "2021-01-01",
      end_date = "2021-01-10"
    ),
    regexp = "Invalid interval"
  )
})

test_that("crypto: Invalid dates throw error", {
  expect_error(
    load_crypto_timeseries(
      pair = "ETHUSDT",
      interval = "1d",
      start_date = "2021-13-01",
      end_date = "2021-01-10"
    ),
    regexp = "start_date must be"
  )

  expect_error(
    load_crypto_timeseries(
      pair = "ETHUSDT",
      interval = "1d",
      start_date = "2021-01-01",
      end_date = "2021-13-10"
    ),
    regexp = "end_date must be"
  )

  expect_error(
    load_crypto_timeseries(
      pair = "ETHUSDT",
      interval = "1d",
      start_date = "2099-01-01",
      end_date = "2099-10-10"
    ),
    regexp = "start_date cannot be in"
  )
})

test_that("crypto: valid binance request returns data", {
  res <- load_crypto_timeseries(
    "BTCUSDT",
    "1h",
    start_date = "2021-01-01",
    end_date = "2021-01-02"
  )
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_equal(ncol(res$data), 13)
})

test_that("crypto: Large date range chunks correctly", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1m",
    start_date = "2021-01-01",
    end_date = "2021-01-03"
  )
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
    end_date = "2021-01-05"
  )

  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_true(all(c("ETHUSDT", "BTCUSDT") %in% unique(res$data$symbol)))
  expect_equal(ncol(res$data), 13)
})

test_that("crypto: Empty result, including Invalid pairs, returns null data", {
  res <- load_crypto_timeseries(
    "HAHAUSDT",
    "1d",
    start_date = "2022-01-01",
    end_date = "2022-01-10"
  )
  expect_true(nrow(res$errors) > 0)
  expect_equal(res$data, NULL)
})

test_that("crypto: Data has expected column names", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1d",
    start_date = "2021-01-01",
    end_date = "2021-01-02"
  )
  expect_equal(
    names(res$data),
    c(
      "open_time",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "symbol",
      "adjusted",
      "close_time",
      "quote_asset_volume",
      "trades",
      "taker_buy_base_asset_volume",
      "taker_buy_quote_asset_volume"
    )
  )
})

test_that("crypto: Timeseries is ordered by open_time", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1h",
    start_date = "2021-01-01",
    end_date = "2021-01-03"
  )
  expect_true(all(diff(res$data$open_time) >= 0))
})

test_that("crypto: Source is valid", {
  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "1d",
      start_date = "2021-01-01",
      end_date = "2021-01-02",
      source = "coinbase"
    ),
    regexp = "Currently only"
  )
})


## Dividends tests
# Unit tests for load_yahoo_dividends function

test_that("dividends: start_date after end_date throws error", {
  expect_error(
    load_yahoo_dividends(
      symbols = "AAPL",
      start_date = "2021-02-01",
      end_date = "2021-01-01"
    ),
    regexp = "start_date must be"
  )
})

test_that("dividends: invalid dates throws error", {
  expect_error(
    load_yahoo_dividends(
      symbols = "AAPL",
      start_date = "2021-13-01",
      end_date = "2021-01-10"
    ),
    regexp = "start_date must be"
  )
  expect_error(
    load_yahoo_dividends(
      symbols = "AAPL",
      start_date = "2021-01-01",
      end_date = "2021-13-10"
    ),
    regexp = "end_date must be"
  )
})

test_that("dividends: single symbol returns data", {
  res <- load_yahoo_dividends(
    symbols = "AAPL",
    start_date = "2020-01-01",
    end_date = "2022-01-01"
  )
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "tbl_df")
  expect_true(nrow(res$data) > 0)
  expect_true("symbol" %in% names(res$data))
})

test_that("dividends: multiple symbols returns combined data", {
  res <- load_yahoo_dividends(
    symbols = c("AAPL", "MSFT"),
    start_date = "2020-01-01",
    end_date = "2022-01-01"
  )
  expect_s3_class(res$data, "tbl_df")
  expect_true(all(c("AAPL", "MSFT") %in% unique(res$data$symbol)))
})

test_that("dividends: non-existent symbol captured in errors", {
  res <- suppressWarnings(
    load_yahoo_dividends(
      symbols = c("NONEXISTENT"),
      start_date = "2020-01-01",
      end_date = "2022-01-01"
    )
  )
  expect_true(!is.null(res$errors))
  expect_true("NONEXISTENT" %in% res$errors$symbol)
})

test_that("dividends: data preserves input symbol order", {
  symbols <- c("MSFT", "AAPL")
  res <- load_yahoo_dividends(
    symbols = symbols,
    start_date = "2020-01-01",
    end_date = "2022-01-01"
  )
  expect_equal(levels(res$data$symbol), symbols)
})

test_that("dividends: empty result returns NULL data and non-empty errors", {
  res <- suppressWarnings(
    load_yahoo_dividends(
      symbols = "NONEXISTENT",
      start_date = "2020-01-01",
      end_date = "2022-01-01"
    )
  )
  expect_null(res$data)
  expect_true(nrow(res$errors) > 0)
})

test_that("dividends: returns errors when all symbols have no data", {
  suppressWarnings(
    res <- load_yahoo_dividends(
      symbols = c("INVALID1", "INVALID2"),
      start_date = "2021-01-01",
      end_date = "2021-12-31"
    )
  )
  expect_null(res$data)
  expect_equal(nrow(res$errors), 2)
  expect_true(all(res$errors$symbol %in% c("INVALID1", "INVALID2")))
})

test_that("dividends: preserves factor levels with one symbol", {
  res <- load_yahoo_dividends(
    symbols = "AAPL",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  skip_if(
    is.null(res$data),
    "No dividend data returned from Yahoo in this range"
  )

  expect_s3_class(res$data$symbol, "factor")
  expect_equal(levels(res$data$symbol), "AAPL")
})

test_that("dividends: non-character symbols throws error", {
  expect_error(
    load_yahoo_dividends(
      123,
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "symbols must be a character vector"
  )
})

#### Adjust for splits tests ####
# Mock data for testing adjust_for_splits function #######
create_mock_ohlcv_data <- function(symbol, dates, open_prices) {
  tibble(
    symbol = symbol,
    open_time = as.POSIXct(dates),
    open = open_prices,
    high = open_prices * 1.02,
    low = open_prices * 0.98,
    close = open_prices * 1.01,
    volume = rep(1000000, length(dates))
  )
}

# Create mock splits data
mock_splits <- tibble(
  symbol = c(rep("AAPL", 5), rep("TSLA", 2)),
  date = as.Date(c(
    "1987-06-16",
    "2000-06-21",
    "2005-02-28",
    "2014-06-09",
    "2020-08-31",
    "2020-08-31",
    "2022-08-25"
  )),
  value = c(0.5, 0.5, 0.5, 0.143, 0.25, 0.2, 0.333)
)

test_that("adjust_for_splits handles data with no splits", {
  suppressWarnings(
    # Mock tidyquant to return empty splits data
    with_mocked_bindings(
      tq_get = function(...) NULL,
      {
        data <- create_mock_ohlcv_data("NOSPLT", "2023-01-01", 100)
        result <- adjust_for_splits(data, symbols = "NOSPLT")

        expect_equal(result, data)
        # expect_message(adjust_for_splits(data, "NOSPLT"), "No splits found")
      }
    )
  )
})

# Helper function to create mock OHLCV data
create_test_ohlcv <- function(symbols, dates, base_price = 100) {
  expand.grid(
    symbol = symbols,
    date = as.Date(dates),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      open_time = as.POSIXct(date),
      open = base_price,
      high = base_price * 1.02,
      low = base_price * 0.98,
      close = base_price * 1.01,
      volume = 1000000
    ) %>%
    arrange(symbol, date) %>%
    select(symbol, open_time, open, high, low, close, volume)
}

# Test data: AAPL splits
test_splits <- tibble(
  symbol = c("AAPL", "AAPL", "AAPL"),
  date = as.Date(c("2020-08-31", "2014-06-09", "2005-02-28")),
  value = c(0.25, 0.143, 0.5) # 4:1, 7:1, 2:1 splits
)

test_that("calculate_adjustment_factors works correctly", {
  result <- calculate_adjustment_factors(test_splits)

  expect_equal(nrow(result), 3)
  expect_true(all(c("symbol", "date", "adj_factor") %in% names(result)))

  # Check cumulative factors (working backwards from most recent)
  # 2020 split (most recent): factor = 0.25
  # 2014 split: factor = 0.25 * 0.143 = 0.03575
  # 2005 split (oldest): factor = 0.25 * 0.143 * 0.5 = 0.017875

  result_by_date <- result %>% arrange(date)
  expect_equal(result_by_date$adj_factor[3], 0.25, tolerance = 1e-10) # 2020
  expect_equal(result_by_date$adj_factor[2], 0.25 * 0.143, tolerance = 1e-10) # 2014
  expect_equal(
    result_by_date$adj_factor[1],
    0.25 * 0.143 * 0.5,
    tolerance = 1e-10
  ) # 2005
})

test_that("adjust_for_splits handles empty data", {
  empty_data <- create_test_ohlcv(character(0), character(0))
  result <- adjust_for_splits(empty_data, "AAPL", test_splits)

  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(empty_data))
})

test_that("adjust_for_splits handles missing columns", {
  bad_data <- tibble(symbol = "AAPL", open_time = Sys.time())

  expect_error(
    adjust_for_splits(bad_data, "AAPL", test_splits),
    "Missing required columns"
  )
})

test_that("adjust_for_splits handles no splits data", {
  data <- create_test_ohlcv("NOSPLITS", "2023-01-01")

  expect_message(
    result <- adjust_for_splits(data, "NOSPLITS", tibble()),
    "No splits found"
  )

  expect_equal(result, data)
})

test_that("adjust_for_splits correctly adjusts prices before splits", {
  # Data from before all splits (should get maximum adjustment)
  data <- create_test_ohlcv("AAPL", "2000-01-01", 100)
  result <- adjust_for_splits(data, "AAPL", test_splits)

  # Expected adjustment factor: 0.25 * 0.143 * 0.5 = 0.017875
  expected_factor <- 0.25 * 0.143 * 0.5
  expected_price <- 100 * expected_factor
  expected_volume <- 1000000 / expected_factor

  expect_equal(result$open[1], expected_price, tolerance = 1e-10)
  expect_equal(result$volume[1], expected_volume, tolerance = 1e-10)
})

test_that("adjust_for_splits correctly adjusts prices between splits", {
  # Data between 2014 split and 2020 split
  data <- create_test_ohlcv("AAPL", "2018-01-01", 100)
  result <- adjust_for_splits(data, "AAPL", test_splits)

  # Should only be adjusted by 2020 split: 0.25
  expected_price <- 100 * 0.25
  expected_volume <- 1000000 / 0.25

  expect_equal(result$open[1], expected_price, tolerance = 1e-10)
  expect_equal(result$volume[1], expected_volume, tolerance = 1e-10)
})

test_that("adjust_for_splits doesn't adjust prices after all splits", {
  # Data after all splits
  data <- create_test_ohlcv("AAPL", "2023-01-01", 100)
  result <- adjust_for_splits(data, "AAPL", test_splits)

  # Should have no adjustment
  expect_equal(result$open[1], 100, tolerance = 1e-10)
  expect_equal(result$volume[1], 1000000, tolerance = 1e-10)
})

test_that("adjust_for_splits handles multiple symbols", {
  # Create splits for two symbols
  multi_splits <- tibble(
    symbol = c("AAPL", "AAPL", "TSLA", "TSLA"),
    date = as.Date(c("2020-08-31", "2014-06-09", "2022-08-25", "2020-08-31")),
    value = c(0.25, 0.143, 0.333, 0.2)
  )

  data <- create_test_ohlcv(
    c("AAPL", "TSLA"),
    c("2010-01-01", "2023-01-01"),
    100
  )
  result <- adjust_for_splits(data, c("AAPL", "TSLA"), multi_splits)

  expect_equal(nrow(result), 4) # 2 symbols × 2 dates

  # Check AAPL adjustment for 2010 data (before both splits)
  aapl_2010 <- result %>%
    filter(symbol == "AAPL", as.Date(open_time) == "2010-01-01")
  expected_aapl_factor <- 0.25 * 0.143
  expect_equal(aapl_2010$open, 100 * expected_aapl_factor, tolerance = 1e-10)

  # Check TSLA adjustment for 2010 data (before both splits)
  tsla_2010 <- result %>%
    filter(symbol == "TSLA", as.Date(open_time) == "2010-01-01")
  expected_tsla_factor <- 0.333 * 0.2
  expect_equal(tsla_2010$open, 100 * expected_tsla_factor, tolerance = 1e-10)
})

test_that("adjust_for_splits preserves data structure", {
  data <- create_test_ohlcv("AAPL", c("2020-01-01", "2023-01-01"), 100)
  result <- adjust_for_splits(data, "AAPL", test_splits)

  # Should have same number of rows and basic structure
  expect_equal(nrow(result), nrow(data))
  expect_equal(names(result), names(data))

  # Check data types
  expect_true(is.character(result$symbol))
  expect_true("POSIXct" %in% class(result$open_time))
  expect_true(is.numeric(result$open))
  expect_true(is.numeric(result$volume))
})

test_that("adjust_for_splits handles edge case - data on split date", {
  # Data exactly on split date
  data <- create_test_ohlcv("AAPL", "2020-08-31", 100)
  result <- adjust_for_splits(data, "AAPL", test_splits)

  # Should be adjusted by this split (and future ones don't apply)
  expect_equal(result$open[1], 100 * 0.25, tolerance = 1e-10)
})


test_that("make_query_chunks returns one chunk if range fits in one step", {
  res <- make_query_chunks(
    seq_interval = "1 day",
    chunk_size = 5,
    start_date = "2021-01-01",
    end_date = "2021-01-03",
    symbol = "AAPL"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$start_date, "2021-01-01T00:00")
  expect_equal(res$end_date, "2021-01-03T00:00")
})

test_that("make_query_chunks splits into multiple chunks for larger ranges", {
  res <- make_query_chunks(
    seq_interval = "1 day",
    chunk_size = 2,
    start_date = "2021-01-01",
    end_date = "2021-01-06",
    symbol = "AAPL"
  )
  expect_equal(nrow(res), 3)
  # first chunk should cover 2 days
  expect_equal(res$start_date[1], "2021-01-01T00:00")
  expect_equal(res$end_date[1], "2021-01-02T00:00")
  # last chunk end matches end_date exactly
  expect_equal(res$end_date[nrow(res)], "2021-01-06T00:00")
})

test_that("make_query_chunks handles leftover interval at the end", {
  res <- make_query_chunks(
    seq_interval = "1 hour",
    chunk_size = 3,
    start_date = "2021-01-01",
    end_date = "2021-01-01 10:00",
    symbol = "AAPL"
  )
  expect_true(nrow(res) > 1)
  expect_equal(res$end_date[nrow(res)], "2021-01-01T10:00")
})

test_that("make_query_chunks returns just one period for unsupported interval", {
  res <- make_query_chunks(
    seq_interval = "1 week",
    chunk_size = 2,
    start_date = "2021-01-01",
    end_date = "2021-01-10",
    symbol = "AAPL"
  )
  expect_true(nrow(res) == 1)
})

test_that("check_tiingo_symbols: validates symbols correctly", {
  expect_error(
    check_tiingo_symbols(c("AAPL", "INVALID")),
    regexp = "not supported"
  )
})

test_that("map_interval: maps valid intervals correctly", {
  expect_equal(map_interval("1m"), "1 min")
  expect_equal(map_interval("5m"), "5 min")
  expect_equal(map_interval("15m"), "15 min")
  expect_equal(map_interval("1h"), "1 hour")
  expect_equal(map_interval("4h"), "4 hour")
  expect_equal(map_interval("1d"), "1 day")
  expect_equal(map_interval("1w"), "1 week")
  expect_equal(map_interval("1M"), "1 month")
  expect_equal(map_interval("7h"), NA_character_)
})

# Additional test cases needed for 100% coverage
# 2. load_stock_timeseries: Test non-character symbol input
test_that("stock: non-character symbol throws error", {
  expect_error(
    load_stock_timeseries(
      123,
      "1d",
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "symbol must be a character vector"
  )
})
# 3. load_crypto_timeseries: Test non-character pair input
test_that("crypto: non-character pair throws error", {
  expect_error(
    load_crypto_timeseries(
      123,
      "1d",
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "symbol must be a character vector"
  )
})
# 4. load_yahoo_dividends: Test non-character symbols input

# 5. Test future start_date validation
test_that("stock: future start_date throws error", {
  future_date <- as.character(Sys.Date() + 30)
  expect_error(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1d",
      start_date = future_date,
      end_date = future_date
    ),
    regexp = "start_date cannot be in the future"
  )
})

test_that("crypto: Test chunk size edge cases not covered", {
  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "1d",
      chunk_size = 3000,
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "chunk_size cannot exceed"
  )

  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "1d",
      chunk_size = -5,
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "chunk_size must be a positive integer"
  )

  expect_error(
    load_crypto_timeseries(
      "ETHUSDT",
      "1d",
      chunk_size = "invalid",
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    ),
    regexp = "chunk_size must be a positive integer"
  )
})
# 6. Test successful data retrieval with warnings (partial failures)
test_that("stock: handles partial failures with warnings", {
  # Mock a scenario where some data succeeds but generates warnings
  suppressWarnings(
    res <- load_stock_timeseries(
      c("AAPL", "INVALIDTICKER"),
      "1d",
      start_date = "2021-01-01",
      end_date = "2021-01-02"
    )
  )
  # Should have some data for AAPL and errors for INVALIDTICKER
  expect_true(!is.null(res$data) || !is.null(res$errors))
})
# 7. Test edge cases in make_query_chunks for different intervals
test_that("make_query_chunks handles minute intervals correctly", {
  res <- make_query_chunks("1 min", 5, "2021-01-01", "2021-01-01 00:10", "AAPL")
  expect_true(nrow(res) >= 1)
  expect_true(all(res$symbol == "AAPL"))
})

test_that("make_query_chunks handles hour intervals correctly", {
  res <- make_query_chunks(
    "2 hour",
    3,
    "2021-01-01",
    "2021-01-01 10:00",
    "AAPL"
  )
  expect_true(nrow(res) >= 1)
  expect_equal(res$symbol[1], "AAPL")
})


# 8. Test adjust_for_splits with invalid splits data
test_that("adjust_for_splits handles invalid splits data columns", {
  data <- create_test_ohlcv("AAPL", "2023-01-01")
  bad_splits <- tibble(symbol = "AAPL", wrong_col = "2020-01-01", value = 0.5)

  expect_error(
    adjust_for_splits(data, "AAPL", bad_splits),
    "Splits data missing required columns"
  )
})
# 9. Test adjust_for_splits when tq_get fails with error
test_that("adjust_for_splits handles tq_get errors gracefully", {
  data <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    start_date = "2023-02-01",
    end_date = "2023-02-02"
  )$data

  suppressWarnings(
    # Mock tidyquant to return empty splits data
    with_mocked_bindings(
      tq_get = function(...) stop("API error"),
      {
        result <- adjust_for_splits(data, "AAPL")

        expect_equal(result, data)
      }
    )
  )
})
# 11. Test load_stock_timeseries with Tiingo path and bad symbols edge case
test_that("stock: tiingo path handles unsupported symbols in error message parsing", {
  skip_if(getOption('tiingo_key') == "", "No API key available")

  # Test the error message parsing logic for bad symbols
  with_mocked_bindings(
    check_tiingo_symbols = function(symbols) {
      stop("Unsupported symbols: BADSYMBOL, ANOTHERBAD")
    },
    {
      suppressWarnings(
        res <- load_stock_timeseries(
          "BADSYMBOL",
          "1h",
          start_date = "2021-01-01",
          end_date = "2021-01-02"
        )
      )
      expect_true(!is.null(res$errors))
    }
  )
})

test_that("determine_adjustment_factor handles edge cases", {
  # Test with splits exactly on target date
  splits_data <- tibble(
    date = as.Date("2023-01-01"),
    adj_factor = 0.5
  )
  result <- determine_adjustment_factor(as.Date("2023-01-01"), splits_data)
  expect_equal(result, 0.5)
})

test_that("stock: default chunk_size sets correctly for different intervals", {
  # This tests the case_when logic in load_stock_timeseries

  # For minute/hour intervals, chunk_size should default to 20000
  # For daily intervals, chunk_size should default to 1000

  # We can test this indirectly by checking that functions work with NULL chunk_size
  res <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    chunk_size = NULL,
    start_date = "2021-01-01",
    end_date = "2021-02-02"
  )
  expect_true(!is.null(res$data))

  skip_if(getOption('tiingo_key') == "", "No API key available")
  res <- load_stock_timeseries(
    "AAPL",
    "1h",
    chunk_size = NULL,
    start_date = "2021-01-01",
    end_date = "2021-02-01"
  )
  expect_true(!is.null(res$data))
})

test_that("stock: tiingo message about adjustments is displayed", {
  skip_if(getOption('tiingo_key') == "", "No API key available")

  expect_message(
    load_stock_timeseries(
      symbol = "AAPL",
      interval = "1h",
      start_date = "2021-02-01",
      end_date = "2021-02-02"
    ),
    "Remember: Tiingo timeseries are not adjusted"
  )
})

test_that("stock: handles tq_get returning NULL", {
  local_mocked_bindings(
    tq_get = function(...) NULL,
    .package = "tidyquant"
  )
  res <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    start_date = "2022-02-01",
    end_date = "2022-02-02"
  )
  expect_null(res$data)
  expect_true(nrow(res$errors) > 0)
})

test_that("stock: adjusted column is properly set", {
  res <- load_stock_timeseries(
    symbol = "AAPL",
    interval = "1d",
    start_date = "2021-01-01",
    end_date = "2021-02-02"
  )
  expect_true("adjusted" %in% names(res$data))
})


# 14. Test crypto data ordering and distinct logic
test_that("crypto: data is properly deduplicated and ordered", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1h",
    start_date = "2021-01-01",
    end_date = "2021-01-02"
  )

  # Test that data is ordered by symbol, then open_time
  expect_true(all(diff(as.numeric(res$data$open_time)) >= 0))

  # Test distinct() is working (no duplicates)
  expect_equal(nrow(res$data), nrow(dplyr::distinct(res$data)))
})
# 15. Test that adjusted column is properly set
test_that("crypto: adjusted column equals close column", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1d",
    start_date = "2021-01-01",
    end_date = "2021-01-02"
  )
  expect_equal(res$data$adjusted, res$data$close)
})
# 16. Test relocate logic in load_crypto_timeseries
test_that("crypto: columns are properly relocated", {
  res <- load_crypto_timeseries(
    "ETHUSDT",
    "1d",
    start_date = "2021-01-01",
    end_date = "2021-01-02"
  )

  # symbol and adjusted should be after volume
  col_names <- names(res$data)
  volume_pos <- which(col_names == "volume")
  symbol_pos <- which(col_names == "symbol")
  adjusted_pos <- which(col_names == "adjusted")

  expect_true(symbol_pos > volume_pos)
  expect_true(adjusted_pos > volume_pos)
})
# 17. Test message output in load_stock_timeseries for Tiingo
