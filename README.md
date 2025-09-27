
<!-- README.md is generated from README.Rmd. Please edit that file -->

# invest.funks

<!-- badges: start -->

<!-- badges: end -->

`invest.funks` is a lightweight R package that provides convenient
wrappers to fetch and prepare financial time series data from **Yahoo
Finance, Tiingo, and Binance**. It is designed to be used by other
projects in the same ecosystem, offering consistent data structures,
error tracking, and preprocessing helpers (e.g. split adjustment).

## Installation

You can install the development version of **invest.funks** from GitHub
with:

# install.packages(“pak”)

pak::pak(“henrique-anatole/invest.funks”)

## Core functions

The package provides a small set of opinionated functions that wrap
around existing APIs and packages:

- **`load_stock_timeseries()`** Retrieve **daily OHLCV** stock data from
  Yahoo Finance, or **intraday OHLCV** data from Tiingo. Handles chunked
  queries, split adjustment, and returns results together with failed
  queries.

- **`load_crypto_timeseries()`** Retrieve OHLCV data for crypto pairs
  (e.g. `"ETHUSDT"`) from **Binance**. Supports multiple intervals and
  query chunking.

- **`load_yahoo_dividends()`** Retrieve **dividend payments** for one or
  more tickers from Yahoo Finance. Returns both successfully retrieved
  dividends and a log of any failed symbols.

All functions return a **list** with two slots:

- `data`: tibble with the requested time series or dividends.
- `errors`: tibble with failed queries, or `NULL` if no errors occurred.

This makes it easy to both use the data and inspect problems without
interrupting workflows.

## Example

Load Apple’s daily prices from Yahoo Finance:

``` r
library(invest.funks)

stock_test <- load_stock_timeseries(
  symbol     = "AAPL",
  interval   = "1d",
  start_date = "2021-10-01",
  end_date   = "2022-02-01"
)

head(stock_test$data)
```

Load hourly intraday data (Tiingo):

``` r
stock_test_hour <- load_stock_timeseries(
  symbol     = "AAPL",
  interval   = "1h",
  start_date = "2021-10-01",
  end_date   = "2021-10-02"
)

head(stock_test_hour$data)
```

Load crypto OHLCV from Binance:

``` r
crypto_test <- load_crypto_timeseries(
  pair       = "ETHUSDT",
  interval   = "1m",
  start_date = "2021-01-01",
  end_date   = "2021-01-02"
)

head(crypto_test$data)
```

Load dividends from Yahoo Finance:

``` r
dividends <- load_yahoo_dividends(
  symbols    = c("AAPL", "MSFT"),
  start_date = "2020-01-01",
  end_date   = "2022-01-01"
)

head(dividends$data)
```
