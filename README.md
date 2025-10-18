
<!-- README.md is generated from README.Rmd. Please edit that file -->

# invest.data

<!-- badges: start -->

<!-- badges: end -->

`invest.data` is an R package that provides wrappers to fetch and
prepare different types of financial and investment data from online
sources. Three groups of data are currently supported: 1- stock and
crypto OHLCV time series, fetched from **Yahoo Finance, Tiingo, and
Binance**. 2- base data on stock symbols and benchmarks, scraped from
different online sources. 3- financial data, fetched from **Yahoo
Finance** (e.g. dividends, splits) or scraped from other sources
(e.g. earnings, analyst estimates, etc…).

It is designed to be used by other domestic projects, offering
consistent data structures, error tracking, and preprocessing helpers
(e.g. split adjustment).

Currently, the functions on group 2 are restricted to the universes of
ASX (Australia), B3 (Brazil) and SP500 (US focused). Group 3 functions
are still under development and only US stocks are supported at the
moment.

------------------------------------------------------------------------

## Installation

You can install the development version of **yourpackage** from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("henrique-anatole/invest.data", dependencies = TRUE)
```

------------------------------------------------------------------------

## API Key Setup

Some functions require API keys (e.g., **Tiingo** for stock data and
**Binance** for crypto data). Both are managed with helper functions:

- `set_tiingo_api_key()`
- `set_binance_api_key()`

Each function looks for your credentials in the following order:

1.  Direct input via function arguments
2.  Values already set in `options()`
3.  Environment variables (`TIINGO_KEY`, `BIN_KEY`, `BIN_SECRET`)
4.  Secure storage in the system keyring (`keyring` package)

If no valid key is found, an error is raised with setup instructions.

------------------------------------------------------------------------

### Recommended Method: Keyring (Secure Storage)

This is the safest way to store credentials, since they are encrypted
and managed by the OS.

``` r
# Store keys securely (one-time setup)
keyring::key_set("tiingo_key")
keyring::key_set("binance_key")
keyring::key_set("binance_secret")

# Later, in your session just call:
set_tiingo_api_key()
set_binance_api_key()
```

------------------------------------------------------------------------

### Alternative Methods (Less Secure)

- **Direct Input (scripts / quick testing):**

  ``` r
  set_tiingo_api_key("YOUR_TIINGO_KEY")
  set_binance_api_key("YOUR_BINANCE_KEY", "YOUR_BINANCE_SECRET")
  ```

- **Environment Variables (persistent, but stored in plain text):** Add
  to `~/.Renviron` or set in your shell:

  ``` r
  Sys.setenv(TIINGO_KEY = "YOUR_TIINGO_KEY")
  Sys.setenv(BIN_KEY = "YOUR_BINANCE_KEY", BIN_SECRET = "YOUR_BINANCE_SECRET")
  ```

- **Options (only lasts for the current session):**

  ``` r
  options(tiingo_key = "YOUR_TIINGO_KEY")
  options(bin_key = "YOUR_BINANCE_KEY", bin_secret = "YOUR_BINANCE_SECRET")
  ```

------------------------------------------------------------------------

✅ **Recommendation:** Use **`keyring`** whenever possible.  
⚠️ Use **environment variables** only if running in automated pipelines
or servers where keyring is unavailable.  
🚫 Avoid hardcoding keys in scripts that may be shared or
version-controlled.

------------------------------------------------------------------------

## 1. Stock and Crypto OHLCV Time Series

The package provides a small set of functions that wrap around existing
APIs and packages:

- **`load_stock_timeseries()`**  
  Retrieve **daily OHLCV** stock data from Yahoo Finance, or **intraday
  OHLCV** data from Tiingo.  
  Handles chunked queries, split adjustment, and returns results
  together with failed queries.

- **`load_crypto_timeseries()`**  
  Retrieve OHLCV data for crypto pairs (e.g. `"ETHUSDT"`) from
  **Binance**.  
  Supports multiple intervals and query chunking.

- **`load_yahoo_dividends()`**  
  Retrieve **dividend payments** for one or more tickers from Yahoo
  Finance.  
  Returns both successfully retrieved dividends and a log of any failed
  symbols.

All functions return a **list** with two slots: - `data`: tibble with
the requested time series or dividends.  
- `errors`: tibble with failed queries, or `NULL` if no errors occurred.

This makes it easy to both use the data and inspect problems without
interrupting workflows.

### How does it compare to other packages?

Several R packages already provide access to financial data:

- **`tidyquant::tq_get()`**  
  Flexible and general-purpose. Supports Yahoo, Tiingo, Alpha Vantage,
  and more.  
  *Difference*: `invest.data` builds on `tq_get()` but adds **query
  chunking**, **error logging**, and **consistent output structures**
  (`data` + `errors`).

- **`quantmod::getSymbols()`**  
  One of the oldest tools for financial data in R. Primarily designed
  for **xts/zoo** workflows.  
  *Difference*: `invest.data` returns **tibbles**, integrates with the
  tidyverse, and provides **crypto support**.

- **`riingo`**  
  Official Tiingo client. Useful for low-level Tiingo queries.  
  *Difference*: `invest.data` abstracts over multiple providers (Yahoo,
  Tiingo, Binance) with the **same interface** and adds error handling.

- **`BatchGetSymbols`**  
  Convenient batch retrieval of daily Yahoo Finance data.  
  *Difference*: `invest.data` is more general (intraday + crypto +
  dividends) and maintains the same return structure across all data
  types.

In short, `invest.data` focuses less on being a full-featured API
client, and more on being a **unified, resilient interface** for
fetching stock, crypto, and dividend data with minimal friction.

### Examples

Load daily prices from Yahoo Finance:

``` r
library(invest.data)

stock_test <- load_stock_timeseries(
  symbol     = c("AAPL", "MSFT", "INVALID"),  # includes an invalid symbol to demonstrate error logging
  interval   = "1d",
  start_date = "2021-10-01",
  end_date   = "2022-02-01"
)

head(stock_test$data)
head(stock_test$errors)
```

Load hourly intraday data (Tiingo):

``` r
stock_test_hour <- load_stock_timeseries(
  symbol     = c("AAPL", "MSFT", "INVALID"),  # includes an invalid symbol to demonstrate error logging
  interval   = "1h",
  start_date = "2021-10-01",
  end_date   = "2021-10-02"
)

head(stock_test_hour$data)
head(stock_test_hour$errors)
```

Load crypto OHLCV from Binance:

``` r
crypto_test <- load_crypto_timeseries(
  pair       = c("ETHUSDT", "BTCUSDT", "INVALID"),  # includes an invalid pair to demonstrate error logging
  interval   = "1m",
  start_date = "2021-01-01",
  end_date   = "2021-01-02"
)

head(crypto_test$data)
head(crypto_test$errors)
```

Load dividends from Yahoo Finance:

``` r
dividends <- load_yahoo_dividends(
  symbols    = c("AAPL", "MSFT", "INVALID"),  # includes an invalid symbol to demonstrate error logging
  start_date = "2020-01-01",
  end_date   = "2022-01-01"
)

head(dividends$data)
head(dividends$errors)
```

------------------------------------------------------------------------

## 2. Stock Symbols and Benchmarks

The package provides functions to fetch comprehensive stock universe
data from major exchanges and create benchmark symbols for performance
comparison. These functions return standardized tibbles with consistent
structure across different markets.

### Available Functions

- **`scrap_sp500_symbols()`** - S&P 500 US stocks
- **`scrap_asx_symbols()`** - ASX Australian stocks  
- **`scrap_b3_symbols()`** - B3 Brazilian stocks
- **`create_benchmarks()`** - Common financial benchmarks

All functions return a tibble with the following standardized columns:

| Column | Description |
|----|----|
| `symbol` | Ticker symbol (with exchange suffix: `.AX` for ASX, `.SA` for B3) |
| `name` | Company name |
| `sector` | Standardized sector classification |
| `subsector` | Standardized subsector classification |
| `market_cap` | Market capitalization in local currency |
| `estimated_tot_shares` | Approximate total shares outstanding |
| `index` | Index name (“SP500”, “ASX”, “B3”, “Benchmark”) |
| `rank` | Company rank within index (by market cap) |
| `coin` | Currency (“USD”, “AUD”, “BRL”) |
| `weight` | Relative weight based on market cap |
| `date_updated` | Date of data retrieval |
| `source` | Data source URL or description |

### Selenium Requirement

Some functions (`scrap_asx_symbols()` and `scrap_b3_symbols()`) require
**Selenium** and a compatible web browser (e.g., Firefox) to scrape
dynamic content from websites.

``` r
# Install selenium if needed
install.packages("RSelenium")
# Firefox must be installed on system
```

### Examples

#### S&P 500 Symbols

``` r
library(invest.data)

# Get current S&P 500 constituents
sp500 <- scrap_sp500_symbols()

# View top companies by market cap
sp500 %>%
  select(symbol, name, sector, market_cap, weight) %>%
  head(10)

# Sector distribution
sp500 %>%
  count(sector, sort = TRUE)
```

#### ASX Symbols

``` r
# Get ASX-listed companies (requires Selenium/Firefox)
asx_data <- scrap_asx_symbols()

# View Australian market structure
asx_data %>%
  group_by(sector) %>%
  summarise(
    companies = n(),
    total_mcap = sum(market_cap, na.rm = TRUE) / 1e9,
    avg_weight = mean(weight, na.rm = TRUE)
  ) %>%
  arrange(desc(total_mcap))
```

#### B3 Symbols (Brazil)

``` r
# Get B3/IBOV constituents (requires Selenium/Firefox)
b3_data <- scrap_b3_symbols()

# Brazilian market overview
b3_data %>%
  select(symbol, name, sector, weight) %>%
  arrange(desc(weight)) %>%
  head(15)
```

#### Financial Benchmarks

``` r
# Get common benchmarks for performance comparison
benchmarks <- create_benchmarks()

# View available benchmarks
benchmarks %>%
  select(symbol, name, source) %>%
  print(n = Inf)
```

### Use Cases

#### Portfolio Construction

``` r
# Get complete investment universe
universe <- bind_rows(
  scrap_sp500_symbols(),
  scrap_asx_symbols(),
  scrap_b3_symbols()
)

# Filter by sector and market cap
tech_stocks <- universe %>%
  filter(sector == "Information Technology", market_cap > 10e9)

# Create diversified portfolio weights
portfolio <- universe %>%
  group_by(index) %>%
  slice_max(market_cap, n = 5) %>%
  mutate(weight = market_cap / sum(market_cap)) %>%
  ungroup()
```

#### Performance Benchmarking

``` r
# Combine stock universes with benchmarks
all_assets <- bind_rows(
  scrap_sp500_symbols(),
  create_benchmarks()
)

# Use with time series functions
benchmark_prices <- load_stock_timeseries(
  symbol = c("AAPL", "MSFT", "^GSPC", "VT"),  # Stocks + benchmarks
  interval = "1d",
  start_date = "2023-01-01"
)
```

### Technical Notes

- **Selenium Requirements**: ASX and B3 functions require Selenium and
  Firefox for web scraping dynamic content
- **Data Freshness**: All functions include a `date_updated` column
  indicating when data was retrieved
- **Error Handling**: Functions return `NULL` with a warning if scraping
  fails
- **Sector Standardization**: All sectors and subsectors are
  standardized across different exchanges for consistency

### Data Sources

| Function                | Primary Sources                     |
|-------------------------|-------------------------------------|
| `scrap_sp500_symbols()` | tidyquant, Wikipedia, StockAnalysis |
| `scrap_asx_symbols()`   | MarketIndex ASX listed companies    |
| `scrap_b3_symbols()`    | B3 official IBOV page               |
| `create_benchmarks()`   | Curated list of common benchmarks   |

------------------------------------------------------------------------

This section maintains the same structure and standards as your existing
README, with clear examples, consistent formatting, and practical use
cases that demonstrate how these functions integrate with the time
series functions from the previous section.
