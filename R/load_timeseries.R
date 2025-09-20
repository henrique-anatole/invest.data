#' load_crypto_timeseries
#' Load the timeseries from binance.
#' 
#' @param pair Crypto pair symbol (e.g. "ETHUSDT")
#' @param interval Time interval between two data points in the time series.
#' One of '1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d', '3d', '1w', '1M'
#' @param limit Number of observations to retrieve per request (max 1000) - the function will make multiple requests if needed
#' @param start_date Start date of the time series (YYYY-MM-DD)
#' @param end_date End date of the time series (YYYY-MM-DD)
#' @param source Data source. Currently only "binance" is supported.
#' 
#' @return A tibble with the time series data
#' @import dplyr
#' @import stringr
#' @import lubridate
#' 
#' @export
#' @examples
#' # Load 1 minute time series for ETHUSDT from 2021-01-01 to 2021-07-01
#' crypto_test = load_crypto_timeseries(pair = "ETHUSDT", interval="1m", start_date="2021-01-01", end_date="2021-01-02")
#' # Load 15 minute time series for ETHUSDT from 2021-01-01 to 2021-07-01
#' crypto_test = load_crypto_timeseries(pair = "ETHUSDT", interval="15m", start_date="2021-01-01", end_date="2021-01-02")
#' # Load 1 hour time series for ETHUSDT from 2021-01-01 to 2021-07-01
#' crypto_test = load_crypto_timeseries(pair = "ETHUSDT", interval="1h", start_date="2021-01-01", end_date="2021-01-02")
#' 
load_crypto_timeseries <- function(pair, interval, limit=1000, start_date, end_date, source="binance") {
  
  # check date-time formats
    # Check if is a valid YYYY-MM-DD
    # if is date or posixct, change to character
  if (!lubridate::is.Date(start_date) && !lubridate::is.POSIXct(start_date)) {
    if (is.na(lubridate::as_date(start_date))) {
      stop("start_date is not a valid date. Please use YYYY-MM-DD format.")
    } else {
      start_date <- as.character(lubridate::as_date(start_date))
    }
  } else {
    start_date <- as.character(lubridate::as_date(start_date))
  }

  if (!lubridate::is.Date(end_date) && !lubridate::is.POSIXct(end_date)) {
    if (is.na(lubridate::as_date(end_date))) {
      stop("end_date is not a valid date. Please use YYYY-MM-DD format.")
    } else {
      end_date <- as.character(lubridate::as_date(end_date))
    }
  } else {
    end_date <- as.character(lubridate::as_date(end_date))
  }
  
  # check intervals
  seq_interval <- dplyr::case_when(
    interval == '1m' ~ '1 min',
    interval == '3m' ~ '3 min',
    interval == '5m' ~ '5 min',
    interval == '15m' ~ '15 min',
    interval == '30m' ~ '30 min',
    interval == '1h' ~ '1 hour',
    interval == '2h' ~ '2 hour',
    interval == '4h' ~ '4 hour',
    interval == '6h' ~ '6 hour',
    interval == '8h' ~ '8 hour',
    interval == '12h' ~ '12 hour',
    interval == '1d' ~ '1 day',
    interval == '3d' ~ '3 day',
    interval == '1w' ~ '1 week',
    interval == '1M' ~ '1 month',
    TRUE ~ "what?")
  
    if (seq_interval == "what?") {

      stop("Please, select an interval between c('1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d', '3d', '1w', '1M')")
      
    }
  
  number <- as.numeric(stringr::str_extract(seq_interval, "[0-9]+"))
  period <- stringr::str_extract(seq_interval, "[a-z]+")
  by <- paste(limit*number, period)
  
  
  #check how many ticks of 1000 x interval are needed
  
  seq_quantity <- length(seq(as.POSIXct(start_date, tz="UTC"),as.POSIXct(end_date, tz="UTC"),by=by))
  seq_time <- dplyr::case_when(
    period == "min" ~ lubridate::minutes(limit*number-1),
    period == "hour" ~ lubridate::hours(limit*number-1),
    period == "day" ~ lubridate::days(limit*number-1),
    period == "week" ~ lubridate::weeks(limit*number-1),
    period == "month" ~ months(limit*number-1),
  )

    
  start_date_seq <- seq(as.POSIXct(start_date, tz="UTC"),as.POSIXct(end_date, tz="UTC"),by=by)
  
  end_date_seq <- start_date_seq+seq_time
  end_date_seq[length(end_date_seq)] <- as.POSIXct(end_date, tz="UTC")

  #Create a dataframe with the series
    sequency <- dplyr::tibble(pair = pair, start_date=start_date_seq, end_date=end_date_seq)

  #Run a looping merging the results
    results = NULL
    
    for (i in 1:dim(sequency)[1]) {

      temp <- binance_klines(pair
                             , interval = interval
                             , limit = limit
                             , start_time = as.character(sequency$start_date[i])
                             , end_time = as.character(sequency$end_date[i]))
      
      results <- bind_rows(results,temp)
      
    }
  
    return(results)
  #format the data to export
  
}

#' load_yahoo_dividends
#' Load dividends from yahoo finance using tidyquant.
#' 
#' @param list_tikers A vector of stock symbols
#' @param start_date Start date of the time series (YYYY-MM-DD)
#' @param end_date End date of the time series (YYYY-MM-DD). Default is today's date.
#' @return A tibble with the dividends data
#' @import dplyr
#' @import tidyquant
#' @export
#' @examples
#' #' # Load dividends for AAPL and MSFT from 2020-01-01 to 2022-01-01
#' dividends = load_yahoo_dividends(list_tikers = c("AAPL", "MSFT"), start_date = "2020-01-01", end_date = "2022-01-01")
#' 
# for each symbol, try to get the dividends using tidyquant. Return a list of symbols that have dividends
load_yahoo_dividends <- function(list_tikers, start_date, end_date = as.character(Sys.Date())) {
  
  # Initialize an empty tibble to store the results
  symbols <- c()
  dividends <- tibble()
  
  # Loop through each symbol and try to get the dividends
  for (i in list_tikers) {
    
    tryCatch({
      
      temp <- tidyquant::tq_get(x = i,
                                get = "dividends",
                                from = start_date,
                                to = end_date
                                )
      
      symbols <- c(symbols, i)
      dividends <- rbind(dividends, temp)
      
    }, error = function(e) {
      
      print(paste0(i, " got no dividends"))
      
    })
    
  }
  
  return(dividends)
  
}




#' load_stock_timeseries
#'
#' Load stock timeseries from Yahoo Finance (daily) or Tiingo (intraday).
#'
#' @param symbol Character vector. Stock ticker(s).
#' @param interval Character. One of '1m','3m','5m','15m','30m',
#'   '1h','2h','4h','6h','8h','12h','1d'.
#' @param chunk_size Integer. Number of observations per query (max 1000).
#' @param start_date Character in "YYYY-MM-DD".
#' @param end_date Character in "YYYY-MM-DD".
#' @param adjust_splits Logical. Adjust prices for splits (Tiingo only).
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{Tibble with OHLCV timeseries.}
#'     \item{errors}{Tibble with failed query ranges or NULL.}
#'   }
#' @import dplyr tidyquant riingo lubridate stringr zoo purrr
#' @export
#' @examples
#' # Load daily time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_day = load_stock_timeseries(symbol = "AAPL",
#' interval="1d",
#' start_date="2021-10-01",
#' end_date="2022-02-01")
#' # Load daily time series for AAPL and MSFT from 2021-10-01 to 2022-02-01
#' stock_test_multi = load_stock_timeseries(symbol = c("AAPL", "MSFT"),
#' interval="1d",
#' start_date="2021-10-01",
#' end_date="2022-02-01")
#' # # Load hourly time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_hour = load_stock_timeseries(symbol = "AAPL",
#' interval="1h",
#' start_date="2021-10-01",
#' end_date="2022-02-01")
#' 
load_stock_timeseries <- function(symbol,
                                  interval,
                                  chunk_size = 1000,
                                  start_date,
                                  end_date,
                                  adjust_splits = TRUE) {

  #---- Validation ----
  validate_inputs(symbol, interval, chunk_size, start_date, end_date)

  # Map interval
  seq_interval <- map_interval(interval)

  # Decide source
  tiingo <- grepl("m|h", interval)
  source <- if (tiingo) "tiingo.iex" else "stock.prices"

  if (tiingo) {
    # Check Tiingo API key
    api <- set_tiingo_api_key()
    tidyquant::tiingo_api_key(api)
    check_tiingo_symbols(symbol)
    message("Remember: Tiingo timeseries are not adjusted for splits/dividends")
  }

  # Split date range into chunks
  queries <- make_query_chunks(seq_interval, chunk_size, start_date, end_date, symbol)

  # Run queries
  results <- list(data = NULL, errors = NULL)

  results_list <- purrr::map(1:nrow(queries), function(i) {
    tryCatch(
      {
        temp <- tidyquant::tq_get(
          x      = queries$symbol[i],
          get    = source,
          from   = queries$start_date[i],
          to     = queries$end_date[i],
          resample_frequency = gsub(" ", "", seq_interval)
        )
        if (is.null(temp) || nrow(temp) == 0) {
          warning("No data for ", queries$symbol[i], " in ", queries$start_date[i], " - ", queries$end_date[i])
          return(list(data = NULL, error = queries[i,]))
        }
        list(data = temp, error = NULL)
      },
      error = function(e) {
        message("Error: ", e$message)
        list(data = NULL, error = queries[i,])
      }
    )
  })

  # Combine results & errors
  all_data   <- purrr::map_dfr(results_list, "data") %>% 
    distinct()
  all_errors <- purrr::map_dfr(results_list, "error")

  # Adjust for splits if Tiingo
  if (adjust_splits && tiingo && nrow(all_data) > 0) {
    all_data <- adjust_for_splits(all_data, symbol)
  }

  # Final formatting
  if (nrow(all_data) == 0) {
    results$data   <- NULL
    results$errors <- if (nrow(all_errors) > 0) all_errors else NULL
  } else {
    results$data <- all_data %>%
      dplyr::mutate(symbol = symbol) %>%   # ensure column exists
      dplyr::rename(open_time = "date") %>%
      dplyr::relocate(symbol, .after = volume)
    results$errors <- if (nrow(all_errors) > 0) all_errors else NULL
  }

  return(results)
}

#---- Helper Functions ----
#' Validate inputs
#' 
#' Check types, formats and ranges passed to other functions.
#' @param symbol Character vector. Stock ticker(s).
#' @param interval Character. One of '1m','3m','5m','15m','30m',
#'  '1h','2h','4h','6h','8h','12h','1d'.
#' @param chunk_size Integer. Number of observations per query (max 1000).
#' @param start_date Character in "YYYY-MM-DD".
#' @param end_date Character in "YYYY-MM-DD".
#' @return NULL. Stops with error if invalid.
#' @noRd
#' @import dplyr
#' 
validate_inputs <- function(symbol, interval, chunk_size, start_date, end_date) {
  if (!is.character(symbol)) stop("symbol must be a character vector")
  if (!is_valid_date(start_date)) stop("start_date must be YYYY-MM-DD")
  if (!is_valid_date(end_date)) stop("end_date must be YYYY-MM-DD")
  if (as.Date(start_date) > as.Date(end_date)) stop("start_date must be before end_date")
  if (chunk_size > 10000) stop("chunk_size cannot exceed 10000 (API limits)")
  valid_intervals <- c("1m","3m","5m","15m","30m","1h","2h","4h","6h","8h","12h","1d")
  if (!(interval %in% valid_intervals)) {
    stop("Invalid interval. Must be one of: ", paste(valid_intervals, collapse = ", "))
  }
}

#' map_interval
#' 
#' Check if string is valid interval and map to tidyquant format.
#' 
#' @param interval Character. One of '1m','3m','5m','15m','30m',
#' '1h','2h','4h','6h','8h','12h','1d'.
#' @return character string to match tidyquant interval format or NA
#' @noRd
#' @import dplyr
#' 
map_interval <- function(interval) {
  dplyr::case_when(
    interval == "1m"  ~ "1 min",
    interval == "3m"  ~ "3 min",
    interval == "5m"  ~ "5 min",
    interval == "15m" ~ "15 min",
    interval == "30m" ~ "30 min",
    interval == "1h"  ~ "1 hour",
    interval == "2h"  ~ "2 hour",
    interval == "4h"  ~ "4 hour",
    interval == "6h"  ~ "6 hour",
    interval == "8h"  ~ "8 hour",
    interval == "12h" ~ "12 hour",
    interval == "1d"  ~ "1 day",
    TRUE ~ NA_character_
  )
}

#' check_tiingo_symbols
#' 
#' Check if symbols are supported by Tiingo.
#' @param symbols Character vector. Stock ticker(s).
#' @return NULL. Stops with error if any unsupported.
#' @noRd
#' @import purrr riingo
#' 
check_tiingo_symbols <- function(symbols) {
  unsupported <- purrr::keep(symbols, ~ !riingo::is_supported_ticker(.x))
  if (length(unsupported) > 0) {
    stop("These symbols are not supported on Tiingo: ", paste(unsupported, collapse = ", "))
  }
}

#' make_query_chunks
#' 
#' Split date range into chunks based on interval and chunk size.
#' @param seq_interval Character. Interval in tidyquant format (e.g. "1 min").
#' @param chunk_size Integer. Number of observations per query (max 1000).
#' @param start_date Character in "YYYY-MM-DD".
#' @param end_date Character in "YYYY-MM-DD".
#' @param symbol Character. Stock ticker.
#' @return Tibble with columns: symbol, start_date, end_date.
#' @noRd
#' @import dplyr stringr lubridate tidyquant 
#' 
make_query_chunks <- function(seq_interval, chunk_size, start_date, end_date, symbol) {
  number <- as.numeric(stringr::str_extract(seq_interval, "[0-9]+"))
  period <- stringr::str_extract(seq_interval, "[a-z]+")
  by     <- paste(chunk_size * number, period)

  step <- dplyr::case_when(
    period == "min"  ~ lubridate::minutes(chunk_size*number-1),
    period == "hour" ~ lubridate::hours(chunk_size*number-1),
    period == "day"  ~ lubridate::days(chunk_size*number-1)
  )

  start_seq <- seq(as.POSIXct(start_date, tz = "UTC"),
                   as.POSIXct(end_date, tz = "UTC"),
                   by = by)

  end_seq <- start_seq + step
  end_seq[length(end_seq)] <- as.POSIXct(end_date, tz = "UTC")

  dplyr::tibble(
    symbol     = symbol,
    start_date = lubridate::format_ISO8601(start_seq, precision = "ymdhm"),
    end_date   = lubridate::format_ISO8601(end_seq,   precision = "ymdhm")
  )
}


#' adjust_for_splits
#' 
#' Adjust OHLCV prices for stock splits using Tiingo splits data.
#' @param data Tibble with OHLCV timeseries.
#' @param symbols Character vector. Stock ticker(s).
#' @return Tibble with adjusted OHLCV prices.
#' @noRd
#' @import dplyr tidyquant zoo
#' 
adjust_for_splits <- function(data, symbols) {

  splits <- tidyquant::tq_get(symbols, get = "splits",
                              from = "1900-01-01", to = Sys.Date())
  
  if (is.null(splits) || nrow(splits) == 0) {
    message("No splits found, returning unadjusted data")
    return(data)
  }
  ratios <- splits %>%
    dplyr::mutate(adjRatio = rev(cumprod(rev(value)))) %>%
    dplyr::rename(day = date)

  data %>%
    dplyr::mutate(day = as.Date(date)) %>%
    dplyr::left_join(ratios, by = c("day", "symbol")) %>%
    dplyr::mutate(
      adjRatio = dplyr::lead(adjRatio, 1),
      adjRatio = zoo::na.locf(adjRatio, fromLast = TRUE, na.rm = FALSE),
      adjRatio = zoo::na.fill(adjRatio, fill = 1.0),
      open  = open  * adjRatio,
      high  = high  * adjRatio,
      low   = low   * adjRatio,
      close = close * adjRatio,
      volume = volume / adjRatio
    ) %>%
    dplyr::select(-c(day, value, adjRatio))
}




