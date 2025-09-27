#' load_crypto_timeseries ############
#'
#' Load crypto timeseries from Binance.
#'
#' @param pair Character vector. Crypto pair(s), e.g. "ETHUSDT".
#' @param interval Character. One of '1m','3m','5m','15m','30m','1h',
#'   '2h','4h','6h','8h','12h','1d','3d','1w','1M'.
#' @param chunk_size Integer. Number of observations per query (max 1000).
#' @param start_date Character in "YYYY-MM-DD".
#' @param end_date Character in "YYYY-MM-DD".
#' @param source Character. Currently only "binance" supported.
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{Tibble with OHLCV timeseries.}
#'     \item{errors}{Tibble with failed query ranges or NULL.}
#'   }
#' @import dplyr stringr lubridate binancer purrr
#' @export
#'
#' @examples
#' # Load 1 minute time series for ETHUSDT
#' crypto_test <- load_crypto_timeseries(
#'   pair = "ETHUSDT",
#'   interval = "1m",
#'   start_date = "2021-01-01",
#'   end_date   = "2021-01-02"
#' )
load_crypto_timeseries <- function(
  pair,
  interval,
  chunk_size = 1000,
  start_date,
  end_date,
  source = "binance"
) {
  if (source != "binance") {
    stop("Currently only 'binance' is supported as source")
  }

  valid_intervals <- c(
    "1m",
    "3m",
    "5m",
    "15m",
    "30m",
    "1h",
    "2h",
    "4h",
    "6h",
    "8h",
    "12h",
    "1d",
    "3d",
    "1w",
    "1M"
  )
  max_chunk_size <- 1000
  validate_inputs(
    symbol = pair,
    interval,
    chunk_size,
    max_chunk_size,
    start_date,
    end_date,
    valid_intervals
  )

  seq_interval <- map_interval(interval)

  # Run queries for each pair using purrr::map
  pair_results <- purrr::map(pair, function(p) {
    queries <- make_query_chunks(
      seq_interval,
      chunk_size,
      start_date,
      end_date,
      p
    )

    results_list <- purrr::map(1:nrow(queries), function(i) {
      tryCatch(
        {
          temp <- binancer::binance_klines(
            symbol = queries$symbol[i],
            interval = interval,
            limit = chunk_size,
            start_time = queries$start_date[i],
            end_time = queries$end_date[i]
          )
          list(data = temp, error = NULL)
        },
        error = function(e) {
          # Record the invalid symbol in error tibble
          err <- queries[i, ] %>%
            dplyr::mutate(error_message = e$message)
          list(data = NULL, error = err)
        }
      )
    })

    list(
      data = purrr::map_dfr(results_list, "data"),
      errors = purrr::map_dfr(results_list, "error")
    )
  })

  # Combine all pairs
  all_data <- purrr::map_dfr(pair_results, "data")
  all_errors <- purrr::map_dfr(pair_results, "errors")

  # Preserve original pair order
  if (!is.null(all_data) && nrow(all_data) > 0) {
    all_data <- all_data %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        symbol = factor(symbol, levels = pair),
        adjusted = close
      ) %>%
      dplyr::arrange(symbol, open_time) %>%
      dplyr::relocate(c(symbol, adjusted), .after = "volume") %>%
      dplyr::distinct()
  }

  results <- list(
    data = if (nrow(all_data) > 0) all_data else NULL,
    errors = if (nrow(all_errors) > 0) all_errors else NULL
  )

  return(results)
}

#' load_stock_timeseries ############
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
#' end_date="2021-11-01")
#'
load_stock_timeseries <- function(
  symbol,
  interval,
  chunk_size = NULL,
  start_date,
  end_date,
  adjust_splits = TRUE
) {
  # Determine chunk size
  if (is.null(chunk_size)) {
    chunk_size <- case_when(
      grepl("m|h", interval) ~ 20000, # intraday max for Tiingo
      TRUE ~ 1000 # daily max for Yahoo
    )
  }

  # Validation
  valid_intervals <- c(
    "1m",
    "3m",
    "5m",
    "15m",
    "30m",
    "1h",
    "2h",
    "4h",
    "6h",
    "8h",
    "12h",
    "1d"
    # "1w",
    # "1M"   # yahoo is always returning daily data anyway, so no point in supporting these
  )
  max_chunk_size <- if (grepl("m|h", interval)) 20000 else 1000

  validate_inputs(
    symbol,
    interval,
    chunk_size,
    max_chunk_size,
    start_date,
    end_date,
    valid_intervals
  )

  # Map interval
  seq_interval <- map_interval(interval)

  # Decide source
  tiingo <- grepl("m|h", interval)
  source <- if (tiingo) "tiingo.iex" else "stock.prices"

  # Track errors
  error_records <- tibble::tibble()

  if (tiingo) {
    # Check Tiingo API key
    api <- set_tiingo_api_key()
    tidyquant::tiingo_api_key(api)

    # Handle unsupported symbols consistently with Yahoo
    bad_syms <- tryCatch(
      {
        check_tiingo_symbols(symbol)
        character(0) # no bad symbols
      },
      error = function(e) {
        msg <- conditionMessage(e)
        trimws(unlist(strsplit(msg, ":"))[2])
      }
    )

    if (length(bad_syms) > 0) {
      bad_queries <- tibble::tibble(
        symbol = bad_syms,
        start_date = start_date,
        end_date = end_date
      )
      error_records <- dplyr::bind_rows(error_records, bad_queries)
      # remove bad symbols from processing
      symbol <- setdiff(symbol, bad_syms)
    }

    message("Remember: Tiingo timeseries are not adjusted for splits/dividends")
  }

  # Split date range into chunks
  queries <- make_query_chunks(
    seq_interval,
    chunk_size,
    start_date,
    end_date,
    symbol
  )

  # Run queries
  results_list <- purrr::map(1:nrow(queries), function(i) {
    tryCatch(
      {
        temp <- tidyquant::tq_get(
          x = queries$symbol[i],
          get = source,
          from = queries$start_date[i],
          to = queries$end_date[i],
          resample_frequency = gsub(" ", "", seq_interval)
        )
        if (is.null(temp) || nrow(temp) == 0) {
          # warning("No data for ", queries$symbol[i], " in ", queries$start_date[i], " - ", queries$end_date[i])
          return(list(data = NULL, error = queries[i, ]))
        }
        list(data = temp, error = NULL)
      },
      error = function(e) {
        message("Error for ", queries$symbol[i], ": ", e$message)
        list(data = NULL, error = queries[i, ])
      }
    )
  })

  # Combine results & errors
  all_data <- purrr::map_dfr(results_list, "data") %>%
    dplyr::distinct()

  if (nrow(all_data) > 0) {
    all_data <- all_data %>%
      dplyr::rename(open_time = "date") # ensure consistent column name
  }

  all_errors <- purrr::map_dfr(results_list, "error")
  all_errors <- dplyr::bind_rows(error_records, all_errors)

  # Adjust for splits if Tiingo
  if (adjust_splits && tiingo && nrow(all_data) > 0) {
    all_data <- adjust_for_splits(all_data, symbol)
  }

  # Final formatting
  results <- list(data = NULL, errors = NULL)

  if (!is.null(all_data) && nrow(all_data) > 0) {
    results$data <- all_data %>%
      # dplyr::mutate(symbol = symbol) %>% # ensure column exists
      # dplyr::rename(open_time = "date") %>%
      dplyr::relocate(symbol, .after = "volume")
  }

  if (!is.null(all_errors) && nrow(all_errors) > 0) {
    results$errors <- all_errors
  }

  return(results)
}

#' load_yahoo_dividends ############
#'
#' Load dividends from Yahoo Finance using tidyquant.
#'
#' @param symbols Character vector. Stock ticker(s) to retrieve dividends for.
#' @param start_date Character. Start date in "YYYY-MM-DD".
#' @param end_date Character. End date in "YYYY-MM-DD". Default is today's date.
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{Tibble with dividends data.}
#'     \item{errors}{Tibble with symbols that could not be retrieved, including error message.}
#'   }
#' @import dplyr tidyquant purrr tibble
#' @export
#'
#' @examples
#' dividends <- load_yahoo_dividends(
#'   symbols = c("AAPL", "MSFT"),
#'   start_date = "2020-01-01",
#'   end_date   = "2022-01-01"
#' )
load_yahoo_dividends <- function(
  symbols,
  start_date,
  end_date = as.character(Sys.Date())
) {
  # Validate inputs
  if (!is.character(symbols)) {
    stop("symbols must be a character vector")
  }
  if (!is_valid_date(start_date)) {
    stop("start_date must be YYYY-MM-DD")
  }
  if (!is_valid_date(end_date)) {
    stop("end_date must be YYYY-MM-DD")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("start_date must be before end_date")
  }

  # Run queries for each symbol
  results <- purrr::map(symbols, function(s) {
    # tryCatch(
    #   {
    temp <- tidyquant::tq_get(
      x = s,
      get = "dividends",
      from = start_date,
      to = end_date
    )

    if (any(is.null(temp), is.na(temp), nrow(temp) == 0)) {
      # warning("No data for ", s, " in ", start_date, " - ", end_date)
      return(list(
        data = NULL,
        error = tibble(symbol = s, error_message = "No data returned")
      ))
    }

    list(data = temp, error = NULL)
    #   },
    #   error = function(e) {
    #     list(data = NULL,
    #          error = tibble(symbol = s, error_message = e$message))
    #   }
    # )
  })

  # Combine results
  all_data <- purrr::map_dfr(results, "data")
  all_errors <- purrr::map_dfr(results, "error")

  # Preserve input symbol order in data
  if (!is.null(all_data) && nrow(all_data) > 0) {
    all_data <- all_data %>%
      tibble::as_tibble() %>%
      dplyr::mutate(symbol = factor(symbol, levels = symbols)) %>%
      dplyr::arrange(symbol, date)
  }

  # Return standardized list
  list(
    data = if (nrow(all_data) > 0) all_data else NULL,
    errors = if (nrow(all_errors) > 0) all_errors else NULL
  )
}


# Helper Functions ##########

#' Validate inputs ##########
#'
#' Check types, formats and ranges passed to other functions.
#' @param symbol Character vector. Stock ticker(s).
#' @param interval Character. One of '1m','3m','5m','15m','30m',
#'  '1h','2h','4h','6h','8h','12h','1d'.
#' @param chunk_size Integer. Number of observations per query (max 1000).
#' @param start_date Character in "YYYY-MM-DD".
#' @param end_date Character in "YYYY-MM-DD".
#' @param valid_intervals Character vector. Valid intervals for the function.
#' @return NULL. Stops with error if invalid.
#' @noRd
#' @import dplyr
#'
validate_inputs <- function(
  symbol,
  interval,
  chunk_size,
  max_chunk_size,
  start_date,
  end_date,
  valid_intervals
) {
  if (!is.character(symbol)) {
    stop("symbol must be a character vector")
  }
  if (!is_valid_date(start_date)) {
    stop("start_date must be YYYY-MM-DD")
  }
  if (!is_valid_date(end_date)) {
    stop("end_date must be YYYY-MM-DD")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("start_date must be before end_date")
  }
  if (as.Date(start_date) > Sys.Date()) {
    stop("start_date cannot be in the future")
  }
  if (
    !is.numeric(chunk_size) ||
      chunk_size != as.integer(chunk_size) ||
      is.na(chunk_size) ||
      chunk_size <= 0 ||
      chunk_size < 10
  ) {
    stop("chunk_size must be a positive integer at least 10")
  }
  if (chunk_size > max_chunk_size) {
    stop(paste0("chunk_size cannot exceed ", max_chunk_size, " (API limits)"))
  }
  if (!(interval %in% valid_intervals)) {
    stop(
      "Invalid interval. Must be one of: ",
      paste(valid_intervals, collapse = ", ")
    )
  }
}

#' map_interval ##########
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
    interval == "1m" ~ "1 min",
    interval == "3m" ~ "3 min",
    interval == "5m" ~ "5 min",
    interval == "15m" ~ "15 min",
    interval == "30m" ~ "30 min",
    interval == "1h" ~ "1 hour",
    interval == "2h" ~ "2 hour",
    interval == "4h" ~ "4 hour",
    interval == "6h" ~ "6 hour",
    interval == "8h" ~ "8 hour",
    interval == "12h" ~ "12 hour",
    interval == "1d" ~ "1 day",
    interval == "3d" ~ "3 day",
    interval == "1w" ~ "1 week",
    interval == "1M" ~ "1 month",
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
    stop(
      "These symbols are not supported on Tiingo: ",
      paste(unsupported, collapse = ", ")
    )
  }
}

#' make_query_chunks ##########
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
make_query_chunks <- function(
  seq_interval,
  chunk_size,
  start_date,
  end_date,
  symbol
) {
  number <- as.numeric(stringr::str_extract(seq_interval, "[0-9]+"))
  period <- stringr::str_extract(seq_interval, "[a-z]+")
  by <- paste(chunk_size * number, period)

  # Step is chunk_size * number - 1 to avoid overlapping periods
  step <- dplyr::case_when(
    period == "min" ~ lubridate::minutes(chunk_size * number - 1),
    period == "hour" ~ lubridate::hours(chunk_size * number - 1),
    period == "day" ~ lubridate::days(chunk_size * number - 1)
  ) # other periods return NA

  start_seq <- base::seq(
    as.POSIXct(start_date, tz = "UTC"),
    as.POSIXct(end_date, tz = "UTC"),
    by = by
  )

  end_seq <- start_seq + step # as other periods return NA end_seq will be NA, later replaced by end_date
  end_seq[length(end_seq)] <- as.POSIXct(end_date, tz = "UTC")

  dplyr::tibble(
    symbol = symbol,
    start_date = lubridate::format_ISO8601(start_seq, precision = "ymdhm"),
    end_date = lubridate::format_ISO8601(end_seq, precision = "ymdhm")
  )
}

#' adjust_for_splits ##########
#'
#' Adjust OHLCV prices for stock splits using splits data.
#'
#' This function adjusts historical price data for stock splits by applying
#' cumulative adjustment factors. Prices before splits are adjusted downward,
#' while volume is adjusted upward to maintain economic equivalence.
#'
#' @param data Tibble with OHLCV timeseries containing columns:
#'   - symbol: stock ticker
#'   - open_time: timestamp (POSIXct or Date)
#'   - open, high, low, close: price columns
#'   - volume: volume column
#' @param symbols Character vector of stock ticker(s) to adjust
#' @param splits_data Optional tibble with splits data. If NULL, fetches from tidyquant.
#'   Must contain columns: symbol, date, value (split ratio)
#' @return Tibble with split-adjusted OHLCV prices
#' @export
adjust_for_splits <- function(data, symbols, splits_data = NULL) {
  # Input validation
  if (nrow(data) == 0) {
    return(data)
  }

  required_cols <- c(
    "symbol",
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "volume"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get or use splits data
  if (is.null(splits_data)) {
    # splits_data <- tryCatch({
    #   result <- tidyquant::tq_get(symbols,
    #                             get = "splits",
    #                             from = "1900-01-01",
    #                             to = Sys.Date())
    #   # Handle the case where tq_get returns NA instead of throwing error
    #   if (all(is.na(result)) || is.null(result)) {
    #     NULL
    #   } else {
    #     result
    #   }
    # }, error = function(e) {
    #   warning("Failed to fetch splits data: ", e$message)
    #   NULL
    # })

    result <- tidyquant::tq_get(
      symbols,
      get = "splits",
      from = "1900-01-01",
      to = Sys.Date()
    )
    # Handle the case where tq_get returns NA instead of throwing error
    if (all(is.na(result)) || is.null(result)) {
      result <- NULL
    }
    splits_data <- result
  }

  # If no splits, return original data
  if (is.null(splits_data) || nrow(splits_data) == 0) {
    message("No splits found for symbols: ", paste(symbols, collapse = ", "))
    return(data)
  }

  # Validate splits data
  splits_required_cols <- c("symbol", "date", "value")
  missing_splits_cols <- setdiff(splits_required_cols, names(splits_data))
  if (length(missing_splits_cols) > 0) {
    stop(
      "Splits data missing required columns: ",
      paste(missing_splits_cols, collapse = ", ")
    )
  }

  # Calculate adjustment factors for each symbol
  adjustment_factors <- calculate_adjustment_factors(splits_data)

  # Apply adjustments
  # Apply adjustments
  result <- data %>%
    mutate(date = as.Date(open_time)) %>%
    left_join(adjustment_factors, by = c("symbol", "date")) %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      # For each date, find the adjustment factor that applies
      # This should be the factor from the next split date (or 1.0 if after all splits)
      adj_factor = determine_adjustment_factor(
        date,
        adjustment_factors[adjustment_factors$symbol == first(symbol), ]
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Apply adjustments
      open = open * adj_factor,
      high = high * adj_factor,
      low = low * adj_factor,
      close = close * adj_factor,
      volume = volume / adj_factor
    ) %>%
    select(-date, -adj_factor)

  return(result)
}

#' determine_adjustment_factor ##########
#'
#' Determine the adjustment factor for a given date
#'
#' For each date, finds the cumulative adjustment factor from all future splits
#'
#' @param target_dates Vector of dates to get factors for
#' @param splits_for_symbol Tibble with date and adj_factor columns for one symbol
#' @return Vector of adjustment factors
#' @noRd
determine_adjustment_factor <- function(target_dates, splits_for_symbol) {
  sapply(target_dates, function(target_date) {
    # Find all splits that occur AFTER this target date
    future_splits <- splits_for_symbol[splits_for_symbol$date >= target_date, ]

    if (nrow(future_splits) == 0) {
      # No future splits, no adjustment needed
      return(1.0)
    } else {
      # Find the adjustment factor from the earliest future split
      # (which already contains the cumulative effect of all future splits)
      earliest_future <- future_splits[which.min(future_splits$date), ]
      return(earliest_future$adj_factor)
    }
  })
}

#' calculate_adjustment_factors ##########
#'
#' Calculate cumulative adjustment factors for splits
#'
#' For each symbol and date, calculates the cumulative adjustment factor
#' that should be applied to prices before that date.
#'
#' @param splits_data Tibble with columns: symbol, date, value
#' @return Tibble with columns: symbol, date, adj_factor
#' @noRd
calculate_adjustment_factors <- function(splits_data) {
  splits_data %>%
    arrange(symbol, desc(date)) %>% # Sort by symbol, then by date DESCENDING (newest first)
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      # Calculate cumulative adjustment from newest split backwards
      # Each adj_factor represents the total adjustment needed for dates BEFORE this split
      adj_factor = cumprod(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(symbol, date, adj_factor)
}
