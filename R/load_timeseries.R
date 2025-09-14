#' load the timeseries from yahoo finance (daily) or tiingo (intraday)
#' 
#' @param symbol Stock symbol or vector of symbols
#' @param interval Time interval between two data points in the time series.
#' One of '1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d'
#' @param limit Number of observations to retrieve per request (max 1000)
#' @param start_date Start date of the time series (YYYY-MM-DD)
#' @param end_date End date of the time series (YYYY-MM-DD)
#' @param adjust_splits Logical. If TRUE, adjust the prices for stock splits (only for tiingo data)
#' @return A tibble with the time series data
#' @import dplyr
#' @import tidyquant
#' @import riingo
#' @import lubridate
#' @import stringr
#' @import zoo
#' @import DBI
#' @examples 
#' # Load hourly time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_hour = load_stock_timeseries(symbol = "AAPL", interval="1h", start_date="2021-10-01", end_date="2022-02-01")
#' 
#' # Load daily time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_day = load_stock_timeseries(symbol = "AAPL", interval="1d", start_date="2021-10-01", end_date="2022-02-01")
#' 
#' # Load daily time series for AAPL and MSFT from 2021-10-01 to 2022-02-01
#' stock_test_multi = load_stock_timeseries(symbol = c("AAPL", "MSFT"), interval="1d", start_date="2021-10-01", end_date="2022-02-01")
#' 
#' # Load weekly time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_week = load_stock_timeseries(symbol = "AAPL", interval="1w", start_date="2021-10-01", end_date="2022-02-01")
#' 
#' # Load monthly time series for AAPL from 2021-10-01 to 2022-02-01
#' stock_test_month = load_stock_timeseries(symbol = "AAPL", interval="1M", start_date="2021-10-01", end_date="2022-02-01")
#' 
load_stock_timeseries <- function(symbol, interval, limit=60*24*7, start_date, end_date, adjust_splits = TRUE) {
  
  #check intervals
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
    # interval == '3d' ~ '3 day',
    # interval == '1w' ~ '1 week',
    # interval == '1M' ~ '1 month',
    TRUE ~ "what?")
  
  if (seq_interval == "what?") {
    
    stop("Please, select an interval between c('1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d')")
    
  }

  # if interval ends in m or h, tiingo is set to be true, else, is false
  tiingo <- ifelse(grepl("m", interval) | grepl("h", interval), TRUE, FALSE)

  if(tiingo == TRUE) {
    
    #TIINGO pricelists
    api <- tiingo_api()
    tidyquant::tiingo_api_key(api)

    source <- "tiingo.iex"

    for (s in symbol) {
      tiingo_support <- riingo::is_supported_ticker(s)
      if (tiingo_support == FALSE) {
        message(paste0("Symbol ", s, " is not supported by Tiingo"))
        # stop the function and return NULL
        return(NULL)
      } # end of if
    } # end of for
    
    message("Remember Tiingo timeseries are not adjusted for splits and dividends")

  } else {

    source <- "stock.prices"

  }
  #check date-time formats
  #Check if is a valid YYYY-MM-DD
  #if is date or posixct, change to character

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
  sequency <- dplyr::tibble(symbol = symbol
                            , start_date=format_ISO8601(start_date_seq, precision="ymdhm")
                            , end_date=format_ISO8601(end_date_seq,precision="ymdhm"))
  
  #Run a looping merging the results
  results = NULL

  for (i in 1:dim(sequency)[1]) {
    
    # Try to get the data
    tryCatch({
      temp <- tidyquant::tq_get(c(symbol),
                                get = source,
                                from   = sequency$start_date[i],
                                to     = sequency$end_date[i],
                                resample_frequency = sub(" ", "", seq_interval)) #Excluding the space character
      
    }, error = function(e) {
      
      message(paste0("Error for ", symbol, ": "), e)
      temp <<- NULL

    })
    
    if (is.null(dim(temp))) {
      
      message("No data found for ", symbol)
      
      if (exists("loading_errors")) {
        
        loading_errors <<- dplyr::bind_rows(loading_errors, sequency[i,])
        
      } else {
        
        loading_errors <<- sequency[i,]
        
      }

      next
      
    } else {
      
      results <- dplyr::bind_rows(results,temp)
      
      if(adjust_splits & tiingo){
  
      # load the splits data
        symbol_splits <- tidyquant::tq_get(x = symbol,
                    get = "splits",
                    from = "1900-01-01",
                    to = Sys.Date()
                    )
        if (is.null(symbol_splits)) {
          message("No splits found for ", symbol)
        } else {
  
          # Implementing adjustments for splits
          # This chain merges the price data into the div.data in order to get the prices to calculate the adjRatio
            symbol_splits_ratios <- symbol_splits %>% 
                dplyr::mutate(adjRatio = rev(cumprod(rev(value)))) %>% 
                dplyr::rename(day = date)
      
            # This chain merges the div.data back into the price series, propagates the adjRatio and calculates the Adjusted Close:
            timeserie_tiingo_date <- results %>% 
              dplyr::mutate(day = as.Date(date))
      
            results <- timeserie_tiingo_date %>% 
              dplyr::left_join(symbol_splits_ratios) %>% 
              # move adjRatio value 1 row back
              dplyr::mutate(adjRatio = dplyr::lead(adjRatio, n=1)) %>% 
              # replacing each NA with the most recent non-NA prior to it
              dplyr::mutate(adjRatio = zoo::na.locf(adjRatio, fromLast = TRUE, na.rm = FALSE)) %>% 
              # Fast fill missing values with 1 (after the last dividend payd)
              dplyr::mutate(adjRatio = na.fill(adjRatio, fill = 1.0)) %>% 
              dplyr::mutate(open = open * adjRatio
                    , high = high * adjRatio
                    , low = low * adjRatio
                    , close = close * adjRatio) %>% 
              dplyr::select(-day, -value, -adjRatio)
        
          } # end of if is.null(symbol_splits)
        
      } # end of if adjust_splits
      
    } # end of if is.na(temp)
    
  } # end of loop
  
  if (any(is.null(results), dim(results) == 0)) {
    
    results <- NULL
  
    } else {
    #format the data to export
      results <- results %>% 
      dplyr::rename(open_time = date) %>% 
      dplyr::relocate(symbol, .after = volume)
    
    return(results)
  }
  
  
}

#' Set the tiingo API Key
#' 
#' @name tiingo_api
#' 
#' @param tiingo_key A character string with your Tiingo API Key.
#' 
#' @return Invisibly returns API key once set. Use print method to view.
#' 
#' @details
#' The Tiingo
#' API key must be set [tiingo_api()] prior to load stock timeseries that needs it.
#' You can obtain an API key at your Tiingo account (https://api.tiingo.com/).
#' 
#' @examples
#' \dontrun{
#' tiingo_api("YOUR_API_KEY")
#' stock_test = load_stock_timeseries(symbol = "AAPL", start_date="2021-01-01", end_date="2021-07-01")
#' }
#' 
#' @export
tiingo_api <- function(tiingo_key) {
    
    # Set the tiingo API Key, if it was informed
    if (!missing(tiingo_key)) {

        options(tiingo_key = tiingo_key)

    }

    # Return the API key
    invisible(getOption('tiingo_key'))

}