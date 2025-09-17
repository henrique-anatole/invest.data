#' Set the binance API Key and secret
#'
#' @name binance_api
#'
#' @param bin_key A character string with your Binance API Key.
#' @param bin_secret A character string with your Binance API Secret.
#'
#' @return Invisibly returns API key and Secret once set. Use print method to view.
#'
#' @details
#' The Binance API key and Secret must be set prior to load crypto timeseries that needs it.
#'  You can obtain an API key at your binance account (https://www.binance.com/en/support/faq/how-to-create-an-api-key-for-a-lead-trading-portfolio-2bec848b904b422197ce121d0925f20b).
#'
#' @examples
#' \dontrun{
#' binance_api("YOUR_API_KEY")
#' crypto_test = load_crypto_timeseries(pair = "ETHUSDT", interval="1m", start_date="2021-01-01", end_date="2021-07-01")
#' }
#'
#' @export
binance_api <- function(bin_key, bin_secret) {
    
    # Set the binance API Key and secret, if it was informed
    if (!missing(bin_key) | !missing(bin_secret)) {
        # Set the API key and secret in the global environment
        options(bin_key = bin_key)
        options(bin_secret = bin_secret)        

    } else if (is.null(getOption('bin_key')) | is.null(getOption('bin_secret'))) {
        # Stop the process and show an error message as a pop-up window.
        stop("You must save a valid Binance API key and Secret to the global environment (e.g: run < bin_key <<- 'your_key_here' > and < bin_secret <<- 'your_secret_here' > somewhere before start running this function). You can obtain a Binance key and secret at https://www.binance.com/en/support/faq/how-to-create-an-api-key-for-a-lead-trading-portfolio-2bec848b904b422197ce121d0925f20b", call. = FALSE)
        
    }

    # Return the API key and Secret
    invisible(list(key = getOption('bin_key')
                , secret = getOption('bin_secret')))
    # remove the key and secret from the environment
    rm(bin_key, bin_secret)
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
#' API key must be set prior to load stock timeseries that needs it.
#' You can obtain an API key at your Tiingo account (https://api.tiingo.com/).
#' 
#' @examples
#' \dontrun{
#' tiingo_api("YOUR_API_KEY")
#' stock_test = load_stock_timeseries(symbol = "AAPL", start_date="2021-01-01", end_date="2021-07-01")
#' }
#' 
#' @export
#' 
tiingo_api <- function(tiingo_key) {
    
    # Set the tiingo API Key, if it was informed
    if (!missing(tiingo_key)) {
        # Set the API key in the global environment
        options(tiingo_key = tiingo_key)
        # remove the key from the environment
        rm(tiingo_key)
        
    } else if (is.null(getOption('tiingo_key'))) {
        # Stop the process and show an error message as a pop-up window.
        stop("You must save a valid Tiingo API key to the global environment (e.g: run < tiingo_key <<- 'your_key_here' > somewhere before start running this function). You can obtain a Tiingo key at https://api.tiingo.com/", call. = FALSE)
        
    }

    # Return the API key
    invisible(getOption('tiingo_key'))

}