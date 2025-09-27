#' Set or Retrieve the Tiingo API Key
#'
#' @name set_tiingo_api_key
#' @param tiingo_key Optional. A character string with your Tiingo API Key.
#'   If not provided, the function will first look in `options("tiingo_key")`,
#'   then in the system environment variable `TIINGO_KEY`,
#'   and finally attempt to retrieve it from the keyring.
#' @param overwrite Logical, default = FALSE. Whether to overwrite the
#'   key already set in options().
#' @importFrom keyring key_get
#' @return Invisibly returns the Tiingo API key once set.
#' @details
#' The Tiingo API key must be set before calling functions that require it.
#'
#' Order of precedence for retrieving the key:
#' 1. Direct input via `tiingo_key`
#' 2. Already set in `options("tiingo_key")`
#' 3. Environment variable `Sys.getenv("TIINGO_KEY")`
#' 4. Securely stored in the system keyring under the name `"tiingo_key"`
#'
#' @examples
#' \dontrun{
#' # Direct input
#' set_tiingo_api_key("YOUR_API_KEY")
#'
#' # Or if previously stored with:
#' # keyring::key_set("tiingo_key")
#' set_tiingo_api_key()
#' }
#'
#' @export
set_tiingo_api_key <- function(tiingo_key = NULL, overwrite = FALSE) {
  # 1. Already set in options
  if (!overwrite && !is.null(getOption("tiingo_key"))) {
    return(invisible(getOption("tiingo_key")))
  }

  # 2. Direct input
  if (!is.null(tiingo_key)) {
    if (!is.character(tiingo_key) || length(tiingo_key) != 1) {
      stop("tiingo_key must be a single character string", call. = FALSE)
    }
    options(tiingo_key = tiingo_key)
    return(invisible(tiingo_key))
  }

  # 3. Environment variable
  env_key <- Sys.getenv("TIINGO_KEY", unset = NA)
  if (!is.na(env_key) && nzchar(env_key)) {
    options(tiingo_key = env_key)
    return(invisible(env_key))
  }

  # 4. Keyring (if available)
  if (requireNamespace("keyring", quietly = TRUE)) {
    kr_key <- tryCatch(
      keyring::key_get("tiingo_key"),
      error = function(e) NULL
    )
    if (!is.null(kr_key)) {
      options(tiingo_key = kr_key)
      return(invisible(kr_key))
    }
  }

  # If all fail
  stop(
    "No valid Tiingo API key found. Please set one using either:\n",
    " - set_tiingo_api_key(\"YOUR_KEY\")\n",
    " - Sys.setenv(TIINGO_KEY = \"YOUR_KEY\")\n",
    " - keyring::key_set(\"tiingo_key\")",
    call. = FALSE
  )
}

#' Set or Retrieve the Binance API Key and Secret
#'
#' @name set_binance_api_key
#' @param bin_key Optional. A character string with your Binance API Key.
#'   If not provided, the function will first look in `options("bin_key")`,
#'   then in the system environment variable `BIN_KEY`,
#'   and finally attempt to retrieve it from the keyring.
#' @param bin_secret Optional. A character string with your Binance API Secret.
#'   If not provided, the function will first look in `options("bin_secret")`,
#'   then in the system environment variable `BIN_SECRET`,
#'   and finally attempt to retrieve it from the keyring.
#' @param overwrite Logical, default = FALSE. Whether to overwrite keys already set in options().
#' @importFrom keyring key_get
#' @return Invisibly returns a list with `key` and `secret`.
#' @details
#' The Binance API key and secret must be set before calling functions that require them.
#'
#' Order of precedence for retrieving keys:
#' 1. Direct input via `bin_key` / `bin_secret`
#' 2. Already set in `options("bin_key")` / `options("bin_secret")`
#' 3. Environment variables `BIN_KEY` / `BIN_SECRET`
#' 4. Securely stored in the system keyring under `"binance_key"` and `"binance_secret"`
#'
#' @examples
#' \dontrun{
#' # Direct input
#' set_binance_api_key("YOUR_API_KEY", "YOUR_API_SECRET")
#'
#' # Or if previously stored securely with:
#' # keyring::key_set("binance_key")
#' # keyring::key_set("binance_secret")
#' set_binance_api_key()
#' }
#' @export
set_binance_api_key <- function(
  bin_key = NULL,
  bin_secret = NULL,
  overwrite = FALSE
) {
  # 1. Already set in options
  if (
    !overwrite &&
      !is.null(getOption("bin_key")) &&
      !is.null(getOption("bin_secret"))
  ) {
    return(invisible(list(
      key = getOption("bin_key"),
      secret = getOption("bin_secret")
    )))
  }

  # 2. Direct input
  if (!is.null(bin_key) && !is.null(bin_secret)) {
    if (!is.character(bin_key) || length(bin_key) != 1) {
      stop("bin_key must be a single character string", call. = FALSE)
    }
    if (!is.character(bin_secret) || length(bin_secret) != 1) {
      stop("bin_secret must be a single character string", call. = FALSE)
    }
    options(bin_key = bin_key, bin_secret = bin_secret)
    return(invisible(list(key = bin_key, secret = bin_secret)))
  }

  # 3. Environment variables
  env_key <- Sys.getenv("BIN_KEY", unset = NA)
  env_secret <- Sys.getenv("BIN_SECRET", unset = NA)

  if (
    !is.na(env_key) &&
      nzchar(env_key) &&
      !is.na(env_secret) &&
      nzchar(env_secret)
  ) {
    options(bin_key = env_key, bin_secret = env_secret)
    return(invisible(list(key = env_key, secret = env_secret)))
  }

  # 4. Keyring (if available)
  if (requireNamespace("keyring", quietly = TRUE)) {
    kr_key <- tryCatch(keyring::key_get("binance_key"), error = function(e) {
      NULL
    })
    kr_secret <- tryCatch(
      keyring::key_get("binance_secret"),
      error = function(e) NULL
    )
    if (!is.null(kr_key) && !is.null(kr_secret)) {
      options(bin_key = kr_key, bin_secret = kr_secret)
      return(invisible(list(key = kr_key, secret = kr_secret)))
    }
  }

  # If all fail
  stop(
    "No valid Binance API key and secret found. Please set them using either:\n",
    " - set_binance_api_key(\"YOUR_KEY\", \"YOUR_SECRET\")\n",
    " - Sys.setenv(BIN_KEY = \"YOUR_KEY\", BIN_SECRET = \"YOUR_SECRET\")\n",
    " - keyring::key_set(\"binance_key\"); keyring::key_set(\"binance_secret\")",
    call. = FALSE
  )
}

#### OLD FUNCTION USING KEYRING PACKAGE TO MANAGE THE TIINGO KEY SECURELY

# #' Set the tiingo API Key
# #'
# #' @name tiingo_api
# #' @param tiingo_key A character string with your Tiingo API Key.
# #' @return Invisibly returns API key once set. Use print method to view.
# #' @details
# #' The Tiingo API key must be set prior to load stock timeseries that needs it.
# #' You can obtain an API key at your Tiingo account (https://api.tiingo.com/).
# #' This function is using the keyring package to manage the API key securely.
# #' @examples
# #' \dontrun{
# #' tiingo_api("YOUR_API_KEY")
# #' stock_test = load_stock_timeseries(symbol = "AAPL", start_date="2021-01-01", end_date="2021-07-01")
# #' }
# #'
# #' @export
# #'
# tiingo_api <- function() {

#     # check if keyring package is installed
#     if (!requireNamespace("keyring", quietly = TRUE)) {
#         stop("The 'keyring' package is required but not installed. Please install it using install.packages('keyring') and try again.", call. = FALSE)
#     }

#     # check if it was loaded previously
#     if (is.null(getOption('tiingo_key'))) {

#         # Retrieve the Tiingo API key from the keyring
#         tiingo_key <- tryCatch(
#             # Attempt to get the key
#             keyring::key_get("tiingo_key"),
#             # If an error occurs (e.g., key not found), handle it here
#             error = function(e) {
#                 message("Error retrieving Tiingo API key: ", e$message)
#                 return(NULL)
#             }
#         )
#         # Set the tiingo API Key, if it was informed
#         if (!is.null(tiingo_key)) {
#             # Set the API key in the global environment
#             options(tiingo_key = tiingo_key)
#             # remove the key from the environment
#             rm(tiingo_key)

#         } else if (is.null(getOption('tiingo_key'))) {
#             # Stop the process and show an error message as a pop-up window.
#             stop("You must save a valid Tiingo API key to the global environment. run keyring::key_set('tiingo_key') to add the key to add one. You can obtain a Tiingo key at https://api.tiingo.com/", call. = FALSE)
#         }

#     }
#     # Return the API key
#     invisible(getOption('tiingo_key'))
# }

#' Set the binance API Key and secret
# #'
# #' @name binance_api
# #'
# #' @param bin_key A character string with your Binance API Key.
# #' @param bin_secret A character string with your Binance API Secret.
# #'
# #' @return Invisibly returns API key and Secret once set. Use print method to view.
# #'
# #' @details
# #' The Binance API key and Secret must be set prior to load crypto timeseries that needs it.
# #'  You can obtain an API key at your binance account (https://www.binance.com/en/support/faq/how-to-create-an-api-key-for-a-lead-trading-portfolio-2bec848b904b422197ce121d0925f20b).
# #'
# #' @examples
# #' \dontrun{
# #' binance_api("YOUR_API_KEY")
# #' crypto_test = load_crypto_timeseries(pair = "ETHUSDT", interval="1m", start_date="2021-01-01", end_date="2021-07-01")
# #' }
# #'
# #' @export
# binance_api <- function(bin_key, bin_secret) {

#     # Set the binance API Key and secret, if it was informed
#     if (!missing(bin_key) | !missing(bin_secret)) {
#         # Set the API key and secret in the global environment
#         options(bin_key = bin_key)
#         options(bin_secret = bin_secret)

#     } else if (is.null(getOption('bin_key')) | is.null(getOption('bin_secret'))) {
#         # Stop the process and show an error message as a pop-up window.
#         stop("You must save a valid Binance API key and Secret to the global environment (e.g: run < bin_key <<- 'your_key_here' > and < bin_secret <<- 'your_secret_here' > somewhere before start running this function). You can obtain a Binance key and secret at https://www.binance.com/en/support/faq/how-to-create-an-api-key-for-a-lead-trading-portfolio-2bec848b904b422197ce121d0925f20b", call. = FALSE)

#     }

#     # Return the API key and Secret
#     invisible(list(key = getOption('bin_key')
#                 , secret = getOption('bin_secret')))
#     # remove the key and secret from the environment
#     rm(bin_key, bin_secret)
# }
