# get_financial_data ############
#' Download financial statement datasets from DoltHub
#'
#' Retrieves full historical financial datasets (Balance Sheets, Income Statements,
#' Cash Flow, etc.) from the DoltHub 'earnings' repository.
#'
#' @param files_to_get Character vector. Names of financial statement datasets to download.
#' @param timeout Integer. Seconds to allow for download before timing out.
#'   Default is 300.
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{A named list of tibbles, one for each financial statement type.}
#'     \item{errors}{A tibble containing filenames that failed to download and the error message.}
#'   }
#' @import dplyr readr purrr tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all financial tables
#' financials <- get_fundamentals_data()
#'
#' # Access the Income Statement table
#' income_stmt <- financials$data$income_statement
#' }
get_fundamentals_data <- function(
  files_to_get = c(
    "balance_sheet_equity",
    "balance_sheet_assets",
    "balance_sheet_liabilities",
    "cash_flow_statement",
    "earnings_calendar",
    "eps_estimate",
    "eps_history",
    "income_statement",
    "rank_score",
    "sales_estimate"
  ),
  timeout = 300
) {
  # Set temporary timeout for large file downloads
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout))

  # Internal helper for URL construction
  get_dolt_url <- function(file_name) {
    paste0(
      "https://www.dolthub.com/csv/post-no-preference/earnings/master/",
      file_name,
      "?include_bom=0"
    )
  }

  message(
    "Starting download of ",
    length(files_to_get),
    " financial datasets..."
  )

  # Iterate through files and download using purrr
  results <- purrr::map(files_to_get, function(f) {
    url <- get_dolt_url(f)

    tryCatch(
      {
        # read_csv for better performance and column type guessing
        df <- readr::read_csv(url, show_col_types = FALSE)
        # If you have data, return it. If not, return NULL and log error.
        if (nrow(df) == 0) {
          stop("File is empty.")
        }
        list(data = df, error = NULL)
      },
      error = function(e) {
        # Log warning but continue with other files
        warning("Failed to download '", f, "': ", e$message)

        list(
          data = NULL,
          error = tibble::tibble(
            file = f,
            error_message = as.character(e$message),
            timestamp = Sys.time()
          )
        )
      }
    )
  })

  # Assign names to the list based on file names
  names(results) <- files_to_get

  # Extract successful dataframes into a named list
  all_data <- purrr::map(results, "data") %>%
    purrr::compact()

  # Combine all error tibbles into one
  all_errors <- purrr::map_dfr(results, "error")

  # Standardized Return Object
  return(list(
    data = if (length(all_data) > 0) all_data else NULL,
    errors = if (nrow(all_errors) > 0) all_errors else NULL
  ))
}
