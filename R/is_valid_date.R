#' Validate Date Strings
#'
#' Checks whether one or more strings are in the format `"YYYY-MM-DD"` and represent real calendar dates.
#'
#' @param date_str Character vector. One or more strings representing dates.
#'
#' @return Logical vector. Each element is `TRUE` if the corresponding input string is a valid date in `"YYYY-MM-DD"` format, `FALSE` otherwise. `NA` values in the input return `FALSE`.
#'
#' @details
#' The function performs two checks for each element in `date_str`:
#' 1. The string matches the pattern `YYYY-MM-DD`.
#' 2. The string can be successfully parsed into a valid date using `lubridate::ymd()`.
#' 
#' Invalid dates (e.g., `"2023-02-30"`) or incorrectly formatted strings (e.g., `"15-10-2023"`) return `FALSE`.  
#' This function is vectorized, so it can process multiple date strings at once.
#'
#' @examples
#' is_valid_date("2023-10-15")                 # TRUE
#' is_valid_date("2023-02-30")                 # FALSE (invalid date)
#' is_valid_date("15-10-2023")                 # FALSE (wrong format)
#' is_valid_date(c("2023-01-01", "2023-02-30")) # c(TRUE, FALSE)
#' is_valid_date(NA)                            # FALSE
#'
#' @export
is_valid_date <- function(date_str) {
  # Ensure input is character
  if (all(!is.character(date_str), !is.na(date_str))) {
    stop("`date_str` must be a character vector", call. = FALSE)
  }

  # Handle NA inputs
  date_str[is.na(date_str)] <- ""

  # Must match YYYY-MM-DD
  valid_format <- stringr::str_detect(date_str, "^\\d{4}-\\d{2}-\\d{2}$")
  
  # Parse only those that match format
  parsed <- suppressWarnings(lubridate::ymd(date_str, quiet = TRUE))

  valid_format & !is.na(parsed)
}
