#' is_valid_date
#' Check if a date string is in a valid format (YYYY-MM-DD) and represents a real date.
#' @param date_str A string representing a date.
#' @return TRUE if the date string is valid, FALSE otherwise.
#' @examples
#' is_valid_date("2023-10-15")  # TRUE
#' is_valid_date("2023-02-30")  # FALSE (invalid date)
#' is_valid_date("15-10-2023")  # FALSE (wrong format)
is_valid_date <- function(x) {
  # Must match YYYY-MM-DD
  valid_format <- str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  
  # Parse only those that match format
  parsed <- suppressWarnings(ymd(x, quiet = TRUE))

  valid_format & !is.na(parsed)

}