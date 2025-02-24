#' Remove columns from dataset with all NA or blank spaces
#'
#' This function's input is a dataframe. The function returns a cleaned dataframe
#' removing all columns with all NAs (no informative information)
#'
#'
#' @param df dataframe containing some missing columns
#' @return A dataframe without the missing columns
#' @export

remove_na_cols <- function(df) {
  df[, sapply(df, function(col) {
    if (all(is.na(col))) {
      FALSE  # Exclude columns where all values are NA
    } else if (is.character(col) && all(is.na(col) | col == "")) {
      FALSE  # Exclude character columns where all values are NA or empty strings
    } else {
      TRUE   # Keep the column
    }
  })]
}
