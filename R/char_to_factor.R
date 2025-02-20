#' Converts all character variables to factor variables in a data frame
#'
#' When supplied a data frame with character variables, this function
#' converts all character variables within a data frame to factor variables.
#'
#'
#' @param df A data frame or tibble object.
#' @return the same dataframe provided with all character variables converted to factor variables
#' @export


char_to_factor <- function(df) {
  # Ensure the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # ISpecifiy character columns
  char_cols <- sapply(df, is.character)

  # convert character columns to factors
  df[char_cols] <- lapply(df[char_cols], factor)

  return(df)
}



