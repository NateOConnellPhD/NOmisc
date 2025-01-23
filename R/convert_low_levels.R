#' Convert Level with Low Frequencies to 'other'
#'
#' This function takes a vector input 'x' that should be of type factor or character.
#' It converts all levels with frequency < 'thresh' to an "Other" category (by default),
#' or to a name of the users choosing specified by 'new_level_name'
#'
#'
#' @param x factor or character vector
#' @param thresh threshold for which all levels with frequency less than will be categorized into 'Other' or 'new_level_name'
#' @param new_level_name optional parameter to define what the new all encompasing level name should be
#' @return A factor vector
#' @export

convert_low_levels <- function(x, thresh, new_level_name = "Other") {
  # Check if the input is a factor or character
  if (!is.factor(x) && !is.character(x)) {
    stop("The input vector must be a factor or character.")
  }

  # Convert character to factor if needed
  if (is.character(x)) {
    x <- as.factor(x)
  }

  # Calculate frequencies of each level
  level_counts <- table(x)

  # Identify levels to be converted to the new level name
  levels_to_convert <- names(level_counts[level_counts < thresh])

  # Update the factor levels
  x <- factor(x, levels = c(levels(x), new_level_name))
  x[x %in% levels_to_convert] <- new_level_name

  # Drop unused levels
  x <- droplevels(x)

  return(x)
}
