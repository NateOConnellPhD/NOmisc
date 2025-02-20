#' Logical Check if variable is binary or not
#'
#' This function takes a vector input 'y' and checks if it is binary
#'
#'
#' @param y A numeric, integer, or character vector
#' @return Logical TRUE or FALSE. If TRUE, the supplied vector is binary (contains only 2 levels when assessed as a factor)
#' @export


is_binary = function(y){
  if(length(unique(y))>2){
    F
  } else T
}

