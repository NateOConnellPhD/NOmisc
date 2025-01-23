#' Reads in all sheets from an Excel File and stores them as a list
#'
#' When supplied an excel file, this reads in all the sheets and stores each as a
#' component to a list.
#'
#'
#' @param filename the filename of the excel file to read in
#' @param tibble logical; if TRUE, then reads in as TIBBLE.
#' @return a list of dataframes or TIBBLEs corresponding to each Excel Sheet
#' @export

read.excel.allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
