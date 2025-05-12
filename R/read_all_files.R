#' Read Multiple Files from a Folder
#'
#' Reads all files from a specified folder that match a given file extension and (optionally) contain a specific substring in the filename.
#'
#' @param folder Character. Path to the folder containing the files.
#' @param extension Character. File extension to match (e.g., `"csv"`, `"txt"`). Default is `".csv"`.
#' @param contains Optional character string. If provided, only files whose names contain this string will be read.
#'
#' @return A named list of data frames. Each element corresponds to a file, with names based on filenames (excluding extensions).
#' @examples
#' \dontrun{
#' # Read all CSV files in the "data" folder that contain "survey" in their names
#' survey_data <- read_files_from_folder("data", extension = "csv", contains = "survey")
#'
#' # Read all TXT files regardless of name
#' txt_data <- read_files_from_folder("logs", extension = "txt")
#' }
#' @export
read_all_files <- function(folder, extension = ".csv", contains = NULL) {
  if (!startsWith(extension, ".")) {
    extension <- paste0(".", extension)
  }

  files <- list.files(path = folder, pattern = paste0("\\", extension, "$"), full.names = TRUE)

  if (!is.null(contains)) {
    files <- files[grepl(contains, basename(files))]
  }

  data_list <- lapply(files, read.csv)
  names(data_list) <- tools::file_path_sans_ext(basename(files))

  return(data_list)
}
