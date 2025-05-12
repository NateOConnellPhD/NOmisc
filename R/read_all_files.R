#' Read Multiple Files from a Folder Quickly (as Data Frames)
#'
#' Efficiently reads all files from a specified folder that match a given file extension,
#' and optionally contain a specific substring in the filename. Uses `fread` from the
#' `data.table` package for speed, then converts each result to a standard `data.frame`.
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
#' }
#' @export
read_all_files <- function(folder, extension = ".csv", contains = NULL) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  # Ensure the extension starts with a dot
  if (!startsWith(extension, ".")) {
    extension <- paste0(".", extension)
  }

  # List matching files
  files <- list.files(path = folder, pattern = paste0("\\", extension, "$"), full.names = TRUE)

  # Filter by optional 'contains' string
  if (!is.null(contains)) {
    files <- files[grepl(contains, basename(files))]
  }

  # Read files using fread and convert to data.frame
  data_list <- lapply(files, function(f) as.data.frame(data.table::fread(f)))

  # Name the list elements by the file names without extensions
  names(data_list) <- tools::file_path_sans_ext(basename(files))

  return(data_list)
}

