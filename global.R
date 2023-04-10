#' Function to import files by automatically identify delimiters
#'
#' @param file_path the name of the file which the data are to be read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param row_names a vector of row names.
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @importFrom utils unzip read.table read.csv
#'
#' @return data.frame
#' @export
#'
read_files <- function(file_path = NULL, header = TRUE, row_names = 1, stringsAsFactors = F, skip = 0) {
  # read the second line to identify delimiters
  if (grepl(x = file_path, pattern = ".zip")) {
    Lines_2 <- readLines(unzip(file_path), n = 2)[2]
  } else {
    Lines_2 <- readLines(file_path, n = 2)[2]
  }
  # # identify delimiters
  if (grepl(x = Lines_2, pattern = "\t") == T) {
    delimiters <- "\t"
  } else if (grepl(x = Lines_2, pattern = ",") == T) {
    delimiters <- ","
  } else if (grepl(x = Lines_2, pattern = " ") == T) {
    delimiters <- " "
  } else if (grepl(x = Lines_2, pattern = ";") == T) {
    delimiters <- ";"
  } else if (grepl(x = Lines_2, pattern = "|") == T) {
    delimiters <- "|"
  } else if (grepl(x = Lines_2, pattern = ":") == T) {
    delimiters <- ":"
  }
  # # use read.csv function to import files
  if (grepl(x = file_path, pattern = ".zip")) {
    data <- read.csv(file = unzip(file_path), header = header, row.names = row_names, sep = delimiters, check.names = F, stringsAsFactors = stringsAsFactors, skip = skip)
  } else {
    data <- read.csv(file = file_path, header = header, row.names = row_names, sep = delimiters, check.names = F, stringsAsFactors = stringsAsFactors, skip = skip)
  }
  return(data)
}

#' Load in data sets and creat dataframe of Reads Number
#'
#' @param path the name of the file which the data are to be read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param row_names a logical value indicating whether the file contains the names of the variables as its first column.
#'
#' @return data.frame
#' @export
#'

upload.data <- function(path, header, row_names = TRUE, skip = 0) {
  if (isTRUE(row_names)) {
    df <- read_files(file_path = path, header = header, row_names = 1, skip = skip)
  } else {
    df <- read_files(file_path = path, header = header, row_names = NULL, skip = skip)
  }
  return(df)
}