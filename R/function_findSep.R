#' Line separator
#' 
#' Find line separator character in a data file
#' 
#' @param file_path string, full path and name of data file
#' @return the line separator (character).
#' @examples
#' file_path=file.path("Scripts", "extdata", "file_example1.txt");findSep(file_path)

findSep <- function(file_path) {
  # Read file with no sep
  df <- read.table(file_path, fill = TRUE)
  # Find index of row in the middle of df (in case there is a big header)
  n_middle <- round(NROW(df)/2)
  # Read file
  whichsep <- NULL
  for(s in c(";", ",", "\t")) {
    # skipping "n_middle" rows
    if(is.null(whichsep)) {
      df <- read.delim(file_path, sep = s, skip = n_middle, nrow = n_middle+1)
      if(NCOL(df) > 1) {
        whichsep <- s
      }
    }
  }
  return(whichsep)
}
