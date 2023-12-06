#' Site name
#' 
#' Find site name based on file name according to the naming system "3 letters followed by 2 digits"
#' 
#' @param file_name_substrings character vector, containing substrings of file name
#' @return the site name (string).
#' @examples
#' file_name_substrings=c( "files/20131213","BIM13","00001");findSiteName(file_name_substrings)

findSiteName <- function(file_name_substrings) {
  # Find 3 letters followed by 1 digit, followed by 1 digit (mainland) or 2 digits (overseas) or 1 letter (Corsica)
  site_name <- stringr::str_subset(file_name_substrings , "[:alpha:]{3}([:digit:]{1})([:digit:]{1,2}|[:alpha:]{1})")
  if (length(site_name) == 0) {
    site_name <- NA
  } else if(nchar(site_name)>5) { # Site name is not 3 letters and 2 digits only
    site_name <- stringr::str_extract(site_name , "[:alpha:]{3}([:digit:]{1})([:digit:]{1,2}|[:alpha:]{1})")
  }
  return(site_name)
}
