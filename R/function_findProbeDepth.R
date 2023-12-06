#' Probe depth
#' 
#' Find probe depth based on file name according to a 5-digit system describing depth in cm
#' 
#' @param file_name_substrings character vector, containing substrings of file name
#' @return the probe depth in m (string).
#' @examples
#' file_name_substrings=c( "files/20131213","BIM13","00001");findProbeDepth(file_name_substrings)

findProbeDepth <- function(file_name_substrings) {
  # Find 5 digits starting with at least one zero
  probe_depth <- stringr::str_subset(file_name_substrings , "^[0][:digit:]{4}")
  if (length(probe_depth) == 0) {
    probe_depth <- NA
  } else if (nchar(probe_depth)>5) { # Probe depth is not 5 digits only
    probe_depth <- stringr::str_extract(probe_depth , "[:digit:]{5}")
  }
  # Convert depth in m
  if(!is.null(probe_depth)) {
    probe_depth <- as.numeric(probe_depth) * 10^-2
  }
  return(probe_depth)
}
