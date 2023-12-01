#' getfiledestinations
#'
#' Create file2destination mapping based on information from the model
#'
#'
#' @author Jan Philipp Dietrich, David Klein


getfiledestinations <- function() {
  message(Sys.time(), '   start getfiledestinations()')
  folders <- base::list.dirs(recursive = FALSE, full.names = FALSE)
  folders <- grep("^(\\.|225|output|calib_run|figure)", folders, invert = TRUE, value = TRUE)
  files <- NULL
  for (f in folders) {
    message(Sys.time(), '   in "folders" loop')
    files <- c(files, dir(path = f, pattern = "^files$", recursive = TRUE, full.names = TRUE))
  }
  out <- NULL
  for (f in files) {
    message(Sys.time(), '   in "files" loop')
    tmp <- grep("^\\*", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
    add <- data.frame(file = tmp, destination = dirname(f), stringsAsFactors = FALSE)
    out <- rbind(out, add)
  }
  if (is.null(out)) {
    message(Sys.time(), '   getfiledestinations() return(NULL)')
    return(NULL)
  }
  out <- as.data.frame(lapply(out, trimws), stringsAsFactors = FALSE)
  message(Sys.time(), '   getfiledestinatins() return(out)')
  return(out[out[[1]] != "", ])
}
