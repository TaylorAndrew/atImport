#' sourceDir
#'
#' sourceDir sources all .R files contained at a given path directory
#'
#' @param path Path to folder containing all files to source.
#' @param verbose If TRUE, prints the name of each file sourced to the console
#'
#' @return NULL
#' @export
#'
#' @examples
#' #NULL
sourceDir <- function(path, verbose = TRUE) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if (verbose)
      cat(nm,":")
    source(file.path(path, nm))
    if (verbose)
      cat("\n")
  }
}
