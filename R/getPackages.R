#' getPackages
#'
#' getPackages checks the package library for the package list. For any packages not yet installed, it will install them, and then will load all packages.
#'
#' @param list a vector of packages to be installed/loaded
#'
#' @return NULL
#' @export
#'
#' @examples
#' #getPackages(c("tidyr", "dplyr", "ggplot2"))
getPackages <- function(list) {
  new.packages <- list[!(list %in% installed.packages()[,"Package"])]
  old.packages <- list[(list %in% installed.packages()[,"Package"])]
  if (length(old.packages)) {
    print(cbind(paste0(old.packages, " is already installed.")))
  }
  if (length(new.packages)) {
    print(cbind(
      paste0(new.packages, " not yet installed. Attempting to install now")
    ))
    install.packages(new.packages)
  }
  lapply(list,function(x) {
    library(x,character.only = TRUE)
  })
  print(cbind(paste0(list, " has been loaded")))
}
