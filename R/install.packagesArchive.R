#' install.packagesArchive
#'
#' install.packagesArchive in a convenience wrapper for installing archived versions of packages from CRAN.
#'
#' @param package Name of the package
#' @param version Version of the package
#'
#' @return NULL
#' @export
#'
#' @examples
#' #install.packagesArchive(package = "mixlow", version = "1.0.1")
#' #install.packagesArchive(package = "mixlow", version = "2.0.1")
install.packagesArchive <- function(package, version) {
  packageurl <- paste0("http://cran.r-project.org/src/contrib/Archive/",
                       package, "/", package, "_", version, ".tar.gz")
  if(!url.exists(packageurl)) return(print("That package/version does not exist in the CRAN archives"))
  if(url.exists(packageurl)) install.packages(packageurl, repos=NULL, type='source')
}
