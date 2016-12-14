#' update_github_pkgs
#'
#' update_github_pkgs updates all github packages
#'
#' @return N/A
#' @export
#'
#' @examples
#' update_github_pkgs()
update_github_pkgs <- function(pkgList = NULL) {

  # check/load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }

  pkgs <- installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  if(!is.null(pkgList)) {
    githubPackagesInstalled <- pkgList[pkgList%in%github_pkgs]
    githubPackagesNotInstalled <- pkgList[!pkgList%in%github_pkgs]
    if(length(githubPackagesInstalled)>0) {
    print(paste0('The following package was detected and will be updated: ',
                 githubPackagesInstalled))
    github_pkgs <- githubPackagesInstalled
    }
    if(length(githubPackagesNotInstalled)>0) {
    print(paste0('The following package was NOT detected and will NOT be updated: ',
                 githubPackagesNotInstalled))
    github_pkgs <- NULL
    }
  }

  # print(github_pkgs)
  if(!is.null(github_pkgs)) {
    lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")

    repo = packageDescription(pac, fields = "GithubRepo")
    username = packageDescription(pac, fields = "GithubUsername")

    install_github(repo = paste0(username, "/", repo))
    })
  }
}
