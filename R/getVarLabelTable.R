#' getVarLabelTable
#'
#' getvarLabelTable will extract the variable names and variable labels from a data.frame imported from an SPSS or SAS file via either the foreign or Hmisc packages
#'
#' @param SAS.SPSS_Data a data.frame object imported via 'foreign' or 'Hmisc'
#' @param import.method the import method used: foreign or Hmisc
#'
#' @return A data.frame object containing the names of the data.frame, along with the corresponding variable labels.
#' @export
#'
#' @examples
#' #NULL
getVarLabelTable <- function(SAS.SPSS_Data, import.method = "foreign") {
  if (!import.method %in% c("foreign","Hmisc"))
    print("Data must have been imported using either 'foreign' or 'Hmisc'")
  else{
    if (import.method == "Hmisc") {
      VarLabsAttrs <- label(SAS.SPSS_Data)
    }
    if (import.method == "foreign") {
      VarLabsAttrs <- get_var_labels(SAS.SPSS_Data)
    }
    VarLabsdf <- data.frame(vars = names(VarLabsAttrs),
                            labels = VarLabsAttrs)
    VarLabsdf[,2] <-
      suppressWarnings(ifelse(
        VarLabsdf[,2] == "",
        as.character(VarLabsdf[,1]),
        as.character(VarLabsdf[,2])
      ))
    return(VarLabsdf)
  }
}
