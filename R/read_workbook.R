#' read_workbook
#'
#' read_workbook is an extension of readxl::read_excel, loading all sheets in an excel workbook either directly into the global environment or as a part of a list
#'
#' @param ExcelFile Character string of the directory and filename pointing at the file to load
#' @param na_value Value in the excel workbook that should be treated as missing/NA
#' @param add_to_Envir TRUE/FALSE: Should each sheet be added to the global environment, each as a data.frame
#' @param output_ls TRUE/FALSE: Should each sheet be appended to a list of data.frames
#'
#' @return if output_ls==TRUE, a list of data.frames.
#' @export
#'
#' @examples
#' #NULL
read_workbook <- function(ExcelFile, na_value = '', add_to_Envir = TRUE, output_ls = FALSE) {
  if (!file.exists(ExcelFile)) {
    print(paste0("The file at: ", ExcelFile, " does not exist"))
    if (substring(ExcelFile, nchar(ExcelFile), nchar(ExcelFile)) %in% c("x", "X")) {
      ExcelFile <- substring(ExcelFile, 1, (nchar(ExcelFile) - 1))
      print(paste0("Now looking for: ", ExcelFile))
      if (!file.exists(ExcelFile)) {
        return(paste0("The file at: ", ExcelFile, " does not exist"))
      }
      if (file.exists(ExcelFile))
        print(paste0(ExcelFile, " exists and will be loaded"))
    } else  if (!substring(ExcelFile, nchar(ExcelFile), nchar(ExcelFile)) %in%
                c("x", "X")) {
      ExcelFile <- paste0(ExcelFile, "x")
      print(paste0("Now looking for: ", ExcelFile))
      if (!file.exists(ExcelFile)) {
        return(paste0("The file at: ", ExcelFile, " does not exist"))
      }
      if (file.exists(ExcelFile))
        print(paste0(ExcelFile, " exists and will be loaded"))
    }
  }

  if (file.exists(ExcelFile)) {
    sheetn <- excel_sheets(ExcelFile)
    #Load data
    getsheet <- function(x) {
      dat <- read_excel(ExcelFile, sheet = x, na = na_value)
    }

    loaddat <- function(i) {
      x <- getsheet(sheetn[i])
      if (add_to_Envir == TRUE)
        assign(sheetn[i],x,env = .GlobalEnv)
      return(x)
    }
    list_of_data <- lapply(1:length(sheetn),loaddat)
    names(list_of_data) <- sheetn
    if (output_ls == TRUE)
      return(list_of_data)
  }
}
