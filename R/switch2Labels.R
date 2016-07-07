#Helper function for haven package. Specifically regarding the read_sas() function.

#Haven package imports SAS datasets. Optionally, user can also import the
#.sas7bcat file that includes all formats.
#Haven applies a class 'labelled' to the variables with the meta data providing
#what those labels are, but does not directly apply the labels to the variables.

#This function iterates through all variables in a given dataset and checks
#for class == 'labelled', and where class == 'labelled' is TRUE, uses
#haven::as_factor() to apply the labels.
switch2Labels <- function(data) {
  len <- length(names(data))
  i = 1
  recur <- function(i) {
    if(class(data[,i])=='labelled') data[, i] <<- as_factor(data[,i])
    if(i==len) return(data)
    if(i < len) {
      i = i + 1
      recur(i)
  }
  }
  data = recur(1)
  return(data)
}