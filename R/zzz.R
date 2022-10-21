#' Squash the global variable notes when building a package. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "time", "series", "date_unix", "uqk", "device", "sensor", "unit", 
    "value", "day"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
