clean_environment <- function(type = c("variables", "functions", "both"), include_hidden = FALSE) {
  type <- match.arg(type)
  
  # List objects, optionally including hidden ones
  all_objects <- ls(envir = .GlobalEnv, all.names = include_hidden)
  
  # Identify which are functions
  is_func <- sapply(all_objects, function(x) is.function(get(x, envir = .GlobalEnv)))
  
  # Select objects to remove based on arguments
  if (type == "variables") {
    to_remove <- all_objects[!is_func]
  } else if (type == "functions") {
    to_remove <- all_objects[is_func]
  } else if (type == "both") {
    to_remove <- all_objects
  }
  
  # Remove selected objects
  rm(list = to_remove, envir = .GlobalEnv)
}
