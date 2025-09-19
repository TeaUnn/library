# this is a helper function to check if a certain package is installed,
# and install it and load if it it isnt, else just load it.

# use case: imagine youre writing a neat script for a customer to
# execute. when you source your setup you dont want to run
# install.packages() if not needed, but you do want to run
# it if needed.

install_and_load <- function(pkgs, dependencies = TRUE, stop_on_error = FALSE, verbose = TRUE) {
  success <- character()
  failed <- character()
  
  for (pkg in pkgs) {
    # Try to install if not already installed
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing '", pkg, "' (with dependencies) ...")
      tryCatch(
        install.packages(pkg, dependencies = dependencies),
        error = function(e) {
          msg <- paste0("Failed to install '", pkg, "'. Error: ", conditionMessage(e))
          if (stop_on_error) stop(msg, call. = FALSE) else message(msg)
        }
      )
    }
    
    # Try to load the package
    tryCatch(
      {
        suppressPackageStartupMessages(
          library(pkg, character.only = TRUE)
        )
        success <- c(success, pkg)
      },
      error = function(e) {
        msg <- paste0("Failed to load '", pkg, "'. Error: ", conditionMessage(e))
        if (stop_on_error) stop(msg, call. = FALSE) else {
          message(msg)
          failed <- c(failed, pkg)
        }
      }
    )
  }
  
  # Optional summary
  if (verbose) {
    message("\n--- Summary ---")
    message("Successfully loaded: ", if (length(success)) paste(success, collapse = ", ") else "None")
    message("Failed to load: ", if (length(failed)) paste(failed, collapse = ", ") else "None")
  }
  
  invisible(list(success = success, failed = failed))
}


# --- use like this: -----------------------------------------------------------

# install_and_load(c(
#   "readxl",   # For reading Excel files
#   "haven",    # For reading SAS files
#   "skräp"
#   ),
#   dependencies = TRUE,   # install also dependent packages
#   stop_on_error = FALSE, # do keep on running if something goes wrong
#   verbose = TRUE         # give me summary info
# )
