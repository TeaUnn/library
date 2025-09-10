# this is a helper function to check if a certain package is installed,
# and install it and load if it it isnt, else just load it.

# use case: imagine youre writing a neat script for a customer to
# execute. when you source your setup you dont want to run
# install.packages() if not needed, but you do want to run
# it if needed.

install_and_load <- function(pkgs, dependencies = TRUE) {
  # ensure a CRAN mirror is set (avoids "@CRAN@" default on some systems)
  if (is.null(getOption("repos")) || identical(getOption("repos")["CRAN"], "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  
  for (pkg in pkgs) {
    # if not installed, install it
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing '", pkg, "' (with dependencies) ...")
      tryCatch(
        install.packages(pkg, dependencies = dependencies),
        error = function(e) {
          stop("Failed to install '", pkg, "'. Error: ", conditionMessage(e),
               "\nIf you're behind a corporate proxy, set proxy env vars before retrying, e.g.:",
               "\nSys.setenv(HTTP_PROXY='http://user:pass@proxy:port', HTTPS_PROXY='http://user:pass@proxy:port')",
               call. = FALSE)
        }
      )
    }
    # load silently
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
  invisible(TRUE)
}

# --- use like this: -----------------------------------------------------------

# install_and_load(c(
#   "readxl",   # For reading Excel files
#   "haven",    # For reading SAS files
#   "data.table",
#   "ggplot2"
# ))
