# function to run last in every chunk in quarto to clean.
# it will remove all objects in global environment that is not named
# "g_name".
# (however, it won't remove hidden files (".file"))

g_clean <- function() {
  rm(list = setdiff(ls(envir = .GlobalEnv),
                    ls(pattern = "^g_",
                       envir = .GlobalEnv)),
     envir = .GlobalEnv)
}