.onLoad <- function(libname, pkgname){
  if(!requireNamespace("tidyproject",quietly = TRUE))
    stop("tidyproject needed for this function to work. Please install it.",
         call. = FALSE)
  x <- lapply("tidyproject",library,character.only = TRUE,warn.conflicts=FALSE)
  set_opts()
  invisible()
}
