#' Check tidyproject for best practice compliance
#'
#' @param proj_name character. default = current working directory. path to directory.
#' @param silent logical. default = FALSE. suppress messages or not
#' @param check_rstudio logical (default = FALSE). Check rstudio studio project directory
#' @export

check_session <- function(proj_name = getwd(), silent = FALSE, check_rstudio = TRUE) {

  daz0 <- tidyprojectAZ_tests(proj_name = proj_name,
                              silent = silent)

  dtidy <- tidyproject::check_session(proj_name = proj_name,
                                      silent = silent,
                                      check_rstudio = check_rstudio)


  d <- rbind(daz0,dtidy)
  invisible(d)
}

#' az specific tests
#'
#' Not intended to be run directly by user
#' @param proj_name character. default = current working directory. path to directory.
#' @param silent logical. default = FALSE. suppress messages or not
#' @export

tidyprojectAZ_tests <- function(proj_name = getwd(),silent = FALSE){
  tidyproject::do_test(
    "running on AZ RStudio Server Pro" = {
      tryCatch({
        ans <- get("RStudio.Version")()$edition
        if(grepl("pro",ans,ignore.case = TRUE)) return("TRUE") else
          return("FALSE: Use http://rstudio.rd.astrazeneca.net")
      },
      error = function(e){
        return("FALSE: Not RStudio. Use http://rstudio.rd.astrazeneca.net")
      })
    }, "user R library removed" = {
      if(.Platform$OS.type != "unix") return("not on unix")
      current_lib_paths <- .libPaths()
      if(any(grepl("^/home/.+/R/",current_lib_paths))) return("FALSE") else return("TRUE")
    }, "working in QCP_MODELING" = {
      proj_name <- normalizePath(proj_name)
      if(grepl("^/projects/QCP_MODELING/",proj_name)) return("TRUE") else return("FALSE")
    },
    silent = silent)
}

#' Create new R script
#' @param name character indicating name of script to create
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @param libs character. What libraries to add.
#' @export
new_script <- function(name, overwrite = FALSE, open_file = TRUE, libs=c("NMprojectAZ")) {
  tidyproject::new_script(name=name,overwrite = overwrite,open_file = open_file,libs = libs)
}


#' Create new_project
#'
#' Creates directory structure.  User install tidyproject again in
#'
#' @param proj_name character string of full path to new_project.
#'  Should be path to analysis directory
#'  UNLESS starting a new analysis in QCP KM directory
#'   then proj_name should be directory of drug project and type and name should be specified.
#' @param remove_user_lib logical (default=FALSE) if TRUE will attempt to remove
#'   user R package library from .libPaths()
#' @param type character. type argument for QCP knowledge management
#' @param name name. type argument for QCP knowledge management
#'
#' @export
make_project <- function(proj_name, remove_user_lib = FALSE, type, name) {
  new_proj <- !file.exists(proj_name)
  in_qcp_km <- grepl("^/projects/QCP_MODELING",normalizePath(proj_name,winslash = "/"))
  if(new_proj & in_qcp_km){
    if(missing(type)|missing(name))
      stop("new projects on knowledge management need \"type\" and \"name\" arguments specified")
    run_in <- normalizePath(proj_name,winslash = "/")
    if(grepl("^/projects/QCP_MODELING/[^/]/.*/*$",run_in))
      stop("proj_name doesn't seem to correspond to a drug project directory.\n  Set up analysis directory manually")
    message("Launching up QCP knowledge management scripts")
    system_nm(paste("/home/qcpadmin/bin/create_activity",type,name),
              dir = run_in)
  }
  tidyproject::make_project(proj_name=proj_name, remove_user_lib = remove_user_lib)
}

system_nm <- function(cmd,dir=getOption("models.dir"),...){
  if(is.null(dir) | !file.exists(dir)) dir <- "."
  if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_nm")(cmd,...)
}
