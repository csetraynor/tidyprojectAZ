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

check_if_az_rstudio_pro <- function(){
  tryCatch({
    ans <- get("RStudio.Version")()$edition
    if(grepl("pro",ans,ignore.case = TRUE)) return("TRUE") else
      return("FALSE: Use http://rstudio.rd.astrazeneca.net")
  },
  error = function(e){
    return("FALSE: Not RStudio. Use http://rstudio.rd.astrazeneca.net")
  })
}

#' az specific tests
#'
#' Not intended to be run directly by user
#' @param proj_name character. default = current working directory. path to directory.
#' @param silent logical. default = FALSE. suppress messages or not
#' @export

tidyprojectAZ_tests <- function(proj_name = getwd(),silent = FALSE){
  tidyproject::do_test(
    "running on AZ RStudio Server Pro" = check_if_az_rstudio_pro()
    ,"user R library removed" = {
      if(.Platform$OS.type != "unix") return("not on unix")
      current_lib_paths <- .libPaths()
      if(any(grepl("^/home/.+/R/",current_lib_paths))) return("FALSE") else return("TRUE")
    }, "working in QCP_MODELING" = {
      proj_name <- normalizePath(proj_name,winslash = "/",mustWork = FALSE)
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
new_script <- function(name, overwrite = FALSE, open_file = TRUE, libs=c("tidyprojectAZ")) {
  tidyproject::new_script(name=name,overwrite = overwrite,open_file = open_file,libs = libs)
}


#' Create new_project
#'
#' Creates directory structure.  User install tidyproject again in
#'
#' @param proj_name character string of full path to new_project
#' @param remove_user_lib logical (default=FALSE) if TRUE will attempt to remove
#'   user R package library from .libPaths()
#' @param overwrite_rprofile logical. should project .Rprofile be overwritten (default=FALSE)
#' @param type character. type argument for QCP knowledge management
#' @param name name. type argument for QCP knowledge management
#'
#' @export

make_project <- function(proj_name, remove_user_lib = FALSE,
                         overwrite_rprofile = FALSE, type, name) {
  in_qcp_km <- grepl("^/projects/QCP_MODELING",normalizePath(proj_name,winslash = "/",mustWork = FALSE))
  is_qcp_project_dir <- grepl("^/projects/QCP_MODELING/[^/]+/[^/]+$",normalizePath(proj_name,winslash = "/",mustWork = FALSE))
  if(in_qcp_km & !is_qcp_project_dir)
    if(!tidyproject::check_if_tidyproject(proj_name))
      stop("proj_name, when pointing to KM server, should either be:",
           "\n  a drug project dir (i.e. /project/QCP_MODELING/[TA]/[PROJECT]) OR",
           "\n  an existing activity created with the QCP KM scripts",call. = FALSE)
  if(is_qcp_project_dir){
    if(missing(type)|missing(name))
      stop("new activites on KM server need \"type\" and \"name\" arguments specified",call. = FALSE)
    run_in <- normalizePath(proj_name,winslash = "/",mustWork = TRUE)
    message("Launching up QCP knowledge management scripts")
    currentwd <- getwd() ; setwd(run_in) ; on.exit(setwd(currentwd))
    system(paste("/home/qcpadmin/bin/create_activity",type,name))
    parent_dir <- dir(run_in, full.names = TRUE)
    proj_name <- parent_dir[grepl(type,parent_dir) & grepl(name,parent_dir)]
    if(!file.exists(proj_name))
      stop("Something wrong. QCP KM scripts failed to create activity.",
           "\n  Revert to command line KM process",call. = FALSE)
    proj_name <- normalizePath(proj_name,winslash = "/",mustWork = TRUE)
    message("Making tidyproject...")
  }
  tidyproject::make_project(proj_name=proj_name, remove_user_lib = remove_user_lib,
                            overwrite_rprofile = overwrite_rprofile)
}

