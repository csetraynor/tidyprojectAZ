set_opts <- function(){

  options(code_library_path = "/projects/QCP_MODELING/PMXcodelibrary/")

  options(system_nm = function(cmd,...) {
    if(grepl("seml.+\\.astrazeneca\\.net",Sys.info()["nodename"])) {
      system(paste0("ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); module unload psn git r && module load psn nonmem git r && ",cmd,"\""),...)
    } else {
      stop("need to be on AZ infrastructure")
    }
  })

}
