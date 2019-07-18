
#' Read and write rds files
#' @param name name of the output rds file
#' @param data data to be written. If NULL, the rds file is loaded.
#' @param dir.save repository where the rds file is written
#' @param overwrite overwrite if existing file (default is FALSE)
#' @export
rds <- function(name,data=NULL,dir.save=NULL,overwrite=FALSE) {

    if(is.null(dir.save)) {
        if(Sys.getenv("R.rds")!="") {
            dir.save <- Sys.getenv("R.rds")
        } else {
            dir.save <- paste0(Sys.getenv("HOME"),"/storage/R.rds")
        }
    }

    if(name=="ls") return(gsub(".rds","",list.files(dir.save)))
    
    fn.rds <- paste0(dir.save,"/",name,".rds")
       
    if(is.null(data)) {
        if(!file.exists(fn.rds)) return("File doesn't exist")
        data <- readRDS(fn.rds); return(data)
    } else {
        if(file.exists(fn.rds) & !overwrite) return("File already exists. Explicit overwrite if necessary.")
        dir.create(dirname(fn.rds),showWarnings=FALSE)        
        saveRDS(data,fn.rds)
    }
    
    return(NULL)
    
}

