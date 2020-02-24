
#' @export
logfile <- function(...,fn,lev=0,append=TRUE,dots=FALSE,newline=FALSE) {
    pre <- rep(" ",lev); post <- ""
    if(dots) post <- paste0(post,"...")
    if(newline) post <- paste0(post,"\n")

    cat(pre,...,post,file=fn,append=append)

    return(NULL)
}
