#' Standardized log file output
#'
#' @param logtext string, text to be exported in the log file
#' @param logfn string, name of the log file
#' @param lev integer, indenting space to be added before `logtext'.
#' Default is 0.
#' @param dots logical, indicates if an ellipsis should be added at the end of `logtext'.
#' Default is FALSE.
#' @param newline logical, indicates if a new line should be added after `logtext'.
#' Default is FALSE.
#' @param append logical, indicates if the log file should be appended, otherwise an existing file would be replaced.
#' Default is TRUE.
#' @return
#' NULL
#' @export
logfile <- function(logtext,logfn,lev=0,dots=FALSE,newline=FALSE,append=TRUE) {
    pre <- rep(" ",lev); post <- ""
    if(dots) post <- paste0(post,"...")
    if(newline) post <- paste0(post,"\n")

    cat(pre,logtext,post,file=logfn,append=append)

    return(NULL)
}
