#' Create and register a cluster; Cluster ID is returned.
#' @param names cluster names
#' @param ncores number of cpu per cluster
#' @param outfile outfile for cluster runs
#' @export
regcluster <- function(ncores=1,names="localhost",outfile="/dev/null") {

    if(length(ncores)==1) {
        spec <- rep(names,ncores)
    } else if(length(ncores)==length(names)) {
        spec <- as.vector(sapply(1:length(names),function(i) {paste(rep(names[i],ncores[i]))}))
    } else {
        return(warning("Number of cores is incorrect."))
    }
    print(spec)

    cl <- snow::makeCluster(spec, type = "SOCK", outfile = outfile)
    doSNOW::registerDoSNOW(cl)

    return(cl)
}


#' List information on requested clusters
#' @param user username
#' @export
lscluster <- function(user="b380333") {
    cmd <- paste0('squeue -u ',user,' -h ')
    df <- data.frame(jobid=system(paste0(cmd,'--format="%i"'),intern=TRUE),
                     partition=system(paste0(cmd,'--format="%P"'),intern=TRUE),
                     user=system(paste0(cmd,'--format="%u"'),intern=TRUE),
                     status=system(paste0(cmd,'--format="%t"'),intern=TRUE),
                     nodes=system(paste0(cmd,'--format="%D"'),intern=TRUE),
                     time=system(paste0(cmd,'--format="%M"'),intern=TRUE),
                     nodelist=system(paste0(cmd,'--format="%R"'),intern=TRUE),
                     stringsAsFactors = FALSE) %>%
        dplyr::mutate(nodes=as.numeric(nodes),
                      jobid=as.numeric(jobid))

    ## Return a running local partition if there is nothing else
    if(nrow(df%>%dplyr::filter(status=="R"))==0) {
        df <- dplyr::bind_rows(df,data.frame(jobid=NA,partition="local",user="b380333",status="R",nodes=1,time=NA,nodelist="localhost"))
    }

    return(df)

}


#' Run a function on a local cluster
#' Can be used to embed a local cluster into a remote one
#' @param x_local vector or list of input parameters
#' @param fun_local function to be ran locally
#' @param ... extra arguments to be used by fun_local
#' @export
localcluster <- function(x_local,fun_local,...) {
  cl_local <- baseutils::regcluster(length(x_local),"localhost")
  out <- snow::clusterApply(cl=cl_local,x=x_local,fun=fun_local,...)
  snow::stopCluster(cl_local)
  return(out)
}




## ' Request an exclusive nodes on a cluster
## ' @param project project nodes to request
## ' @param n number of nodes
## ' @param time requested time on the node (format: HH:MM:SS), default is 8h
## ' @param part name of the requested partition
## ' @param mem amount of memory required
##' @export
## alcluster <- function(project,n=1,time="08:00:00",part="compute2",mem=254000) {
##     cmd <- paste0('salloc -A ',project,' --time=',time,' --partition=',part,' --exclusive --mem=',mem,' &')
##     for(i in 1:n) system(cmd,ignore.stdout=TRUE,ignore.stderr=TRUE,wait=FALSE)
##     return(NULL)
## }
