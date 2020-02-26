#' Create and register a local or remote parallel socket cluster.
#'
#' @param nnodes integer or vector; number of nodes to be forked.
#' Default is 1.
#' @param names character or vector; host name(s) on which copies of R will be ran.
#' Default is "localhost".
#' @param outfile character or vector; file containing the output from each host.
#' Default is "/dev/null".
#' @param silent logical; indicates if the function should be silent, else registered host names will be returned.
#' Default is TRUE.
#' @return
#' an object of class `cluster'
#' @export
regcluster <- function(nnodes = 1, names = "localhost", outfile = "/dev/null", silent = TRUE) {

  if(length(nnodes) == 1) {
    spec <- rep(names, nnodes)
  } else if(length(nnodes) == length(names)) {
    spec <- as.vector(sapply(1:length(names), function(i) {paste(rep(names[i],nnodes[i]))}))
  } else {
    return(warning("Number of cores is incorrect."))
  }
  if(!silent) print(spec)

  cl <- snow::makeCluster(spec, type = "SOCK", outfile = outfile)
  doSNOW::registerDoSNOW(cl)

  return(cl)
}


#' List information on requested clusters.
#' Currently only works for clusters using a SLURM management system.
#'
#' @param user character; name of the cluster user.
#' @param allocated logical; indicates of the cluster information should be limited to allocated (running) nodes.
#' Default is TRUE.
#' @return
#' a data frame containing the job ID, partition, username, status, node host names and node numbers associated with user.
#' @export
lscluster <- function(user, allocated=TRUE) {

  cmd <- paste0('squeue -u ', user, ' -h ')

  df <- data.frame(jobid = system(paste0(cmd, '--format="%i"'), intern=TRUE),
                   partition = system(paste0(cmd, '--format="%P"'), intern=TRUE),
                   user = system(paste0(cmd, '--format="%u"'), intern=TRUE),
                   status = system(paste0(cmd, '--format="%t"'), intern=TRUE),
                   nodes = system(paste0(cmd, '--format="%D"'), intern=TRUE),
                   time = system(paste0(cmd, '--format="%M"'), intern=TRUE),
                   nodelist = system(paste0(cmd, '--format="%R"'), intern=TRUE),
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(nodes = as.numeric(nodes),
                  jobid = as.numeric(jobid))

  ## Return a running local partition if there is nothing else
  if(nrow(df %>% dplyr::filter(status == "R")) == 0) {
    df <- rbind(df,data.frame(jobid=NA, partition="local", user=user, status="R",
                              nodes=1, time="0:00", nodelist="localhost"))
  }

  if(allocated) df %>% dplyr::filter(status == "R") -> df


  return(df)

}


#' Run a function on a local cluster.
#' Can be used to embed a local cluster into a remote one.
#'
#' @param x_local vector; input parameters to be used by snow::clusterApply.
#' This corresponds to the first argument of `fun_local'.
#' The length of x_local determines the size of the cluster that will be registered.
#' @param fun_local function that will be executed on the local cluster.
#' @param ... extra arguments to be used by fun_local
#' @return
#' List containing the function outputs
#' @export
localcluster <- function(x_local, fun_local, ...) {

  ## Register a local socket cluster
  cl_local <- baseutils::regcluster(length(x_local), "localhost")

  ## Run `fun_local' on that cluster
  out <- snow::clusterApply(cl=cl_local,x=x_local,fun=fun_local,...)

  snow::stopCluster(cl_local)

  return(out)
}


#' Run clusterApply on local sub-clusters created on the selected hosts.
#'
#' @param n integer; size of the local socket sub-cluster to create.
#' @param cl cluster object; cluster on which the local sub-clusters will be executed.
#' @param x vector; first input argument of the `fun' function
#' @param fun function; function to be executed on the sub-clusters
#' @param ... extra input arguments of the `fun' function
#' @return
#' List containing function outputs
#' @examples
#'
#' # Extract information on allocated nodes for the user "username". This is hereinafter called the main cluster.
#' # cl.info <- baseutils::lscluster(user="username")
#'
#' #Create and register the main cluster
#' # cl <- baseutils::regcluster(cl.info$nodes,cl.info$nodelist)
#'
#' # Run the function `fun_cluster' on sub-clusters of 20 nodes that will be created on each nodes of the main cluster.
#' # Extra arguments `argument1' and `argument2' are two arguments of the `fun_cluster' function.
#' # argument1 is a vector whose elements will be subset into 20 equal chunks that will be passed to each node of the main cluster.
#' # null <- baseutils::clusterApply_hop(n = 20, cl = cl, x = argument1, fun = fun_cluster, argument2)
#'
#' # De-register the main cluster
#' # snow::stopCluster(cl)
#'
#' @export
clusterApply_hop <- function(n, cl, x, fun, ...) {

  x.chunks <- baseutils::eqsplit(x, n)

  snow::clusterApply(cl, x.chunks, baseutils::localcluster ,fun_local=fun, ...)

}
