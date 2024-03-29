% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{regcluster}
\alias{regcluster}
\title{Create and register a local or remote parallel socket cluster.}
\usage{
regcluster(
  nnodes = 1,
  names = "localhost",
  outfile = "/dev/null",
  silent = TRUE
)
}
\arguments{
\item{nnodes}{integer or vector; number of nodes to be forked.
Default is 1.}

\item{names}{character or vector; host name(s) on which copies of R will be ran.
Default is "localhost".}

\item{outfile}{character or vector; file containing the output from each host.
Default is "/dev/null".}

\item{silent}{logical; indicates if the function should be silent, else registered host names will be returned.
Default is TRUE.}
}
\value{
an object of class `cluster'
}
\description{
Create and register a local or remote parallel socket cluster.
}
