#' Create or install package
#' @param name package name
#' @param check run devtools::check on the package (default is FALSE)
#' @export
package <- function(name,check=FALSE,...) {

  old.wd <- getwd()
    
  accepted.packages <- c("icnc", "icon", "cospr", "rttools", "rmodis", "SplitR", "rhdf4", "dardarNi",
                         "rrrtmg", "niforcing", "baseutils", "darnitools", "nctools", "cldphys")

  if(length(name)==0) {
    stop(paste("Please indicate package name as argument. Currently:",paste(accepted.packages,collapse = ", ")))
  } else {
    package <- name[1]
    if(!package%in%accepted.packages) stop(paste("This package cannot be proccessed. Currently:",paste(accepted.packages,collapse = ", ")))
  }

  dir.packages <- paste0(Sys.getenv("HOME"),"/github/R.packages")

  if(package=="SplitR") {
    author <- 'person("Richard", "Iannone", email = "riannone@me.com", role = c("aut"))'
    license <- "MIT + file LICENSE"
  } else if(package=="rrrtmg") {
    author <- ' person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))'
    license <- "What license is it under?"
  } else {
    author <- 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))'
    license <- "GPL (>= 3)"
  }

  if(package=="icnc") description <- list(Title="ICNC tools for R",
                                          Description="Diverse tools for ice crystal number concentration analysis (extraction, plotting, etc)",
                                          Version="0.1")
  if(package=="icon") description <- list(Title="ICON tools for R",
                                          Description="Diverse tools for dealing with ICON data",
                                          Version="0.1")

  if(package=="cospr") description <- list(Title="COSP tools for R",
                                           Description="Diverse tools for dealing with COSP",
                                           Version="0.1")

  if(package=="rttools") description <- list(Title="Radiative transfer tools for R",
                                             Description="Diverse tools for radiative transfer calculations",
                                             Version="0.1")

  if(package=="rmodis") description <- list(Title="Tools to deal with MODIS data",
                                            Description="Diverse tools for MODIS reading/processing",
                                            Version="0.1")

  if(package=="SplitR") description <- list(Title="Use the HYSPLIT model from inside R",
                                            Description="Use the HYSPLIT model from inside R and do more with it.",
                                            Version="0.4")

  if(package=="rhdf4") description <- list(Title="Functions to read hdf4 files",
                                           Description="Tools to extract data from HDF4 files",
                                           Version="0.1")

  if(package=="dardarNi") description <- list(Title="Functions used to create the DARDAR Ni dataset",
                                              Description="Functions used to create the DARDAR Ni dataset",
                                              Version="1.0.0")

  if(package=="rrrtmg") description <- list(Title= "What the Package Does (one line, title case)",
                                            Description="What the package does (one paragraph).",
                                            Version="0.0.0.9000")

  if(package=="niforcing") description <- list(Title= "Package used for ni forcing paper",
                                               Description="Just what the title says",
                                               Version="0.1")

  if(package=="baseutils") description <- list(Title= "Some useful general functions for R",
                                               Description="Just what the title says",
                                               Version="0.1")

  if(package=="darnitools") description <- list(Title= "General tools to process DARDAR-Nice data",
                                                Description="Just what the title says",
                                                Version="1.0")

  if(package=="nctools") description <- list(Title= "General tools to process netcdf data",
                                             Description="Just what the title says",
                                             Version="1.0")

  if(package=="cldphys") description <- list(Title= "Some helpful functions for cloud and atmospheric physics",
                                             Description="Just what the title says",
                                             Version="1.0")

  options(devtools.desc.author=author,
          devtools.desc.license=license,
          devtools.desc=description
          )

  pkg <- paste(dir.packages,package,sep="/")

  if(!dir.exists(pkg)) {
    usethis::create_package(path=pkg)
    usethis::use_mit_license("Odran Sourdeval")
  }

  setwd(pkg)
  check.data <- function(...) if(length(list(...))) TRUE else FALSE
  if(check.data(...)) {
    usethis::use_data(...)
  }

  devtools::document(pkg)
  devtools::install(pkg)

  if(check) devtools::check(pkg)

  setwd(pkg)

  return(NULL)

}


