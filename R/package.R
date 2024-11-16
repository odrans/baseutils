#' Create or install package
#' @param name package name
#' @param check run devtools::check on the package (default is FALSE)
#' @export
package <- function(name, check = FALSE, ...) {

  old.wd <- getwd()

  accepted.packages <- c("icnc", "icon", "cospr", "rttools", "rmodis", "SplitR", "rhdf4", "dardarNi",
                         "rrrtmg", "niforcing", "baseutils", "darnitools", "nctools", "cldphys", "Rs3com",
                         "darflex")

  if (length(name) == 0) {
    stop(paste("Please indicate package name as argument. Currently:", paste(accepted.packages, collapse = ", ")))
  } else {
    package <- name[1]
    if (!package %in% accepted.packages) {
      stop(paste("This package cannot be processed. Currently:", paste(accepted.packages, collapse = ", ")))
    }
  }

  dir.packages <- paste0(Sys.getenv("HOME"), "/github/R.packages")

  # Define package metadata in a list
  package_info <- list(
    icnc = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                license = "GPL (>= 3)",
                description = list(Title = "ICNC tools for R",
                                   Description = "Diverse tools for ice crystal number concentration analysis",
                                   Version = "0.1")),
    icon = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                license = "GPL (>= 3)",
                description = list(Title = "ICON tools for R",
                                   Description = "Diverse tools for dealing with ICON data",
                                   Version = "0.1")),
    cospr = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                 license = "GPL (>= 3)",
                 description = list(Title = "COSP tools for R",
                                    Description = "Diverse tools for dealing with COSP",
                                    Version = "0.1")),
    rttools = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                   license = "GPL (>= 3)",
                   description = list(Title = "Radiative transfer tools for R",
                                      Description = "Diverse tools for radiative transfer calculations",
                                      Version = "0.1")),
    rmodis = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                  license = "GPL (>= 3)",
                  description = list(Title = "Tools to deal with MODIS data",
                                     Description = "Diverse tools for MODIS reading/processing",
                                     Version = "0.1")),
    SplitR = list(author = 'person("Richard", "Iannone", email = "riannone@me.com", role = c("aut"))',
                  license = "MIT + file LICENSE",
                  description = list(Title = "Use the HYSPLIT model from inside R",
                                     Description = "Use the HYSPLIT model from inside R and do more with it.",
                                     Version = "0.4")),
    rhdf4 = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                 license = "GPL (>= 3)",
                 description = list(Title = "Functions to read hdf4 files",
                                    Description = "Tools to extract data from HDF4 files",
                                    Version = "0.1")),
    dardarNi = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                    license = "GPL (>= 3)",
                    description = list(Title = "Functions used to create the DARDAR Ni dataset",
                                       Description = "Functions used to create the DARDAR Ni dataset",
                                       Version = "1.0.0")),
    rrrtmg = list(author = 'person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))',
                  license = "What license is it under?",
                  description = list(Title = "What the Package Does (one line, title case)",
                                     Description = "What the package does (one paragraph).",
                                     Version = "0.0.0.9000")),
    niforcing = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                     license = "GPL (>= 3)",
                     description = list(Title = "Package used for ni forcing paper",
                                        Description = "Just what the title says",
                                        Version = "0.1")),
    baseutils = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                     license = "GPL (>= 3)",
                     description = list(Title = "Some useful general functions for R",
                                        Description = "Just what the title says",
                                        Version = "0.1")),
    darnitools = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                      license = "GPL (>= 3)",
                      description = list(Title = "General tools to process DARDAR-Nice data",
                                         Description = "Just what the title says",
                                         Version = "1.0")),
    nctools = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                   license = "GPL (>= 3)",
                   description = list(Title = "General tools to process netcdf data",
                                      Description = "Just what the title says",
                                      Version = "1.0")),
    cldphys = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                   license = "GPL (>= 3)",
                   description = list(Title = "Some helpful functions for cloud and atmospheric physics",
                                      Description = "Just what the title says",
                                      Version = "1.0")),
    Rs3com = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                  license = "MIT",
                  description = list(Title = "Useful tools for S3COM",
                                     Description = "",
                                     Version = "1.0")),
    darflex = list(author = 'person("Odran", "Sourdeval", email = "odran.sourdeval@univ-lille.fr", role = c("aut", "cre"))',
                   license = "GPL-3",
                   description = list(Title = "Tools for DARDAR and FLEXPART",
                                      Description = "",
                                      Version = "0.9"))
  )

  # Assign package metadata
  if (package %in% names(package_info)) {
    author <- package_info[[package]]$author
    license <- package_info[[package]]$license
    description <- package_info[[package]]$description
  } else {
    stop("Package metadata not found.")
  }

  options(devtools.desc.author = author,
          devtools.desc.license = license,
          devtools.desc = description)

  pkg <- paste(dir.packages, package, sep = "/")

  if (!dir.exists(pkg)) {
    usethis::create_package(path = pkg)
    if (license == "MIT") {
      usethis::use_mit_license("Odran Sourdeval")
    } else if (grepl("GPL", license)) {
      usethis::use_gpl3_license(license)
    }
  }

  setwd(pkg)
  if (length(list(...)) > 0) {
    usethis::use_data(...)
  }

  devtools::document(pkg)
  devtools::install(pkg)

  if (check) {
    devtools::check(pkg)
  }

  setwd(old.wd)

  return(NULL)
}
