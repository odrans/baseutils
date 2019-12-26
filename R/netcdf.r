#' @export
ncvar.load <- function(nc,var,load.val=TRUE) {
    l <- vector("list")
    if(load.val) l$val <- as.numeric(ncdf4::ncvar_get(nc,var))
    l$name <- nc$var[[var]]$name
    l$units <- nc$var[[var]]$units
    l$longname <- nc$var[[var]]$longname
    l$standard_name <- ncdf4::ncatt_get(nc,var,"standard_name")$value; if(l$standard_name==0) l$standard_name <- ""
    l$description <- ncdf4::ncatt_get(nc,var,"description")$value; if(l$description==0) l$description <- ""        
    l$prec <- nc$var[[var]]$prec
    l$ndims <- nc$var[[var]]$ndims
    l$dimids <- nc$var[[var]]$dimids
    l$size <- nc$var[[var]]$size
    l$missval <- nc$var[[var]]$missval

    tmp <- plyr::laply(nc$var[[var]]$dim,function(x) x$name)
    l$dimnames <- rev(c(tail(tmp,1),rev(head(tmp,-1))))

    return(l)
}


#' @export
ncdim.load <- function(nc,var) {
    l <- vector("list")
    l$name <- nc$dim[[var]]$name
    l$val <- nc$dim[[var]]$val
    l$units <- nc$dim[[var]]$units
    l$len <- nc$dim[[var]]$len
    l$id <- nc$dim[[var]]$id
    return(l)
}
