
#' Convert a POSIXct time into seasons
#' @param time POSIXct variable
#' @export
time2season <- function(time) {
    seq.months <- stringr::str_pad(1:12,2,pad="0")
    seq.seas <- c("DJF","DJF",rep("MAM",3),rep("JJA",3),rep("SON",3),"DJF")
    factor(format(time,"%m"),levels=seq.months,labels=seq.seas) 
}
