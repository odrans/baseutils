
#' Attach multiple sqlite3 files into a single sqlite3 file
#' @param lf.in : list of input sqlite3 files to be merged
#' @param fn.out : name of the output sqlite3 file
#' @param tablename : name of the table to be merged. All others will be dropped.
#' @export
SQLite.attach <- function(lf.in,fn.out,tablename) {

    file.copy(lf.in[1],fn.out,overwrite=TRUE)
    if(length(lf.in)==1) return(NULL)

    db.merged <- src_sqlite(fn.out); con <- db.merged$con

    ## Only tablename will be merged
    all.tables <- RSQLite::dbListTables(con)

    idx.rm <- which(!(grepl("sqlite_",all.tables) | grepl(tablename,all.tables)))
    if(length(idx.rm)!=0) {
        for(i in idx.rm)  RSQLite::dbSendQuery(con,paste0("DROP TABLE `",all.tables[i],"`"))
        RSQLite::dbSendQuery(con,"vacuum")
    }

    for(fn in lf.in[-1]) {
        rs <- DBI::dbSendQuery(con,paste0("attach '",fn,"' as tmp")); DBI::dbClearResult(rs)
        rs <- DBI::dbSendQuery(con, "begin"); DBI::dbClearResult(rs)
        rs <- try(DBI::dbSendQuery(con,paste0("insert into '",tablename,"' select * from tmp.'",tablename,"'"))); DBI::dbClearResult(rs)
        rs <- DBI::dbSendQuery(con,"end"); DBI::dbClearResult(rs)
        rs <- DBI::dbSendQuery(con,"detach tmp"); DBI::dbClearResult(rs)
    }

    DBI::dbDisconnect(con)
    return(NULL)
}

#' Attach all sqlite3 files from a given directory into a single file with the name of that directory
#' @param ld.in : list of directories containing merged files
#' @param tablename : name of the table to be merged. All others will be dropped.
#' @param overwrite : overwrite merged file, if exists
#' @export
create.merged.sql <- function(ld.in,tablename,overwrite=FALSE) {
  lf.in <- list.files(ld.in,full=TRUE,pattern=".sqlite3"); fn.out <- paste0(ld.in,".sqlite3")
  if(length(lf.in)==0) lf.in <- list.files(ld.in,full=TRUE,pattern=".sql3"); fn.out <- paste0(ld.in,".sql3")
  if(length(lf.in)==0) return(NULL)

  if(file.exists(fn.out) & !overwrite) return(NULL)

  baseutils::SQLite.attach(lf.in,fn.out,tablename)
  return(NULL)
}
