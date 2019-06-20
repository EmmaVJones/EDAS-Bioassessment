

# Creative solution to use RStudio 1.2+ with 32 bit version of R in background
# in order to connect to 32bit version of Access databases

#> sessionInfo()
#R version 3.5.2 (2018-12-20)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 7 x64 (build 7601) Service Pack 1

#Matrix products: default

#locale:
#  [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                           LC_TIME=English_United States.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] svSocket_0.9-57 odbc_1.1.6     

#loaded via a namespace (and not attached):
#  [1] bit_1.1-14      compiler_3.5.2  hms_0.4.2       DBI_1.0.0       tools_3.5.2     svMisc_1.1.0    Rcpp_1.0.0      bit64_0.9-7     blob_1.1.1      pkgconfig_2.0.2 rlang_0.3.1     tcltk_3.5.2    

library(RODBC)
library(svSocket)


access_query_32_EVJ <- function(db_path, db_table, table_out ) {
  
  # Emma Jones adaptation (5/6/2019) of manotheshark Stack Overflow solution
  # https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window
  
  # variables to make values uniform
  sock_port <- 8642L
  sock_con <- "sv_con"
  ODBC_con <- "a32_con"
  #db_path <- "data/EDASxp_Family_090117.accdb"
  
  if (file.exists(db_path)) {
    
    # build ODBC string
    ODBC_str <- local({
      s <- list()
      s$path <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db_path)))
      s$driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
      s$threads <- "Threads=4"
      s$buffer <- "MaxBufferSize=4096"
      s$timeout <- "PageTimeout=5"
      paste(s, collapse=";")
    })
    
    # start socket server to transfer data to 32 bit session
    startSocketServer(port=sock_port, server.name="access_query_32", local=TRUE)
    
    # build expression to pass to 32 bit R session
    expr <- "library(svSocket)"
    expr <- c(expr, "library(RODBC)")
    expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC_con, ODBC_str))
    expr <- c(expr, sprintf("if('%1$s' %%in%% sqlTables(%2$s)$TABLE_NAME) {%1$s <- sqlFetch(%2$s, '%1$s')} else {%1$s <- 'table %1$s not found'}", db_table, ODBC_con))
    expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock_con, sock_port))
    expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, table_out, db_table))
    expr <- c(expr, "odbcCloseAll()")
    expr <- c(expr, sprintf("close(%s)", sock_con))
    expr <- paste(expr, collapse=";")
    
    # launch 32 bit R session and run expressions
    prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
    system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)
    
    # stop socket server
    stopSocketServer(port=sock_port)
    
    # display table fields
    message("retrieved: ", table_out, " - ", paste(colnames(get(table_out)), collapse=", "))
  } else {
    warning("database not found: ", db_path)
  }
}


# Example use:
#access_query_32_EVJ(db_path = "data/EDASxp_Family_090117.accdb", # where is your database located?
#                    db_table = "SCIQuery", # Name of table you want to query inside accesss database
#                    table_out = "SCIQuery" # object name you want to save query results
#                    ) 
  
