# Packages load || install & load -----------------------------------------
r.exp <- expression(require(x, character.only = TRUE, quietly = TRUE))
sapply(c("RODBC"), function(x) eval(r.exp) ||
           {install.packages(x); eval(r.exp)})
rm(r.exp)

sqlQueryDB <- function(query, database = "datamart_tableau",
                       server = "oro-dwh-01", manual = FALSE) {
    
    
    # Sys.setlocale("LC_ALL","Russian")
    logkey <- readLines("zlogkey.txt")
    stopifnot(all(vapply(list(database, server), length, numeric(1)) == 1))
    
    if (manual == TRUE) {
        query <- readline("Manualy enter SQL query: ")
    }
    
    acc_args <- list(driver   = "driver={SQL Server}",
                     server   = paste0("server=", server),
                     database = paste0("database=", database),
                     uid      = "uid=predict",
                     pwd      = paste0("pwd=", logkey))
    
    arg_str  <- do.call(paste0, list(acc_args, collapse = ";"))
    con      <- odbcDriverConnect(connection = arg_str)
    on.exit(odbcClose(con), add = TRUE)
    sql_data <- NULL
    try(sql_data <- sqlQuery(con, query, stringsAsFactors = FALSE))
    return(sql_data)
}
