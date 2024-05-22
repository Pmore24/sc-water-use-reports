
#' @title Import the tables from an access database.
#' @description Before setting up the ODBC connection in R studio, it is
#' required to have MS Access driver (.mdb, .accdb) installed.
#' A data source should be setup for this driver which will contain the access
#' database to be connected to R Studio. Refer to vignette, "Connecting to an Access Database"

#' @param dsn is the connection between R studio and MS Access driver.
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlTables
#' @importFrom RODBC sqlFetch
#' @importFrom RODBC odbcClose
#' @return A list object containing the tables from the database.
#' @export
# dsn= "DHEC_database"
read_access_data <- function(dsn= dsn) {
   ODBC_channel <- RODBC::odbcConnect(dsn=dsn)
   table_names <-
      RODBC::sqlTables(channel=ODBC_channel, tableType = "TABLE")$TABLE_NAME
   tables <- lapply(table_names,function(x){RODBC::sqlFetch(ODBC_channel, x)})
   names(tables) <- table_names
   RODBC::odbcClose(ODBC_channel)
   tables
}

# install.packages("RODBC")
# library(RODBC)
# ODBC_channel <- odbcConnect("DHEC_database")
