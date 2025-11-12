# Muestra algunas filas de dbo.Invoices para confirmar datos
library(DBI)
library(odbc)

con <- dbConnect(odbc::odbc(),
                 Driver = "{ODBC Driver 17 for SQL Server}",
                 Server = Sys.getenv("SQLSERVER_HOST", unset="localhost\\SQLEXPRESS"),
                 Database = Sys.getenv("SQLSERVER_DB", unset="Northwind"),
                 UID = Sys.getenv("SQLSERVER_UID", unset=""),
                 PWD = Sys.getenv("SQLSERVER_PWD", unset=""),
                 Trusted_Connection = ifelse(Sys.getenv("SQLSERVER_UID")=="","Yes","No")
)

head_inv <- DBI::dbGetQuery(con, "SELECT TOP 20 * FROM dbo.Invoices ORDER BY 1")
print(head_inv)

DBI::dbDisconnect(con)
