# Verifica las columnas reales de dbo.Invoices en tu instancia
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

inv <- DBI::dbGetQuery(con, "SELECT TOP 1 * FROM dbo.Invoices")
cat("Columnas reales de dbo.Invoices:\n")
print(names(inv))

DBI::dbDisconnect(con)
