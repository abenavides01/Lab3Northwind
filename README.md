# Lab3Northwind
Interactive App, it consume data from the Invoices View using R. Also it generate graphics, interactive tables, descriptive labels, so that view relevant information about invoices.

## Dependencies
- R packages: `shiny`, `DBI`, `odbc`, `dplyr`, `ggplot2`, `DT`, `bcrypt`, `bslib`.
- **SQL Server** (local instance) with **Northwind** database and **`dbo.Invoices`** view.


## App Structure
```
Laboratorio3/
├─ App/
│  └─ app.R
├─ Scripts/
│  ├─ 01_check_invoices_cols.R
│  └─ 02_sample_rows.R
└─ Files/
   └─ sql_setup.sql
```

## Install
```r
install.packages(c("shiny","DBI","odbc","dplyr","ggplot2","DT","bcrypt","bslib"))
```

## How to run
1. Configure connection (create a `.Renviron` file at the project root)
2. Restart RStudio.
3. Start the app:
  ```r
  setwd("C:/Minería de Datos/Laboratorio3/App")
  shiny::runApp("app.R", launch.browser = TRUE)
  ```