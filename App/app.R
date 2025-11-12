suppressPackageStartupMessages({
  library(shiny); library(DBI); library(odbc); library(dplyr)
  library(ggplot2); library(DT); library(bcrypt); library(bslib)
})

db_connect <- function() {
  dbConnect(odbc::odbc(),
            Driver   = "{ODBC Driver 17 for SQL Server}",
            Server   = Sys.getenv("SQLSERVER_HOST", unset = "localhost"),
            Database = Sys.getenv("SQLSERVER_DB",   unset = "Northwind"),
            UID      = Sys.getenv("SQLSERVER_UID",  unset = ""),
            PWD      = Sys.getenv("SQLSERVER_PWD",  unset = ""),
            Trusted_Connection = ifelse(Sys.getenv("SQLSERVER_UID","") == "", "Yes", "No"),
            timeout = 15)
}

ensure_appusers_table <- function(con) {
  if (identical(as.integer(Sys.getenv("REQUIRE_AUTH","1")), 1L)) {
    sql <- "
    IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = 'AppUsers')
    CREATE TABLE dbo.AppUsers (
      Id INT IDENTITY(1,1) PRIMARY KEY,
      Email NVARCHAR(255) NOT NULL UNIQUE,
      PasswordHash NVARCHAR(255) NOT NULL,
      CreatedAt DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME()
    );"
    DBI::dbExecute(con, sql)
  }
}

get_user_by_email <- function(con, email) {
  DBI::dbGetQuery(con, "SELECT TOP 1 * FROM dbo.AppUsers WHERE Email = ?", params = list(email))
}

create_user <- function(con, email, password) {
  hash <- bcrypt::hashpw(password)
  DBI::dbExecute(con, "INSERT INTO dbo.AppUsers (Email, PasswordHash) VALUES (?, ?)",
                 params = list(email, hash))
}

validate_login <- function(con, email, password) {
  u <- get_user_by_email(con, email)
  if (nrow(u) == 0) return(FALSE)
  bcrypt::checkpw(password, u$PasswordHash[[1]])
}

pick_col <- function(df, candidates) {
  existing <- candidates[candidates %in% names(df)]
  if (length(existing) == 0) return(NA_character_)
  existing[[1]]
}

read_invoices_exact <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM dbo.Invoices")
}

app_theme <- bs_theme(bootswatch = "flatly")

ui <- page_fluid(
  theme = app_theme,
  titlePanel("Laboratorio 3 - Northwind"),
  navset_tab(id = "tabs",
             nav_panel("Login",
                       layout_columns(
                         col_widths = c(4,4,4), NULL,
                         card(card_header("Iniciar sesión"),
                              textInput("login_email", "Correo"),
                              passwordInput("login_pwd", "Contraseña"),
                              actionButton("btn_login", "Entrar", class = "btn btn-primary"),
                              div(class="mt-2"),
                              actionButton("go_register", "Crear cuenta")
                         ), NULL
                       )
             ),
             nav_panel("Registro",
                       layout_columns(
                         col_widths = c(4,4,4), NULL,
                         card(card_header("Crear cuenta"),
                              textInput("reg_email", "Correo"),
                              passwordInput("reg_pwd1", "Contraseña"),
                              passwordInput("reg_pwd2", "Confirmar contraseña"),
                              actionButton("btn_register", "Registrar", class = "btn btn-success"),
                              div(class="mt-2"),
                              actionButton("go_login", "Ir a Login")
                         ), NULL
                       )
             ),
             nav_panel("Resumen general",
                       div(class="mb-2", textOutput("welcome")),
                       card(card_header("Filtros"),
                            layout_columns(
                              dateRangeInput("f_fecha", "Rango de fechas", start = NA, end = NA),
                              selectizeInput("f_pais", "País", choices = NULL, multiple = TRUE),
                              selectizeInput("f_cliente", "Cliente", choices = NULL, multiple = TRUE)
                            )
                       ),
                       layout_columns(
                         card(div(h4("Total de facturas"), h2(textOutput("kpi_facturas")))),
                         card(div(h4("Clientes distintos"), h2(textOutput("kpi_clientes")))),
                         card(div(h4("Valor facturado"), h2(textOutput("kpi_total")))),
                         card(div(h4("Promedio por factura"), h2(textOutput("kpi_promedio"))))
                       ),
                       layout_columns(
                         card(card_header("Top 10 países (ExtendedPrice)"), plotOutput("plot_country", height = 350)),
                         card(card_header("Ventas mensuales (ExtendedPrice)"), plotOutput("plot_month", height = 350))
                       ),
                       card(card_header("Descripción"),
                            p("Explora ventas de Northwind a partir de la vista 'dbo.Invoices'. La pestaña de Resumen muestra KPIs, total por país y evolución mensual. La pestaña de Detalle permite buscar y filtrar los registros.")
                       )
             ),
             nav_panel("Detalle de facturas",
                       card(card_header("Filtros"),
                            layout_columns(
                              dateRangeInput("f_fecha2", "Rango de fechas", start = NA, end = NA),
                              selectizeInput("f_pais2", "País", choices = NULL, multiple = TRUE),
                              selectizeInput("f_cliente2", "Cliente", choices = NULL, multiple = TRUE)
                            )
                       ),
                       card(card_header("Detalle de Invoices"), DTOutput("tbl_invoices"))
             )
  )
)

server <- function(input, output, session) {
  con <- NULL
  logged <- reactiveVal(FALSE)
  logged_email <- reactiveVal(NULL)
  invoices <- reactiveVal(NULL)
  REQUIRE_AUTH <- as.integer(Sys.getenv("REQUIRE_AUTH", "1"))
  
  observeEvent(TRUE, {
    con <<- db_connect()
    ensure_appusers_table(con)
    if (REQUIRE_AUTH == 0) {
      logged(TRUE); logged_email("demo@lab.local")
      updateTabsetPanel(session, "tabs", selected = "Resumen general")
      inv <- read_invoices_exact(con); invoices(inv); init_filters(inv)
    } else {
      updateTabsetPanel(session, "tabs", selected = "Resumen general")
    }
  }, once = TRUE)
  
  observeEvent(input$go_register, { updateTabsetPanel(session, "tabs", selected = "Registro") })
  observeEvent(input$go_login,    { updateTabsetPanel(session, "tabs", selected = "Resumen general") })
  
  observeEvent(input$btn_register, {
    req(REQUIRE_AUTH == 1)
    req(input$reg_email, input$reg_pwd1, input$reg_pwd2)
    if (input$reg_pwd1 != input$reg_pwd2) { showNotification("Las contraseñas no coinciden.", type="error"); return() }
    if (nrow(get_user_by_email(con, input$reg_email)) > 0) { showNotification("Ya existe una cuenta con ese correo.", type="warning"); return() }
    create_user(con, input$reg_email, input$reg_pwd1)
    showNotification("Cuenta creada. Ahora inicia sesión.", type="message")
    updateTabsetPanel(session, "tabs", selected = "Resumen general")
  })
  
  observeEvent(input$btn_login, {
    req(REQUIRE_AUTH == 1)
    req(input$login_email, input$login_pwd)
    ok <- validate_login(con, input$login_email, input$login_pwd)
    if (!ok) { showNotification("Credenciales inválidas.", type = "error"); return() }
    logged(TRUE); logged_email(input$login_email); updateTabsetPanel(session, "tabs", selected = "Resumen general")
    inv <- read_invoices_exact(con); invoices(inv); init_filters(inv)
  })
  
  output$welcome <- renderText({
    req(logged()); paste0("Bienvenida/o, ", logged_email(), ". Vista: dbo.Invoices (sin modificaciones).")
  })
  
  init_filters <- function(inv) {
    col_country <- pick_col(inv, c("Country","ShipCountry","Country/Region"))
    col_cust    <- pick_col(inv, c("CustomerName","Customer Name","CustomerID","Customer ID"))
    col_date    <- pick_col(inv, c("OrderDate","Order Date","ShippedDate","Shipped Date"))
    dates <- suppressWarnings(as.Date(inv[[col_date]])); rng <- range(dates, na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2])) {
      updateDateRangeInput(session, "f_fecha", start = rng[1], end = rng[2])
      updateDateRangeInput(session, "f_fecha2", start = rng[1], end = rng[2])
    }
    countries <- sort(unique(na.omit(inv[[col_country]])))
    updateSelectizeInput(session, "f_pais",  choices = countries, server = TRUE)
    updateSelectizeInput(session, "f_pais2", choices = countries, server = TRUE)
    customers <- sort(unique(na.omit(inv[[col_cust]])))
    updateSelectizeInput(session, "f_cliente",  choices = customers, server = TRUE)
    updateSelectizeInput(session, "f_cliente2", choices = customers, server = TRUE)
  }
  
  filtered_resumen <- reactive({
    req(logged(), invoices()); inv <- invoices()
    col_country <- pick_col(inv, c("Country","ShipCountry","Country/Region"))
    col_cust    <- pick_col(inv, c("CustomerName","Customer Name","CustomerID","Customer ID"))
    col_date    <- pick_col(inv, c("OrderDate","Order Date","ShippedDate","Shipped Date"))
    df <- inv
    if (!is.null(input$f_fecha) && length(input$f_fecha) == 2) {
      d1 <- as.Date(input$f_fecha[1]); d2 <- as.Date(input$f_fecha[2])
      df <- df %>% mutate(.fecha = as.Date(.data[[col_date]])) %>% filter(!is.na(.fecha), .fecha >= d1, .fecha <= d2)
    }
    if (!is.null(input$f_pais) && length(input$f_pais) > 0)    df <- df %>% filter(.data[[col_country]] %in% input$f_pais)
    if (!is.null(input$f_cliente) && length(input$f_cliente) > 0) df <- df %>% filter(.data[[col_cust]] %in% input$f_cliente)
    df
  })
  
  filtered_detalle <- reactive({
    req(logged(), invoices()); inv <- invoices()
    col_country <- pick_col(inv, c("Country","ShipCountry","Country/Region"))
    col_cust    <- pick_col(inv, c("CustomerName","Customer Name","CustomerID","Customer ID"))
    col_date    <- pick_col(inv, c("OrderDate","Order Date","ShippedDate","Shipped Date"))
    df <- inv
    if (!is.null(input$f_fecha2) && length(input$f_fecha2) == 2) {
      d1 <- as.Date(input$f_fecha2[1]); d2 <- as.Date(input$f_fecha2[2])
      df <- df %>% mutate(.fecha = as.Date(.data[[col_date]])) %>% filter(!is.na(.fecha), .fecha >= d1, .fecha <= d2)
    }
    if (!is.null(input$f_pais2) && length(input$f_pais2) > 0)    df <- df %>% filter(.data[[col_country]] %in% input$f_pais2)
    if (!is.null(input$f_cliente2) && length(input$f_cliente2) > 0) df <- df %>% filter(.data[[col_cust]] %in% input$f_cliente2)
    df
  })
  
  output$kpi_facturas <- renderText({
    df <- filtered_resumen(); col_orderid <- pick_col(df, c("OrderID","Order ID"))
    format(length(unique(df[[col_orderid]])), big.mark = ",")
  })
  output$kpi_clientes <- renderText({
    df <- filtered_resumen(); col_custid <- pick_col(df, c("CustomerID","Customer ID"))
    format(length(unique(df[[col_custid]])), big.mark = ",")
  })
  output$kpi_total <- renderText({
    df <- filtered_resumen(); col_amount <- pick_col(df, c("ExtendedPrice","Extended Price"))
    format(round(sum(as.numeric(df[[col_amount]]), na.rm = TRUE), 2), big.mark = ",")
  })
  output$kpi_promedio <- renderText({
    df <- filtered_resumen()
    col_amount <- pick_col(df, c("ExtendedPrice","Extended Price")); col_orderid <- pick_col(df, c("OrderID","Order ID"))
    total <- sum(as.numeric(df[[col_amount]]), na.rm = TRUE); nfact <- length(unique(df[[col_orderid]]))
    prom <- ifelse(nfact > 0, total / nfact, 0); format(round(prom, 2), big.mark = ",")
  })
  
  output$plot_country <- renderPlot({
    df <- filtered_resumen()
    col_country <- pick_col(df, c("Country","ShipCountry","Country/Region"))
    col_amount  <- pick_col(df, c("ExtendedPrice","Extended Price"))
    validate(need(!is.na(col_country) && !is.na(col_amount), "Faltan columnas Country/ExtendedPrice."))
    top <- df %>% group_by(.data[[col_country]]) %>%
      summarise(ventas = sum(as.numeric(.data[[col_amount]]), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(ventas)) %>% slice_head(n = 10)
    ggplot(top, aes(x = reorder(.data[[col_country]], ventas), y = ventas)) +
      geom_col() + coord_flip() + labs(x="Country", y="ExtendedPrice", title="Top 10 por país (suma de ExtendedPrice)")
  })
  
  output$plot_month <- renderPlot({
    df <- filtered_resumen()
    col_date   <- pick_col(df, c("OrderDate","Order Date","ShippedDate","Shipped Date"))
    col_amount <- pick_col(df, c("ExtendedPrice","Extended Price"))
    validate(need(!is.na(col_date) && !is.na(col_amount), "Faltan columnas de fecha/ExtendedPrice."))
    monthly <- df %>% mutate(.fecha = as.Date(.data[[col_date]])) %>% filter(!is.na(.fecha)) %>%
      group_by(Mes = as.Date(cut(.fecha, "month"))) %>%
      summarise(ventas = sum(as.numeric(.data[[col_amount]]), na.rm = TRUE), .groups = "drop")
    ggplot(monthly, aes(x = Mes, y = ventas)) + geom_line() + labs(x="Mes", y="ExtendedPrice", title="Ventas mensuales (ExtendedPrice)")
  })
  
  output$tbl_invoices <- renderDT({
    df <- filtered_detalle()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  session$onSessionEnded(function() { try(DBI::dbDisconnect(con), silent = TRUE) })
}

shinyApp(ui, server)
