library(shiny)
library(dplyr)

source("ui.R")

# Server ----

server <- function(input, output, session) {
  
  # Dialog Modal
  showModal(modalDialog(
    includeHTML("intro.html"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  data_accidentes <- read.csv(file = "data/incidentes_medellin.csv", fileEncoding = "utf-8");
  
  # Apply filter to visualization
  createTable <- eventReactive(input$filter, {
    dateStart <- format(as.Date(input$dates[1], origin = "1970-01-01"), "%Y/%m/%d")
    dateFinish <- format(as.Date(input$dates[2] + 1, origin = "1970-01-01"), "%Y/%m/%d")
    data <- data_accidentes %>% 
      filter(FECHA >= dateStart & FECHA <= dateFinish) %>% 
      select(BARRIO, COMUNA, FECHA, LATITUD, LONGITUD, GRAVEDAD)
    
    DT::datatable(
      data,
      extensions = "Scroller",
      filter = "top", options = list(
        deferRender = TRUE,
        searching = TRUE,
        filter = list(position = "top", clear = FALSE),
        pageLength = 10,
        sDom  = '<"top">lrt<"bottom">ip',
        language = list(
          url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      rownames = FALSE,
      colnames = c("Barrio", "Comuna", "Fecha", "Latitud", "Longitud", "Gravedad")
    )
  })
  
  output$data <- DT::renderDataTable({
    createTable()
  })
  
  # Predict
  
  # Map
}

# App ----

shinyApp(ui, server)