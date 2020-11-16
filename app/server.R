library(shiny)
library(dplyr)
library(DT)

#map
library(leaflet)
library(leaflet.extras)

# Server ----
# load data
barrios <- paste0(readLines("data/Barrio_Vereda.json",encoding="UTF-8",warn = FALSE), collapse = "")
data_accidentes <- read.csv(file = "data/incidentes_medellin.csv", fileEncoding = "utf-8");
resumen_accidentes <- read.csv(file = "data/Resumen-accidentalidad.csv", sep = ";")

# Add date complete to data
resumen_accidentes$FECHA <- (ISOdate(resumen_accidentes$PERIODO, resumen_accidentes$MES, 
                                     resumen_accidentes$DIA))

resumen_accidentes$Es.fecha.importante <- as.factor(ifelse(resumen_accidentes$Es.fecha.importante 
                                                           == 1, 'Sí', 'No'))

resumen_accidentes$Riesgo <- as.factor(resumen_accidentes$Riesgo)

function(input, output, session) {
  
  # Dialog Modal
  showModal(modalDialog(
    includeHTML("intro.html"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  # Apply filter to visualization
  createTable <- eventReactive(input$filter, {
    dateStart <- format(as.Date(input$dates[1], origin = "1970-01-01"), "%Y-%m-%d")
    dateFinish <- format(as.Date(input$dates[2] + 1, origin = "1970-01-01"), "%Y-%m-%d")
    data <- resumen_accidentes %>% 
      filter(FECHA >= dateStart & FECHA <= dateFinish) %>%
      select("PERIODO", "MES", "DIA", "COMUNA", "Riesgo", "Es.fecha.importante",
             "Numero.de.accidentes", "Accidentes.con.Heridos", "Accidentes.con.solo.daños",
             "Accidentes.con.muertos")
    
    sketch <- htmltools::withTags(
      table(
        tableHeader(c("Año", "Mes", "Día", "Comuna", "Riesgo", "Fecha importante", 
                      "Número accidentes",  "Accidentes con heridos", "Accidentes con solo daños", 
                      "Accidentes con muertos")),
        tableFooter(c("Subtotal","","","","","",0,0,0,0))
      ))
    
    DT::datatable(
      data,
      container = sketch,
      extensions = "Scroller",
      filter = "top", options = list(
        deferRender = TRUE,
        searching = TRUE,
        filter = list(position = "top", clear = FALSE),
        pageLength = 10,
        sDom  = '<"top">lrt<"bottom">ip',
        language = list(
          url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        ),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        footerCallback = JS(
          "function( tfoot, data, start, end, display ) {",
          "var api = this.api(), data;",
          "$( api.column(6).footer()).html(",
          "api.column(6).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(7).footer()).html(",
          "api.column(7).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(8).footer()).html(",
          "api.column(8).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(9).footer()).html(",
          "api.column(9).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "}")
      ),
      rownames = FALSE
    )
  })
  
  
  output$data <- DT::renderDataTable({
    createTable()
  })
  
  # Predict
  
  # Map
  
  output$map_group <- renderLeaflet({
    pal <- colorNumeric("viridis", NULL)
    
    
    
    
    leaflet() %>%
      setView(-75.595178, 6.26, 12) %>%
      addTiles() %>%
      addGeoJSONChoropleth(barrios,
                           #valueProperty = "SHAPEAREA",
                           valueProperty = JS(
                             'function(feature){
                                         var props = feature.properties;
                                         return Math.log(props.SHAPEAREA);
                                      }'
                           ),
                           labelProperty = "NOMBRE",
                           scale = c("YELLOW", "red"),
                           popupProperty = propstoHTMLTable(
                             props = c("NOMBRE", "CODIGO", "NOMBRE_COM"),
                             table.attrs = list(class = "table table-striped table-bordered"),
                             drop.na = TRUE
                           ),
                           #stroke
                           color = "#ffffff", weight = 1, fillOpacity = 0.3,
                           highlightOptions = highlightOptions(
                             weight = 2, color = "#000000",
                             fillOpacity = 0.5, opacity = 1,
                             bringToFront = TRUE, sendToBack = TRUE),
                           pathOptions = pathOptions(
                             showMeasurements = TRUE),
                           legendOptions = legendOptions(
                             title = "log(area (m2))",
                             numberFormatOptions = list(
                               style = "decimal",
                               maximumFractionDigits = 0
                             )
                           ),
      )
  })
  output$click_info<- renderText({
    e <- input$map_group_geojson_click
    if(is.null(e)) 'Seleccione un barrio para mas detalles' else toString(e)
  })  
}