#load(file.choose())

library(shiny)
library(dplyr)
library(DT)
require(mixlm)
library(randomForest)
library(tree)
library(MASS)
library(rpart)
library(knitr)

#knit("data/accidentalidad.Rmd")

load("data/accidentalidad.RData")
#map
library(leaflet)
library(leaflet.extras)

# Server ----
# load data
#barrios <- paste0(readLines("data/barrios_riesgo.json",encoding="UTF-8",warn = FALSE), collapse = "")
altos <- paste0(readLines("data/altos.json",encoding="UTF-8",warn = FALSE), collapse = "")
bajos <- paste0(readLines("data/bajos.json",encoding="UTF-8",warn = FALSE), collapse = "")
medios <- paste0(readLines("data/medios.json",encoding="UTF-8",warn = FALSE), collapse = "")
desconocidos <- paste0(readLines("data/desconocidos.json",encoding="UTF-8",warn = FALSE), collapse = "")
data_accidentes <- read.csv(file = "data/incidentes_medellin.csv", fileEncoding = "utf-8");
resumen_accidentes <- read.csv(file = "data/Resumen-accidentalidad.csv", sep = ";")
meses <- read.csv(file = "data/meses.csv", sep = ";")
fechas_importantes <- read.csv(file = "data/fechas_importantes_2020.csv", sep=";")


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
      dplyr::select("PERIODO", "MES", "DIA", "COMUNA", "BARRIO", "Riesgo",
             "Suma.de.Numero.de.accidentes", "Suma.de.Numero.de.Heridos", "Suma.de.Número.de.solo.daños",
             "Suma.de.Número.de.muertos")
    
    sketch <- htmltools::withTags(
      table(
        tableHeader(c("Año", "Mes", "Día", "Comuna", "Barrio", "Riesgo",  
                      "Número accidentes",  "Accidentes con heridos", "Accidentes con solo daños", 
                      "Accidentes con muertos")),
        tableFooter(c("Subtotal","","","","","","",0,0,0,0))
      ))
    
    DT::datatable(
      data,
      container = sketch,
      extensions = "Scroller",
      filter = "top", options = list(
        deferRender = TRUE,
        searching = TRUE,
        scrollX = TRUE,
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
          "$( api.column(10).footer()).html(",
          "api.column(10).data().reduce( function ( a, b ) {",
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
  #Modelo1
  modelo1 <- rpart(`Numero de accidentes`~MES+DIA+DIA_NOMBRE+COMUNA+puente_festivo+puente_reyes+semana_santa+feria_flores+puente_semana_santa+festivo_entre_semana+no_festivo_importante+Riesgo, data=accidentalidad, method = 'anova')
 
  #Modelo2
  modelo2<-lm(`Accidente con solo daños`~MES+DIA+DIA_NOMBRE+COMUNA+puente_festivo+puente_reyes+semana_santa+feria_flores+puente_semana_santa+festivo_entre_semana+no_festivo_importante+Riesgo+`Numero de accidentes`, data=accidentalidad)
 
  #Creando variable accidentes_heridos_o_muertos
  accidentalidad<-accidentalidad %>%
   dplyr::select(puente_festivo,puente_reyes,semana_santa,feria_flores,puente_semana_santa,festivo_entre_semana,no_festivo_importante,`Accidente de muertos`,MES,DIA,DIA_NOMBRE,COMUNA,Riesgo,`Numero de accidentes`,PERIODO,`Accidentes con Heridos`) %>%
   mutate(accidentalidad,accidentes_heridos_o_muertos=`Accidente de muertos`+`Accidentes con Heridos`)
 
  #Modelo 3
  modelo3<-lm(accidentes_heridos_o_muertos~MES+DIA+DIA_NOMBRE+COMUNA+puente_festivo+puente_reyes+semana_santa+feria_flores+puente_semana_santa+festivo_entre_semana+no_festivo_importante+Riesgo+`Numero de accidentes`, data=accidentalidad)
  
  #function(input, output, session) {
  
  output$secondSelection <- renderUI({
    selectInput("SelectedBarrio", h4("Seleccione el barrio"), choices = unique(resumen_accidentes %>% 
                                                                                  filter(COMUNA == input$SelectedComuna) %>%
                                                                                  dplyr::select("BARRIO")))
  })
  
  output$value_prediction <- eventReactive(input$prediction, {
    COMUNA <- input$SelectedComuna
    
    Riesgo <- unique(resumen_accidentes %>% 
                       filter(COMUNA == input$SelectedComuna & BARRIO == input$SelectedBarrio) %>%
                       dplyr::select("Riesgo"))
    
    
    if(input$SelectedOption=="Mes"){
      DATE <- paste("Mes:", input$SelectedMonth, sep=" ")
      get_month <- meses  %>% 
          filter(nombre_mes==input$SelectedMonth) %>%
          dplyr::select("fecha_inicial", "fecha_final")
      startDate <- as.POSIXct(format(as.Date(get_month[[1]][1], origin = "1970-01-01"), "%Y-%m-%d"))
      endDate <- as.POSIXct(format(as.Date(get_month[[2]][1], origin = "1970-01-01"), "%Y-%m-%d"))
      all_dates = seq(startDate, endDate, by="day")

      for (j in 1:length(all_dates)) {
        fechas <- fechas_importantes %>% 
          filter(FECHA==all_dates[j]) %>%
          dplyr::select("puente_festivo", "puente_reyes", "semana_santa", "puente_semana_santa", "feria_flores", "festivo_entre_semana", "no_festivo_importante")
        
        puente_festivo <- toString(fechas[[1]][1])
        puente_reyes <- toString(fechas[[2]][1])
        semana_santa <- toString(fechas[[3]][1])
        puente_semana_santa <- toString(fechas[[4]][1])
        feria_flores <- toString(fechas[[5]][1])
        festivo_entre_semana <- toString(fechas[[6]][1])
        no_festivo_importante <- toString(fechas[[7]][1])
        date_split <- strsplit(toString(all_dates[j]), '-')[[1]]
        
        MES <-date_split[2]
        if (strsplit(MES, '0')[[1]][1]=="") {
          MES <- strsplit(MES, '0')[[1]][2]
        }
        DIA <-date_split[3]
        if (strsplit(DIA, '0')[[1]][1]=="") {
          DIA <- strsplit(DIA, '0')[[1]][2]
        }
        MES <- as.integer(MES)
        DIA <- as.integer(DIA)
        DIA_NOMBRE <- toupper(weekdays(as.Date(all_dates[j])))
        if (j==1) {
          df_model1 <- data.frame(MES, DIA, DIA_NOMBRE, COMUNA,puente_festivo, puente_reyes,semana_santa, feria_flores, puente_semana_santa, festivo_entre_semana, no_festivo_importante,Riesgo)
        } else {
          add_row <- data.frame(MES, DIA, DIA_NOMBRE, COMUNA,puente_festivo, puente_reyes,semana_santa, feria_flores, puente_semana_santa, festivo_entre_semana, no_festivo_importante,Riesgo)
          df_model1 <- rbind(df_model1, add_row)
        }
      }
    }
    
    
    if(input$SelectedOption=="Semana"){
      DATE <-  paste(toString(input$datesWeek[1]), toString(input$datesWeek[2]), sep="/")
      startDate <- as.POSIXct(format(as.Date(input$datesWeek[1], origin = "1970-01-01"), "%Y-%m-%d"))
      endDate <- as.POSIXct(format(as.Date(input$datesWeek[2], origin = "1970-01-01"), "%Y-%m-%d"))
      all_dates = seq(startDate, endDate, by="day")
      
      for (j in 1:length(all_dates)) {
        fechas <- fechas_importantes %>% 
          filter(FECHA==all_dates[j]) %>%
          dplyr::select("puente_festivo", "puente_reyes", "semana_santa", "puente_semana_santa", "feria_flores", "festivo_entre_semana", "no_festivo_importante")
        puente_festivo <- toString(fechas[[1]][1])
        puente_reyes <- toString(fechas[[2]][1])
        semana_santa <- toString(fechas[[3]][1])
        puente_semana_santa <- toString(fechas[[4]][1])
        feria_flores <- toString(fechas[[5]][1])
        festivo_entre_semana <- toString(fechas[[6]][1])
        no_festivo_importante <- toString(fechas[[7]][1])
        date_split <- strsplit(toString(all_dates[j]), '-')[[1]]
        
        MES <-date_split[2]
        if (strsplit(MES, '0')[[1]][1]=="") {
          MES <- strsplit(MES, '0')[[1]][2]
        }
        DIA <-date_split[3]
        if (strsplit(DIA, '0')[[1]][1]=="") {
          DIA <- strsplit(DIA, '0')[[1]][2]
        }
        MES <- as.integer(MES)
        DIA <- as.integer(DIA)
        DIA_NOMBRE <- toupper(weekdays(as.Date(all_dates[j])))
        
        if (j==1) {
          df_model1 <- data.frame(MES, DIA, DIA_NOMBRE, COMUNA,puente_festivo, puente_reyes,semana_santa, feria_flores, puente_semana_santa, festivo_entre_semana, no_festivo_importante,Riesgo)
        } else {
          add_row <- data.frame(MES, DIA, DIA_NOMBRE, COMUNA,puente_festivo, puente_reyes,semana_santa, feria_flores, puente_semana_santa, festivo_entre_semana, no_festivo_importante,Riesgo)
          df_model1 <- rbind(df_model1, add_row)
        }
      }
    }
    
    
    if(input$SelectedOption=="Dia"){
      DATE <- toString(input$dateDay)
      date_split <- strsplit(toString(input$dateDay), '-')[[1]]
      MES <- toString(date_split[2])
      if (strsplit(MES, '0')[[1]][1]=="") {
        MES <- strsplit(MES, '0')[[1]][2]
      }
      DIA <- toString(date_split[3])
      if (strsplit(DIA, '0')[[1]][1]=="") {
        DIA <- strsplit(DIA, '0')[[1]][2]
      }
      MES <- as.integer(MES)
      DIA <- as.integer(DIA)
      DIA_NOMBRE<-  toupper(weekdays(as.Date(input$dateDay)))
      fecha <- fechas_importantes %>% 
        filter(FECHA==input$dateDay) %>%
        dplyr::select("puente_festivo", "puente_reyes", "semana_santa", "puente_semana_santa", "feria_flores", "festivo_entre_semana", "no_festivo_importante")
      puente_festivo <- toString(fecha[[1]][1])
      puente_reyes <- toString(fecha[[2]][1])
      semana_santa <- toString(fecha[[3]][1])
      puente_semana_santa <- toString(fecha[[4]][1])
      feria_flores <- toString(fecha[[5]][1])
      festivo_entre_semana <- toString(fecha[[6]][1])
      no_festivo_importante <- toString(fecha[[7]][1])
      
      df_model1 <- data.frame(MES, DIA, DIA_NOMBRE, COMUNA,puente_festivo, puente_reyes,semana_santa, feria_flores, puente_semana_santa, festivo_entre_semana, no_festivo_importante,Riesgo)
      }
    
    
    model1<-predict(modelo1, df_model1)
    df_model2 <- cbind(df_model1,`Numero de accidentes`=model1)
    model2<-predict(modelo2, df_model2)
    model3<-predict(modelo3, df_model2)
    m1(DATE)
    m2(round(sum(model1),0))
    m3(round(sum(model2),0))
    m4(round(sum(model3),0))
    
    textoSalida <- paste(DATE, "Numero de accidentes:", sum(model1), "Accidente con solo daños: ", sum(model2), "Accidentes con heridos o muertos:", sum(model3), sep=" ")
    
    })
  
  m1 <- reactiveVal(0)
  m2 <- reactiveVal(0)
  m3 <- reactiveVal(0)
  m4 <- reactiveVal(0)
  
  output$outDate <- renderValueBox({
    valueBox(
      m1(), "Fecha", icon = icon("calendar-day"),
      color = "blue"
    )
  })
  output$outModel1 <- renderValueBox({
    valueBox(
      m2(), "Numero de accidentes", icon = icon("sort-numeric-up"),
      color = "yellow"
    )
  })
  output$outModel2 <- renderValueBox({
    valueBox(
      m3(), "Accidente con solo daños", icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  output$outModel3 <- renderValueBox({
    valueBox(
      m4(), "Accidentes con heridos o muertos", icon = icon("dizzy"),
      color = "red"
    )
  })
  

  # Map
  

  output$map_group <- renderLeaflet({
    pal <- colorNumeric("viridis", NULL)
    leaflet() %>%
      setView(-75.595178, 6.26, 12) %>%
      addTiles() %>%
      addGeoJSONv2(
        altos,
        labelProperty = "NOMBRE",
        popupProperty = propstoHTMLTable(
          props = c("NOMBRE", "NOMBRE_COM", "RIESGO"),
          table.attrs = list(class = "table table-striped table-bordered"),
          drop.na = TRUE
        ),
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.3,
        fillColor = "red",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.5,
          opacity = 1,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        pathOptions = pathOptions(showMeasurements = TRUE),
        
      ) %>%
      addGeoJSONv2(
        medios,
        labelProperty = "NOMBRE",
        popupProperty = propstoHTMLTable(
          props = c("NOMBRE", "NOMBRE_COM", "RIESGO"),
          table.attrs = list(class = "table table-striped table-bordered"),
          drop.na = TRUE
        ),
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.5,
        fillColor = "orange",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.5,
          opacity = 1,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        pathOptions = pathOptions(showMeasurements = TRUE),
        
      ) %>%
      addGeoJSONv2(
        bajos,
        labelProperty = "NOMBRE",
        popupProperty = propstoHTMLTable(
          props = c("NOMBRE", "NOMBRE_COM", "RIESGO"),
          table.attrs = list(class = "table table-striped table-bordered"),
          drop.na = TRUE
        ),
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.2,
        fillColor = "yellow",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.5,
          opacity = 1,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        pathOptions = pathOptions(showMeasurements = TRUE),
        
      ) %>%
      addGeoJSONv2(
        desconocidos,
        labelProperty = "NOMBRE",
        popupProperty = propstoHTMLTable(
          props = c("NOMBRE", "NOMBRE_COM", "RIESGO"),
          table.attrs = list(class = "table table-striped table-bordered"),
          drop.na = TRUE
        ),
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.3,
        fillColor = "gray",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.5,
          opacity = 1,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        pathOptions = pathOptions(showMeasurements = TRUE),
        
      )
  })
  output$click_info<- renderText({
    e <- input$map_group_geojson_click
    if(is.null(e)) 'Seleccione un barrio para mas detalles' else toString(e)
  })  
}