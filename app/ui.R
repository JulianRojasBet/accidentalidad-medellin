library(shiny)
library(shinydashboard)
library(rintrojs)

library(leaflet)


barrios_riesgo <- read.csv(file = "data/barrios_riesgo.csv",fileEncoding = "utf-8", sep = ",")

# dropdown ----
dropdownMenu <- 
  dropdownMenu(
    type = "messages", headerText = NULL,
     messageItem(
       from = "Github",
       message = "Repositorio de la aplicacion",
       icon = icon("code-branch"),
       href = "https://github.com/JulianRojasBet/accidentalidad-medellin"
     ),
     messageItem(
       from = "Nuevo usuario",
       message = "Mira el video promocional",
       icon = icon("video"),
       time = "13:45"
     ),
     messageItem(
       from = "Equipo de desarrollo",
       message = "La nueva version esta lista",
       icon = icon("feather-alt"),
       time = "2020-1-01"
     )
  )


# header ----
header <- 
  dashboardHeader(title = "Accidentalidad Medellin", titleWidth = 245,
                  dropdownMenu)

# sidebar ----
sidebar <- dashboardSidebar(
  width=245,
  sidebarMenu(
    menuItem("Visualizacion", tabName = "visualization", icon = icon("database")),
    menuItem("Prediccion", tabName = "prediction", icon = icon("brain")),
    menuItem("Agrupamiento", tabName = "grouping", icon = icon("globe-americas"), 
             badgeLabel = "nuevo", badgeColor = "green")
  )
)

# box visualization ----
  visualization_box <- 
    box(title = "Visualizacion de datos historicos",
     status = "success", solidHeader = TRUE, width = 12,
     fluidRow(
       column(12, offset = 0,
          div(style="text-align: center; width: 50%; display: block; 
            margin-left: auto; margin-right: auto;",
            dateRangeInput(
              "dates",
               label = h4("Seleccione una ventana de tiempo"),
               start = "2014-01-01", end = "2018-12-31",
               min = "2014-01-01", max = "2018-12-31",
               language = "es", format = "dd/MM/yyyy",
               separator = " - "
            ),
            actionButton('filter', 'Aplicar filtro', icon = icon("filter"),
                         style = "color: white", class = "btn btn-success")
          )
        )
      ),
     fluidRow(
       column(12, offset = 0,
          div(style="text-align: center; width: 95%; display: block; 
            margin-left: auto; margin-right: auto; margin-top: 30px;",
            introBox(
              DT::dataTableOutput("data"),
              data.step = 2,
              data.intro = "Search for the Official Official Game ID with filters"
            )
          )
        )
      )
    )
  
# box prediction ---
  prediction_box <-
    box(title="Predicción",
      status = "success", solidHeader = TRUE, width = 12,
      fluidRow(
        column(6, offset = 0,
               div(style="text-align: center; width: 50%; display: block; 
            margin-left: auto; margin-right: auto; margin-top: 30px;",
            selectInput("SelectedComuna",
               label = h4("Seleccione la comuna"),
               choices = c(unique(barrios_riesgo[["COMUNA"]])),
               selected = "Comuna"
            )
          )
        ),
        column(6, offset = 0,
               div(style="text-align: center; width: 50%; display: block; 
            margin-left: auto; margin-right: auto; margin-top: 30px;",
                   uiOutput("secondSelection")
               )
        ),
        column(6, offset = 0,
               div(style="text-align: center; width: 50%; display: block; 
            margin-left: auto; margin-right: auto; margin-top: 30px;",
                   selectInput("SelectedOption",
                               label = h4("Seleccione el tipo de visualización"),
                               choices = c("Mes" = "Mes", "Semana" = "Semana", "Día" = "Dia"),
                               selected = "Mes"
                   )
               )
        ),
          column(6, offset = 0,
                 div(style="text-align: center; width: 50%; display: block; 
              margin-left: auto; margin-right: auto; margin-top: 30px;",
             conditionalPanel(condition="input.SelectedOption=='Mes'",
                              selectInput("SelectedMonth",
                                          label = h4("Seleccione el tipo de visualización"),
                                          choices = list("Enero","Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                                          selected = "Mes"
                              )
             ),     
             conditionalPanel(condition="input.SelectedOption=='Semana'",
                    dateRangeInput(
                      inputId="datesWeek",
                      label = h4("Seleccione el primer y ultimo día de la semana que desea visualizar"),
                      start = "2020-01-01", end = "2020-12-31",
                      min = "2020-01-01", max = "2020-12-31",
                      language = "es", format = "dd/MM/yyyy",
                      separator = " - "
             )
             ),
             conditionalPanel(condition="input.SelectedOption=='Dia'",
                              dateInput(inputId="dateDay", label = h4("Seleccione el día que desea visualizar"), value = "2020-01-01"),
             ),  
           )
        ),
        column(12, offset = 0,
               div(style="text-align: center; width: 50%; display: block; 
            margin-left: auto; margin-right: auto; margin-top: 30px;",
                   actionButton('prediction', 'Predecir', icon = icon("filter"),
                                style = "color: white", class = "btn btn-success"),
               )
        ),

      ),
      fluidRow(
        #column(12, offset = 0,
        #      div(style="text-align: center; width: 95%; display: block; 
        #    margin-left: auto; margin-right: auto; margin-top: 60px;",
        div(style="visibility:hidden", verbatimTextOutput("value_prediction")),
        column(12,  style="margin-top: 10px; display: block; margin-left: 115px;",valueBoxOutput("outDate", width=4),
               valueBoxOutput("outModel1", width=2),
               valueBoxOutput("outModel2", width=2),
               valueBoxOutput("outModel3", width=2)),
      )
    )
  
#box map group
  map_goup_box <- box(title="Mapa con agrupamiento de los barrios de Medellin",
                      status = "success", solidHeader = TRUE, width = 12,
                        fluidRow(
                          column(12,offset = 0,
                                 div(style="width: 95%;margin-left: auto; margin-right: auto;",
                          leafletOutput("map_group",height = "75vh"),
                          #verbatimTextOutput("click_info"),
                                 )
                          )
                        )
                      )

 
# body ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "visualization", fluidRow(visualization_box)),
    tabItem(tabName = "prediction", fluidRow(prediction_box)),
    tabItem(tabName = "grouping", fluidRow(map_goup_box))
  )
)
  
# ui element
dashboardPage(header, sidebar, body, skin = "green")