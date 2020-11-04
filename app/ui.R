library(shinydashboard)
library(rintrojs)


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
          div(style="text-align: center; width: 80%; display: block; 
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

 
# body ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "visualization", fluidRow(visualization_box)),
    tabItem(tabName = "prediction", fluidRow()),
    tabItem(tabName = "grouping", fluidRow())
  )
)

# ui.R ----
ui <- dashboardPage(header, sidebar, body, skin = "green")