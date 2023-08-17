box::use(
  shiny[
    navbarPage,
    tabPanel,
    fluidRow,
    column,
    sidebarLayout,
    sidebarPanel,
    helpText,
    checkboxGroupInput,
    mainPanel,
    conditionalPanel,
    br,
    p,
    tags,
    tabsetPanel,
    radioButtons
  ],
  leaflet[leafletOutput],
  plotly[plotlyOutput],
  DT[dataTableOutput],
  htmltools[includeMarkdown],
  shinyWidgets[checkboxGroupButtons]
)
box::use(
  app/data/data[prediction],
  app/views/mod_historical_fire_map,
  app/views/mod_fire_risk_map,
  app/views/mod_predicted_causes
)
# User interface ----
ui <- fluidPage(
  navbarPage("VICfire",
    id = "main",
    tabPanel(
      "Historical fire map",
      mod_historical_fire_map$ui("historical_fire_map")
    ),
    tabPanel(
      "2019-2020 Predicted causes",
      mod_predicted_causes$ui("predicted_causes")
    ),
    tabPanel(
      "Fire risk map",
      mod_fire_risk_map$ui("fire_risk_map")
    ),
    tabPanel("Data", dataTableOutput("data")),
    tabPanel("Information", includeMarkdown("readme.md"))
  )
)
