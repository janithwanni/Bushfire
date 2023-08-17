box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    observe,
    fluidRow,
    column,
    sidebarLayout,
    sidebarPanel,
    helpText,
    checkboxGroupInput,
    mainPanel,
    br,
    tags,
    p,
    conditionalPanel,
    tabsetPanel,
    tabPanel
  ],
  leaflet[
    leaflet,
    addTiles,
    addLegend,
    setView,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    clearMarkers,
    addCircleMarkers
  ],
  leafem[addMouseCoordinates],
  plotly[plotlyOutput],
  shinyWidgets[checkboxGroupButtons],
  magrittr[`%>%`],
  dplyr[filter]
)

box::use(
  app/data/data[prediction],
  app/logic/color_palettes[pal1]
)

ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(8,
      align = "left",
      sidebarLayout(
        sidebarPanel(
          helpText("Predicted causes for fire ignition points over Oct 2019 through Mar 2020."),
          checkboxGroupButtons(ns("month1"),
            label = "Choose month:",
            choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
            individual = TRUE, justified = FALSE, selected = c("Dec"),
            width = "100%"
          ),
          checkboxGroupInput(ns("reason1"),
            label = "Choose reason:",
            choices = levels(factor(prediction$new_cause)),
            selected = c("arson", "lightning", "accident")
          )
        ),
        mainPanel(leafletOutput(outputId = ns("map2"), height = 587))
      )
    ),
    column(
      4,
      conditionalPanel(
        # TODO: rename condition2 to a more descriptive name
        condition = "output.condition2 == 0",
        br(),
        tags$h4("About this page", style = "color:blue"),
        p(
          "The fires shown on this page happened 2019.10-2020.4 located by Himawari 8. The causes of such fires are yet to be investigated.
          All ignition reasons are predicted based on our model.
          By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below."
        )
      ),
      conditionalPanel(
        # TODO: rename condition2 to a more descriptive name
        condition = "output.condition2 == 1",
        tabsetPanel(
          tabPanel(
            tags$em("Rainfall", style = "font-size:100%"),
            tags$hr(style = "border-color:  #d27979;"),
            plotlyOutput(ns("rain1"))
          ),
          tabPanel(
            tags$em("Temperature", style = "font-size:100%"),
            tags$hr(style = "border-color:  #ffc266;"),
            plotlyOutput(ns("temp1"))
          )
        )
      )
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # subsets prediction by month and reason
    map_data <- reactive({
      if (!is.null(input$month1)) {
        prediction <- prediction %>% filter(month %in% input$month1)
      }

      if (is.null(input$reason1)) {
        prediction <- prediction %>% filter(new_cause == 1)
      } else {
        prediction <- prediction %>% filter(new_cause %in% input$reason1)
      }
    })

    output$map2 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addLegend(pal = pal1, values = prediction$new_cause) %>%
        setView(lng = 144.7852, lat = -36.3913, zoom = 6.3) %>%
        addMouseCoordinates() %>%
        addCircleMarkers(
          data = map_data(), lat = ~lat, lng = ~lon,
          radius = 3,
          color = ~ pal1(new_cause),
          stroke = FALSE, fillOpacity = 20
        )
    })
    observe({
      leafletProxy("map2") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = map_data(), lat = ~lat, lng = ~lon,
          radius = 3,
          color = ~ pal1(new_cause),
          stroke = FALSE, fillOpacity = 20,
          popup = ~ paste0(
            "Fire reason: ", new_cause, "<br/>",
            "Forest types: ", FOR_TYPE, "<br/>",
            "Distance to road: ", round(dist_road)
          )
        )
    })
  })
}