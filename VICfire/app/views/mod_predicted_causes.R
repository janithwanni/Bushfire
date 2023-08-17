box::use(
  shiny[
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
  leaflet[leafletOutput],
  plotly[plotlyOutput],
  shinyWidgets[checkboxGroupButtons]
)

box::use(
  app/data/data[prediction]
)

ui <- function(id) {
  fluidRow(
    column(8,
      align = "left",
      sidebarLayout(
        sidebarPanel(
          helpText("Predicted causes for fire ignition points over Oct 2019 through Mar 2020."),
          checkboxGroupButtons("month1",
            label = "Choose month:",
            choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
            individual = TRUE, justified = FALSE, selected = c("Dec"),
            width = "100%"
          ),
          checkboxGroupInput("reason1",
            label = "Choose reason:",
            choices = levels(factor(prediction$new_cause)),
            selected = c("arson", "lightning", "accident")
          )
        ),
        mainPanel(leafletOutput(outputId = "map2", height = 587))
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
            plotlyOutput("rain1")
          ),
          tabPanel(
            tags$em("Temperature", style = "font-size:100%"),
            tags$hr(style = "border-color:  #ffc266;"),
            plotlyOutput("temp1")
          )
        )
      )
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}