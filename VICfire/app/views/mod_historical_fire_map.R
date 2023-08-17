box::use(
  shiny[
    NS,
    tagList,
    moduleServer,
    p,
    fluidRow,
    column,
    sidebarLayout,
    sidebarPanel,
    helpText,
    sliderInput,
    checkboxGroupInput,
    actionButton,
    mainPanel,
    br,
    tags,
    conditionalPanel,
    tabsetPanel,
    tabPanel
  ],
  leaflet[
    leafletOutput,
    renderLeaflet
  ],
  plotly[
    plotlyOutput,
    renderPlotly
  ],
  shinyWidgets[checkboxGroupButtons]
)

box::use(
  app/data/data[
    mydata
  ]
)
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
        align = "left",
        sidebarLayout(
          sidebarPanel(
            helpText("Historical locations on fires, and ignition causes, in Victoria over 2000-2019."),
            sliderInput("year",
              label = "Choose year:",
              value = c(2010, 2017),
              min = min(mydata$year),
              max = max(mydata$year),
              step = 1,
              sep = ""
            ),
            checkboxGroupButtons("month",
              label = "Choose month:",
              choices = c(
                "Jan", "Feb", "Mar",
                "Apr", "May", "Jun",
                "Jul", "Aug", "Sep",
                "Oct", "Nov", "Dec"
              ),
              individual = TRUE,
              justified = FALSE,
              selected = c("Nov", "Dec", "Jan"),
              width = "100%"
            ),
            checkboxGroupInput("reason",
              label = "Choose reason:",
              choices = levels(factor(mydata$new_cause)),
              selected = c("lightning", "arson", "accident")
            ),
            actionButton("showd", "Show density plot"),
            actionButton("cleard", "Clear density plot")
          ),
          mainPanel(
            leafletOutput(outputId = "map", height = 587)
          )
        )
      ),
      column(
        4,
        conditionalPanel(
          # The conditionals are set based on
          # whether coordinates are being clicked on.
          # TODO: rename condition1 to a more descriptive name
          condition = "output.condition1 == 0",
          br(),
          tags$h4("About", style = "color:blue"),
          p(
            "This Shiny App helps visualise fires in Victoria for last two decades.
            YAfter choose the year, month, and ignition reason. The fires match these condition will automatically shown on the map.
            Due to the limitation of the package, the density plot cannot be refreshed automatically. Each time you change the conditions, you have to clear and re-plot the densitiy plot.
            By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below."
          )
        ),
        conditionalPanel(
          condition = "output.condition1 == 1",
          # TODO: rename condition1 to a more descriptive name
          tabsetPanel(
            tabPanel(
              tags$em("Percentage", style = "font-size:100%"),
              # TODO: rename ids to more descriptive name
              plotlyOutput("p1", height = 300),
              # TODO: rename ids to more descriptive name
              plotlyOutput("p2", height = 200)
            ),
            tabPanel(
              tags$em("Rainfall", style = "font-size:100%"),
              tags$hr(style = "border-color:  #d27979;"),
              plotlyOutput("rain")
            ),
            tabPanel(
              tags$em("Temperature", style = "font-size:100%"),
              tags$hr(style = "border-color:  #ffc266;"),
              plotlyOutput("temp")
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}