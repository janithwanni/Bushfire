box::use(
  shiny[
    NS,
    moduleServer,
    fluidRow,
    column,
    sidebarLayout,
    sidebarPanel,
    helpText,
    radioButtons,
    mainPanel
  ],
  leaflet[leafletOutput]
)

box::use(
  app/data/data[prediction]
)

ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(8,
      align = "left",
      sidebarLayout(
        sidebarPanel(
          helpText("Predicted fire probability maps"),
          radioButtons("month2",
            label = "Choose month:",
            choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
            selected = c("Oct")
          ),
          radioButtons("reason2",
            label = "Choose reason:",
            choices = levels(factor(prediction$new_cause)),
            selected = c("arson")
          )
        ),
        mainPanel(leafletOutput(outputId = "map3", height = 587))
      )
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}