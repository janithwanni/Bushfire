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
    mainPanel,
    observe,
    req
  ],
  leaflet[
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    leaflet,
    addProviderTiles,
    setView,
    addLegend,
    addRasterImage,
    clearShapes
  ],
  raster[raster],
  magrittr[`%>%`]
)

box::use(
  app/data/data[prediction, risk_map, d0d],
  app/logic/constants[density_cutoff_value],
  app/logic/color_palettes[palRaster]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(8,
      align = "left",
      sidebarLayout(
        sidebarPanel(
          helpText("Predicted fire probability maps"),
          radioButtons(ns("month2"),
            label = "Choose month:",
            choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
            selected = c("Oct")
          ),
          radioButtons(ns("reason2"),
            label = "Choose reason:",
            choices = levels(factor(prediction$new_cause)),
            selected = c("arson")
          )
        ),
        mainPanel(leafletOutput(outputId = ns("map3"), height = 587))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$map3 <- renderLeaflet({
      KernelDensityRaster <- raster(
        list(
          x = d0d$x1,
          y = d0d$x2,
          z = risk_map[["arson"]][["Oct"]]
        )
      )

      KernelDensityRaster@data@values[
        which(
          KernelDensityRaster@data@values < density_cutoff_value
        )
      ] <- NA

      leaflet() %>%
        addProviderTiles("CartoDB") %>%
        setView(lng = 144.7852, lat = -36.3913, zoom = 6.3) %>%
        addLegend(
          pal = palRaster, values = c(1, 0),
          title = "Fire Probability"
        ) %>%
        addRasterImage(KernelDensityRaster, colors = palRaster, opacity = .4)
    })

    observe({
      req(!is.null(input$reason2) & !is.null(input$month2))
      leafletProxy("map3") %>%
        addProviderTiles("CartoDB") %>%
        clearShapes()

      KernelDensityRaster <- raster(
        list(
          x = d0d$x1, y = d0d$x2,
          z = risk_map[[input$reason2]][[input$month2]]
        )
      )
      KernelDensityRaster@data@values[
        which(KernelDensityRaster@data@values < density_cutoff_value)
      ] <- NA
      leafletProxy("map3") %>%
        addRasterImage(KernelDensityRaster, colors = palRaster, opacity = .4)
    })
  })
}