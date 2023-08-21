box::use(
  shiny[
    reactive,
    reactiveValues,
    observeEvent,
    outputOptions,
    renderText
  ],
  dplyr[
    filter,
    group_by
  ],
  magrittr[`%>%`],
  plotly[
    renderPlotly,
    plot_ly,
    add_trace,
    add_lines,
    add_histogram,
    layout,
    highlight_key
  ],
  ggplot2[
    ggplot,
    aes,
    geom_bar
  ],
  htmlwidgets[onRender],
  leaflet[
    leaflet,
    leafletProxy,
    renderLeaflet,
    setView,
    addTiles,
    addProviderTiles,
    addLegend,
    addLayersControl,
    addCircles,
    addCircleMarkers,
    addPolygons,
    addRasterImage,
    clearMarkers,
    clearGroup,
    clearShapes,
    hideGroup
  ],
  leafem[addLogo, addMouseCoordinates],
  raster[raster],
  DT[renderDataTable, datatable]
)

box::use(
  app/data/data[
    mydata, mydata1, mydata2, mydata4,
    prediction,
    risk_map,
    d0d
  ],
  app/logic/constants[density_cutoff_value],
  app/logic/color_palettes[
    pal,
    pal1,
    pal2,
    palRaster,
    density_colors
  ],
  app/logic/calculate_density[
    calculate_density
  ],
  app/views/mod_fire_risk_map,
  app/views/mod_predicted_causes
)

# Server logic ----
server <- function(input, output) {

  mod_fire_risk_map$server("fire_risk_map")
  mod_predicted_causes$server("predicted_causes")

  filtered_data <- reactive({
    if (!is.null(input$year)) {
      mydata <- mydata %>% filter(year >= input$year[1] & year <= input$year[2])
    }
    if (!is.null(input$month)) {
      mydata <- mydata %>% filter(month %in% input$month)
    }
    if (is.null(input$reason)) {
      mydata <- mydata %>% filter(new_case == 1)
    } else {
      mydata <- mydata %>% filter(new_cause %in% input$reason)
    }
    mydata
  })

  clicked_map <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$map_marker_click, {
    clicked_map$clickedMarker <- input$map_marker_click
  })


  selected_coordinates <- reactive(({
    c(clicked_map$clickedMarker$lng, clicked_map$clickedMarker$lat)
  }))


  clicked <- reactive(({
    subset(filtered_data(), lon == as.numeric(selected_coordinates()[1]) & lat == as.numeric(selected_coordinates()[2]))
  }))
  condition1 <- reactive({
    if (is.null(selected_coordinates())) {
      result <- 0
    } else {
      result <- 1
    }
    result
  })

  output$condition1 <- renderText({
    condition1()
  })

  outputOptions(output, "condition1", suspendWhenHidden = FALSE)


  output$rain <- renderPlotly({
    rain <- clicked()
    if (is.null(rain)) {
      return(NULL)
    }
    plot_ly(
      x = c(" 7day", "14day", "28day"),
      y = c(rain$avr7, rain$avr14, rain$avr28), name = "20 years average", opacity = 0.3,
      type = "bar"
    ) %>%
      add_trace(y = c(rain$arf7, rain$arf14, rain$arf28), name = "rain fall for that time", type = "bar", width = 0.3, opacity = 1) %>%
      layout(
        title = "",
        xaxis = list(title = "Period Average rain fall"),
        yaxis = list(title = "mm"),
        barmode = "overlay"
      )
  })

  output$temp <- renderPlotly({
    temp <- clicked()
    if (is.null(temp)) {
      return(NULL)
    }
    plot_ly(
      x = c(" 7day", "14day", "28day"),
      y = c(temp$amaxt7, temp$amaxt14, temp$amaxt28),
      # TODO Move color arguments to color palette based ones
      type = "scatter", mode = "lines", name = "max", line = list(color = "rgb(205, 12, 24)")
    ) %>%
      add_trace(y = c(temp$amint7, temp$amint14, temp$amint28), name = "min", line = list(color = "rgb(22, 96, 167)")) %>%
      add_trace(y = c(temp$avmin7, temp$avmin14, temp$avmin28), name = "20 year average min", line = list(color = "rgb(22, 96, 167)"), opacity = 0.3) %>%
      add_trace(y = c(temp$avmax7, temp$avmax14, temp$avmax28), name = "20 year average max", line = list(color = "rgb(205, 12, 24)"), opacity = 0.3) %>%
      layout(
        title = "",
        xaxis = list(title = "Period Average Max/Min Temperature"),
        yaxis = list(title = "Temperature (°C)")
      )
  })

  clicked_map2 <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$map2_marker_click, {
    clicked_map2$clickedMarker <- input$map2_marker_click
  })
  selected_coordinates1 <- reactive(({
    c(clicked_map2$clickedMarker$lng, clicked_map2$clickedMarker$lat)
  }))


  clicked1 <- reactive(({
    subset(pre_2(), lon == as.numeric(selected_coordinates1()[1]) & lat == as.numeric(selected_coordinates1()[2]))
  }))

  condition2 <- reactive({
    if (is.null(selected_coordinates1())) {
      result <- 0
    } else {
      result <- 1
    }
    result
  })

  output$condition2 <- renderText({
    condition2()
  })

  outputOptions(output, "condition2", suspendWhenHidden = FALSE)

  output$rain1 <- renderPlotly({
    rain1 <- clicked1()
    if (is.null(rain1)) {
      return(NULL)
    }
    plot_ly(
      x = c(" 7day", "14day", "28day"),
      y = c(rain1$arf7, rain1$arf14, rain1$arf28),
      name = "rain fall",
      type = "bar"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Period Average rain fall"),
        yaxis = list(title = "mm")
      )
  })

  output$temp1 <- renderPlotly({
    temp1 <- clicked1()
    if (is.null(temp1)) {
      return(NULL)
    }
    plot_ly(
      x = c(" 7day", "14day", "28day"),
      y = c(temp1$amaxt7, temp1$amaxt14, temp1$amaxt28),
      type = "scatter", mode = "lines", name = "max"
    ) %>%
      add_trace(y = c(temp1$amint7, temp1$amint14, temp1$amint28), name = "min") %>%
      layout(
        title = "",
        xaxis = list(title = "Period Average Max/Min Temperature"),
        yaxis = list(title = "°C")
      )
  })

  # initiate a plotly object
  tx <- highlight_key(mydata2, ~new_cause)
  # initiate a plotly object
  base <- plot_ly(tx, color = ~new_cause) %>% group_by(year)
  # create a time series of median house price
  t <- base %>%
    group_by(new_cause) %>%
    add_lines(x = ~year, y = ~Total, legendgroup = ~new_cause)
  hist <- add_histogram(base, x = ~Total, histnorm = "probability density")
  fig <- plot_ly(mydata4,
    x = ~year, y = ~accident, type = "bar",
    name = "accident", marker = list(color = "#E69F00", level = 1)
  ) %>%
    # TODO Move color arguments to color palette based ones
    add_trace(y = ~arson, name = "arson", marker = list(color = "#000000")) %>%
    add_trace(y = ~burningoff, name = "burningoff", marker = list(color = "#0072B2")) %>%
    add_trace(y = ~lightning, name = "lightning", marker = list(color = "#009E73")) %>%
    add_trace(y = ~other, name = "other", marker = list(color = "#F0E442")) %>%
    add_trace(y = ~relight, name = "relight", marker = list(color = "#CC79A7")) %>%
    layout(yaxis = list(title = "Total number"), legend = list(traceorder = "normal"), barmode = "stack")

  output$p1 <- renderPlotly({
    fig
    fig %>% onRender(js)
  })

  output$p2 <- renderPlotly({
    d <- input$trace
    mydata2 %>%
      filter(new_cause %in% d) %>%
      plot_ly() %>%
      add_lines(x = ~year, y = ~Total, name = "Fire trend") %>%
      layout(
        yaxis = list(
          zeroline = FALSE, showline = FALSE,
          autotick = TRUE, ticks = "outside", rangemode = "tozero", title = "Total number"
        ),
        showlegend = TRUE, xaxis = list(autotick = TRUE, showline = TRUE, ticks = "outside")
      )
  })

  output$percentage <- renderPlot({
    ggplot() +
      geom_bar(
        data = filter(filtered_data(), new_cause == input$reason),
        aes(x = year, y = fire, fill = new_cause, label = "fire"), stat = "identity"
      ) +
      scale_fill_manual("legend",
        values = c( # TODO Move values argument to color palette based ones
          "arson" = "#000000",
          "lightning" = "#009E73",
          "relight" = "#CC79A7",
          "other" = "#F0E442",
          "accident" = "#E69F00",
          "burningoff" = "#0072B2"
        )
      )
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(pal = pal, values = mydata$new_cause) %>%
      addLayersControl(overlayGroups = c("show all fire")) %>%
      setView(lng = 144.7852, lat = -36.3913, zoom = 6.3) %>%
      addMouseCoordinates() %>%
      leafem::addLogo(png, url = "https://www.cfa.vic.gov.au/home") %>%
      addCircles(
        data = mydata1, lat = ~lat, lng = ~lon,
        radius = 100, color = "#C0C0C0", # TODO Move to color palettes
        stroke = FALSE, fillOpacity = 0.7, group = "show all fire"
      ) %>%
      hideGroup("show all fire")
  })

  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data(), lat = ~lat, lng = ~lon,
        radius = 3,
        color = ~ pal(new_cause),
        stroke = FALSE, fillOpacity = 20,
        popup = ~ paste0(
          "Fire ID: ", EVENTID, "<br/>",
          "Fire starts at: ", FIRE_START, "<br/>",
          "Wind speed: ", round(ws, 2), "  m/s", "<br/>",
          "Fire reason: ", new_cause, "<br/>",
          "Forest types: ", FOR_TYPE, "<br/>",
          "Distance to road: ", round(dist_road), "  metres"
        )
      )
  })

  observeEvent(input$showd, {
    req(!is.null(input$showd), !is.null(input$reason))
    leafletProxy("map") %>%
      addTiles() %>%
      clearGroup(group = "plot density")

    density_calculated <- calculate_density(filtered_data(), input$reason)
    leafletProxy("map") %>%
      addTiles() %>% # TODO Move col arguments to color palette based ones
      addPolygons(
        data = density_calculated,
        col = density_colors[input$reason],
        group = "plot density",
        stroke = FALSE
      )
  })

  observeEvent(input$cleard, {
    leafletProxy("map") %>%
      addTiles() %>%
      clearGroup(group = "plot density")
  })

  output$data <- DT::renderDataTable(datatable(
    mydata[, c(4:5, 8, 10, 11, 13, 14, 65)],
    filter = "top",
    colnames = c(
      "Fire name", "Fire district", "Fire Start", "Longitude", "Latitude", "Forest Type", "Forest Category",
      "Cause"
    )
  ))
}
