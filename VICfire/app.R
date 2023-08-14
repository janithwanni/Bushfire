js <- c(
  "function(el, x){",
  "  el.on('plotly_legendclick', function(evtData) {",
  "    Shiny.setInputValue('trace', evtData.data[evtData.curveNumber].name);",
  "  });",
  "}"
)

ystart <- -39.08246
yend <- -34.03690
xstart <- 140.6923
xend <- 149.8976
y <- seq(ystart + 0.1 / 2, yend - 0.1 / 2, .1)
x <- seq(xstart + 0.181 / 2, xend - 0.181 / 2, 0.181)
png <- "https://cdn.cfa.vic.gov.au/o/cfa-theme/images/cfa-logo.png"

# Run app ----
shinyApp(ui, server)