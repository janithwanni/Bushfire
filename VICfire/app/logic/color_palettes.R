# Color palettes
pal <- colorFactor(
  pal = c("#E69F00", "#000000", "#0072B2", "#009E73", "#F0E442", "#CC79A7"),
  domain = c("arson", "lightning", "burningoff", "accident", "relight", "other")
)
pal1 <- colorFactor(
  pal = c("#E69F00", "#000000", "#0072B2", "#009E73"),
  domain = c("arson", "lightning", "burningoff", "accident")
)
pal2 <- colorFactor(
  palette = "red",
  domain = c(0, 1)
)

palRaster <- colorNumeric( # nolint: object_name_linter
  palette = c("yellow", "red"),
  domain = c(1, 0),
  na.color = "transparent"
)
