# Color palettes
#' @export
pal <- leaflet::colorFactor(
  pal = c("#E69F00", "#000000", "#0072B2", "#009E73", "#F0E442", "#CC79A7"),
  domain = c("arson", "lightning", "burningoff", "accident", "relight", "other")
)

#' @export
pal1 <- leaflet::colorFactor(
  pal = c("#E69F00", "#000000", "#0072B2", "#009E73"),
  domain = c("arson", "lightning", "burningoff", "accident")
)

#' @export
pal2 <- leaflet::colorFactor(
  palette = "red",
  domain = c(0, 1)
)

#' @export
palRaster <- leaflet::colorNumeric( # nolint: object_name_linter
  palette = c("yellow", "red"),
  domain = c(1, 0),
  na.color = "transparent"
)


# leaflet density color palettes
#' @export
density_colors <- c(
  "accident" = "#E69F00",
  "arson" = "#000000",
  "burningoff" = "#0072B2",
  "lightning" = "#009E73",
  "other" = "#F0E442",
  "relight" = "#CC79A7"
)