box::use(
  KernSmooth[bkde2D],
  grDevices[contourLines],
  magrittr[`%>%`],
  dplyr[filter],
  sp[
    Polygon,
    Polygons,
    SpatialPolygonsDataFrame,
    SpatialPolygons
  ],
)
#' @export
calculate_density <- function(filtered_data, reason) {
  selected_data <- filtered_data %>% filter(new_cause %in% reason)
  density_2d <- bkde2D(
    cbind(selected_data$lon, selected_data$lat),
    bandwidth = c(0.15, 0.15)
  )
  contour_lines <- contourLines(
    density_2d$x1,
    density_2d$x2,
    density_2d$fhat
  )
  density_dim1 <- sapply(
    seq_along(contour_lines),
    function(i) {
      Polygon(as.matrix(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y)))
    }
  )
  density_dim2 <- sapply(
    seq_along(contour_lines),
    function(i) {
      Polygons(list(density_dim1[[i]]), i)
    }
  )
  polygon_data <- data.frame(
    Value = sapply(
      seq_along(contour_lines),
      function(i) {
        contour_lines[[i]]$level
      }
    )
  )
  return(
    SpatialPolygonsDataFrame(SpatialPolygons(density_dim2), data = polygon_data)
  )
}