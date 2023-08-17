box::use(
  KernSmooth[bkde2D]
)

load("data/save.RData")

# The following pattern of assigning the same variable
# is performed to ensure that the box modules are providing
# the object from the loaded dataset and not the symbol

#' @export
mydata <- mydata
#' @export
mydata1 <- mydata1
#' @export
mydata2 <- mydata2
#' @export
mydata4 <- mydata4

#' @export
prediction <- prediction

#' @export
d0d <- bkde2D(
  cbind(mydata$lon, mydata$lat),
  bandwidth = c(.0045, .0068),
  gridsize = c(50, 50)
)

# thank god there were only 6 * 4 combinations
#' @export
risk_map <- list(
  lightning = list(
    "Oct" = r10l, "Nov" = r11l,
    "Dec" = r12l, "Jan" = r1l,
    "Feb" = r2l, "Mar" = r3l
  ),
  accident = list(
    "Oct" = r10ac, "Nov" = r11ac,
    "Dec" = r12ac, "Jan" = r1ac,
    "Feb" = r2ac, "Mar" = r3ac
  ),
  arson = list(
    "Oct" = r10a, "Nov" = r11a,
    "Dec" = r12a, "Jan" = r1a,
    "Feb" = r2a, "Mar" = r3a
  ),
  burningoff = list(
    "Oct" = r10b, "Nov" = r11b,
    "Dec" = r12b, "Jan" = r1b,
    "Feb" = r2b, "Mar" = r3b
  )
)
