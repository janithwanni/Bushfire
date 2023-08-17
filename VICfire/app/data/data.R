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
#' @export
r10a <- r10a
#' @export
r10ac <- r10ac
#' @export
r10l <- r10l
#' @export
r10b <- r10b

#' @export
r11a <- r11a
#' @export
r11ac <- r11ac
#' @export
r11l <- r11l
#' @export
r11b <- r11b

#' @export
r12a <- r12a
#' @export
r12ac <- r12ac
#' @export
r12l <- r12l
#' @export
r12b <- r12b

#' @export
r1a <- r1a
#' @export
r1ac <- r10ac
#' @export
r1l <- r1l
#' @export
r1b <- r1b

#' @export
r2a <- r2a
#' @export
r2ac <- r2ac
#' @export
r2l <- r2l
#' @export
r2b <- r2b

#' @export
r3a <- r3a
#' @export
r3ac <- r3ac
#' @export
r3l <- r3l
#' @export
r3b <- r3b
