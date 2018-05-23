#' similar fortress game
#'
#' @param a speed input
#' @param b angle input
#' @return the distance of \code{a} and \code{b}
#' @examples
#' player(100, 1)
#' player(150, 1.5)
#'
#'
#'

# Required Packages
library(ggplot2)

# Set arbitrary time & Gravitational acceleration
t <- seq(0, 100, 0.1)
g <- 9.8

# function
player <- function(a, b) {
  rdelement <<- runif(1, -10, 10)
  if (a > 0) {
    v <<- a + runif(1, -10, 10 + rdelement)
  } else {
    print("The Speed must be greater than zero!")
  }

  if (b > 0 & b < pi/2) {
    theta <<- b + runif(1, -0.1, 0.1)
  } else {
    print("The angle must be greater than zero and smaller than 1.57 radian!")
  }

  x <- v*cos(theta)*t
  y <- v*sin(theta)*t - 0.5*g*t^2

  x <- x[y >= 0]
  y <- y[y >= 0]

  df <- data.frame(x, y)
  ggplot(data = df, aes(x = x, y = y)) +
    geom_line(linetype = 5, size = 1, colour = "blue")

}

player(100, 2)

