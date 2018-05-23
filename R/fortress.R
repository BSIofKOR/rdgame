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


# Set arbitrary time & Gravitational acceleration
t <- seq(0, 100, 0.1)
g <- 9.8

# function

player <- function(speed, radian) {
  if ((speed > 0) & (speed < 1000) & (radian > 0) & (radian < pi/2)) {
    v <<- speed + runif(1, 0, 0.1)
    theta <<- radian + runif(1, 0, 0.1)

    # Parabolic formula
    x <- v*cos(theta)*t
    y <- v*sin(theta)*t - 0.5*g*t^2

    x <- x[y >= 0]
    y <- y[y >= 0]

    # Visualization
    df <- data.frame(x, y)
    ggplot2::ggplot(data = df, aes(x = x, y = y)) +
      ggplot2::coord_cartesian(xlim = c(0,10000), ylim = c(0, 5000)) +
      ggplot2::geom_line(linetype = 5, size = 1, colour = "blue")

  } else {
    v <<- 900
    theta <<- 1

    # Parabolic formula
    x <- v*cos(theta)*t
    y <- v*sin(theta)*t - 0.5*g*t^2

    x <- x[y >= 0]
    y <- y[y >= 0]

    # Visualization
    df <- data.frame(x, y)
    ggplot2::ggplot(data = df, aes(x = x, y = y)) +
      ggplot2::coord_cartesian(xlim = c(0,10000), ylim = c(0, 5000)) +
      ggplot2::geom_line(linetype = 5, size = 1, colour = "red")

    print("The Speed must be greater than zero adn smaller then 1000! &")
    print("The angle must be greater than zero and smaller than 1.57 radian!")
  }
}
player(0, 1)



