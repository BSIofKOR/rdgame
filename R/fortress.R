#' similar fortress game
#'
#' @param speed speed input
#' @param angle input
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
    v <- speed
    theta <- radian

    # Parabolic formula
    xx <- v*cos(theta)*t
    yy <- v*sin(theta)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    # Visualization
    df <- data.frame(xx, yy)
    ggplot2::ggplot(data = df, mapping = aes(xx, yy)) +
      ggplot2::coord_cartesian(xlim = c(0, 10000), ylim = c(0, 5000)) +
      ggplot2::geom_line(linetype = 5, size = 1, colour = "blue")

  } else {

    print("The Speed must be greater than zero adn smaller then 1000! &")
    print("The angle must be greater than zero and smaller than 1.57 radian!")

    return()
  }
}
player(0, 10)


