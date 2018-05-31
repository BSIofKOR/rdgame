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

    xx <- sort(xx)
    #yy <- sort(yy)

    #print(xx)
    #print(yy)

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

player(200, 0.9)

#===========================================================================

# function2

player <- function(speed, radian) {

  if ((speed > 0) & (radian > 0) & (radian < pi/2)) {

    t <- seq(0, 100, 1)
    g <- 9.8

    v <- speed
    theta <- radian

    animation::ani.options(interval = 0.08)


    # Parabolic formula
    xx <- v*cos(theta)*t
    yy <- v*sin(theta)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    df <- data.frame(xx, yy)

    plot.new()
    plot(xx, yy,
         type = "n",
         xlim = c(0, 10000), ylim = c(0, 5000),
         xlab = "speed", ylab = "high", main = "fortress")

    for (i in 1:100) {

      points(xx[i], yy[i], pch = 10, cex = 0.1)

      animation::ani.record()

    }

    animation::ani.replay()
    animation::ani.record(reset = T)



  } else {

    print("The Speed must be greater than zero & \n
          The angle must be greater than zero and smaller than 1.57 radian!")

  }
}
