#' simple and similar fortress game
#'
#' @param speed speed input
#' @param angle angle input
#' @return the distance of \code{a} and \code{b}
#' @examples
#' ggplayer(100, 1)
#' aniplayer(150, 1.5)
#'
#'
#'


# function using ggplot2

# Set arbitrary time & Gravitational acceleration

ggplayer <- function(speed, radian) {

  if ((speed > 0) & (radian > 0) & (radian < pi/2)) {

    library(ggplot2)

    # Assign initial value
    t <- seq(0, 100, 0.1)
    g <- 9.8

    v <- speed
    theta <- radian

    # Parabolic formula
    xx <- v*cos(theta)*t
    yy <- v*sin(theta)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    # Visualization
    df <- data.frame(xx, yy)

    ggplot(data = df, mapping = aes(xx, yy)) +
      coord_cartesian(xlim = c(0, 10000), ylim = c(0, 5000)) +
      geom_line(linetype = 5, size = 1, colour = "blue")


  } else {

    print("The Speed must be greater than zero & \n
          The angle must be greater than zero and smaller than 1.57 radian!")

    return()
  }
}

# example
ggplayer(250, 0.9)

#===========================================================================

# function using animation

aniplayer <- function(speed, radian) {

  if ((speed > 0) & (radian > 0) & (radian < pi/2)) {

    library(animation)

    #Assign initial value
    t <- seq(0, 100, 1)
    g <- 9.8

    v <- speed
    theta <- radian

    ani.options(interval = 0.08)

    # Parabolic formula
    xx <- v*cos(theta)*t
    yy <- v*sin(theta)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    df <- data.frame(xx, yy)

    plot.new()
    plot(xx, yy, type = "n",
         xlim = c(-100, 10000), ylim = c(0, 5000),
         xlab = "speed", ylab = "high", main = "fortress")

    for (i in 1:100) {

      points(xx[i], yy[i], pch = 10, cex = 0.1)

      ani.record(replay.cur = TRUE)

    }

    ani.replay()
    ani.record(reset = TRUE)



  } else {

    print("The Speed must be greater than zero & \n
          The angle must be greater than zero and smaller than 1.57 radian!")

  }
}

# example
aniplayer(100, 1.1)
