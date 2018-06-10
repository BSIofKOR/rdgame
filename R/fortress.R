#' HIT THE TARGET
#'
#' @param speed speed input
#' @param angle angle input
#' @param target target input
#' @return the distance of \code{a} and \code{b}
#' @examples
#' inputplay(100, 1.5, 300)
#' inputplay(50, 0.8, 150)
#'
#'
#'



# function

inputplay <- function(speed, radian, target) {

  if ((speed >= 10) & (speed <= 100) & (radian > 0) & (radian < pi/2)) {

    # Assign initial setting

    t <- seq(0, 100, 0.15)
    g <- 9.8

    speed <- speed + runif(1, -20, 20)
    speed <- abs(speed)

    # Parabolic formula

    xx <- speed*cos(radian)*t
    yy <- speed*sin(radian)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    # plot

    df <- data.frame(xx, yy)

    plot.new()
    plot(xx, yy, type = "n",
         xlim = c(0, 1000), ylim = c(0, 500),
         xlab = "DISTANCE", ylab = "HIGH", main = "HIT THE TARGET")

    a1 <- runif(1, target + 5, target + 50)
    a2 <- runif(1, target + 5, target + 50)
    abline(v = c(target, a1), h = c(target, a2), col = "blue", lty = 2)

    # animation

    library(animation)

    ani.options(interval = 0.075)

    n <- length(xx)

    for (i in 1:n) {

      points(xx[i], yy[i], pch = 16, cex = 0.5, col = "red")

      # recording

      ani.record(replay.cur = TRUE)

    }

    # play

    ani.replay()
    ani.record(reset = TRUE)

    # result

    if (max(xx) < 1000) {

      cat("Destination point", ":", max(xx))

      for (i in 1:n) {

        if ((xx[i] > target) & (xx[i] < a1) & (yy[i] > target) & (yy[i]) < a2) {

          cat("\n", "SUCCESS!!!")
          legend(x = 200, y = 400, legend = "SUCCESS!!!", cex = 3, bg = "lemonchiffon")
          break()

        }

      }


    } else {

      # Out of range

      legend(x = 200, y = 400, legend = "OUT!", cex = 3, bg = "red")
      cat("OUT!")

    }

  } else {

    # Out of input range

    cat("The Speed must be between 10 and 100 &", "\n",
        "The angle must be greater than 0 and less than 1.57 radian!")

  }
}

# example
inputplay(100, 1.1, 100)

