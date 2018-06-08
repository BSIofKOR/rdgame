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



# function

aniplayer <- function(speed, radian, a) {

  if ((speed >= 10) & (speed <= 100) & (radian > 0) & (radian < pi/2)) {

    library(animation)

    #Assign initial value
    t <- seq(0, 100, 0.2)
    g <- 9.8

    ani.options(interval = 0.075)

    speed <- speed+runif(1,-20,20)
    speed <- abs(speed)

    # Parabolic formula
    xx <- speed*cos(radian)*t
    yy <- speed*sin(radian)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    df <- data.frame(xx, yy)

    plot.new()
    plot(xx, yy, type = "n",
         xlim = c(0, 1000), ylim = c(0, 500),
         xlab = "distance", ylab = "high", main = "fortress")

    abline(v = c(a, runif(1, a + 5, a + 50)), h = c(a, runif(1, a + 5, a + 50)), col = "blue")

     z <- length(xx)

    for (i in 1:z) {

      points(xx[i], yy[i], pch = 14, cex = 0.5, col = "red")

      ani.record(replay.cur = TRUE)

    }

    ani.replay()

    if (max(xx) < 1000) {

      print(max(xx))

      if


    } else {

      legend(x = 200, y = 400, legend = "FAIL!!!", cex = 4, col = "red")
      print("FAIL!!!")

    }

    ani.record(reset = TRUE)


  } else {

    print("The Speed must be greater than zero & \n
          The angle must be greater than zero and smaller than 1.57 radian!")

  }
}

# example
aniplayer(80, 1, 300)

#=========================================================


# function

aniplayer <- function(speed, radian, a) {

  if ((speed >= 10) & (speed <= 100) & (radian > 0) & (radian < pi/2)) {

    library(animation)

    #Assign initial value
    t <- seq(0, 100, 0.2)
    g <- 9.8

    ani.options(interval = 0.075)

    speed <- speed + runif(1, -20, 20)
    speed <- abs(speed)

    # Parabolic formula
    xx <- speed*cos(radian)*t
    yy <- speed*sin(radian)*t - 0.5*g*t^2

    xx <- xx[yy >= 0]
    yy <- yy[yy >= 0]

    xx <- sort(xx)

    df <- data.frame(xx, yy)

    plot.new()
    plot(xx, yy, type = "n",
         xlim = c(0, 1000), ylim = c(0, 500),
         xlab = "distance", ylab = "high", main = "fortress")
    a1 <- runif(1, a + 5, a + 50)
    a2 <- runif(1, a + 5, a + 50)
    abline(v = c(a,a1), h = c(a, a2), col = "blue")

    z <- length(xx)

    for (i in 1:z) {

      points(xx[i], yy[i], pch = 14, cex = 0.5, col = "red")

      ani.record(replay.cur = TRUE)

    }

    ani.replay()
    ani.record(reset = TRUE)

    if (max(xx) < 1000) {

      print(max(xx))

      for (i in 1:z) {
        if (xx[i] < a) {
          print("awd!")

        }else{
          print("SUCCESS!!!")
          break()
        }

      }


    } else {

      legend(x = 200, y = 400, legend = "FAIL!!!", cex = 4, col = "red")
      print("FAIL!!!")

    }

  } else {

    print("The Speed must be greater than zero & \n
          The angle must be greater than zero and smaller than 1.57 radian!")

  }
}

# example
aniplayer(55, 0.8, 150)

