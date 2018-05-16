player <- function(a, b) {
  if (a > 0) {
    v <<- a + runif(1, 0, 1)
  } else {
    print("속력을 0보다 크게 설정하세요.")
  }

  if (b > 0 & b < pi/2) {
    theta <<- b + runif(1, -0.1, 0.1)
  } else {
    print("각도를 0초과 pi/2미만의 값으로 입력하세요.")
  }

  x <- v*cos(theta)*t
  y <- v*sin(theta)*t - 0.5*g*t^2

  x <- x[y >= 0]
  y <- y[y >= 0]

  df <- data.frame(x, y)
  ggplot(data = df, aes(x = x, y = y)) +
    geom_line(linetype = 5, size = 1, colour = "blue")

}
