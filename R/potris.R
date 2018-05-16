#=============================================
t <- seq(0, 1000, 0.1)
g <- 9.8

v0 <- 100 + runif(1, 0, 1)
theta <- pi/3 + runif(1, -0.1, 0.1)


#dd <- function(x0, y0) {
#  v0 <- x0 + runif(1, 0, 1)
#  theta <- y0 + runif(1, 0, 1)
#
#}
#dd(10, 10)
#?sin

x <- v0*cos(theta)*t
y <- v0*sin(theta)*t - 0.5*g*t^2
x <- x[y >= 0]
y <- y[y >= 0]
df <- data.frame(x, y)
ggplot(data = df, aes(x = x, y = y)) +
  geom_point()
