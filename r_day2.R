library(MASS)
x <- 1 : 10
y <- x^2 / log(x + 1)
plot(x, y)

m1 <- rnorm(200, mean = 3, sd = 1)
m2 <- rnorm(200, 3, 10)
bivn_kde <- kde2d(m1, m2, n = 50)
op <- par(mfrow = c(2, 2))
plot(x, y)
contour(bivn_kde) # 등고선
image(bivn_kde) # 등고선 색칠
persp(bivn_kde, phi = 10, theta = 30, col = "gray") # 3차원

par(mfrow = c(1, 1))
persp(bivn_kde, phi = 0, theta = 0, col = "gray")



# Linear Regression
str(mtcars)
mtcars_lm <- lm(mpg ~ hp, data = mtcars)
summary(mtcars_lm)

coefs <- coef(lm(mpg ~ hp, data = mtcars))
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_abline(intercept = coefs[1], slope = coefs[2], color = "red") +
  theme_light()



df <- data.frame(Time = 1 : 10, score = seq(10, 100, by = 10))
plot(score ~ Time, data = df, pch = "+", col = "blue")
lr <- lm(score ~ Time, data = df)
summary(lr)
abline(lr, col = "red")

cor(mtcars$hp, mtcars$mpg)
plot(mpg ~ hp, mtcars)

lm(dist ~ speed, data = cars)
plot(cars)
m <- lm(dist ~ speed, data = cars)
summary(m)

pre <- predict(m, interval = "confidence")

predict(m, newdata = data.frame(speed = 100))
summary(m)


