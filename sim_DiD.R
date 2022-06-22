library(arm)
library(fixest)
n <- 10000
z1 <- rnorm(n)
x1 <- z1 + rnorm(n)
y2 <- z1 + rnorm(n)

z2 <- .5*z1 + rnorm(n)
x2 <- .5*x1 + .5*y2 + z2 + rnorm(n)
y3 <- z2 + rnorm(n)

df <- data.frame(id = rep(1:n, 2), time = rep(1:2, each=n), x = c(x1, x2), y = c(y2, y3), z = c(z1, z2) )

reg2 <- lm(y ~ x, data=df)
summary(reg2)

reg3 <- lm(y ~ x + z , data=df)
summary(reg3)

reg4 <- feols(y ~ x + z | id + time, data=df)
summary(reg4)

