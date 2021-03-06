# q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

s <- c(0.3, 0.1471, 0.0025, 1.077)

ls <- numeric()
for (n in 1:4) ls[n] <-
  sum(w*(x - s[n])^2)
ls
#[1] 3.880100 3.716543 3.862994 9.768983

sum(x*w)/sum(w)
# [1] 0.1471429

# q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

yfit <- lm(y ~ x - 1)
yfit$coef
# 0.8262517

sum(y*x)/sum(x^2)
# [1] 0.8262517

# q3
data(mtcars)
cfit <- lm(mpg ~ wt, mtcars)
cfit$coef[2]
# -5.344472

attach(mtcars)
cor(mpg, wt) * sd(mpg)/sd(wt)
#[1] -5.344472

# q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xnorm <- (x - mean(x)) / sd(x)
xnorm
# [1] -0.9718658  1.5310215 -0.3993969  0.4393366 -0.5990954

# q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
yfit <- lm(y ~ x)
yfit$coef[1]
# 1.567461 

# q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
s <- c(0.573, 0.8, 0.44, 0.36)

ls <- numeric()
for (n in 1:4) ls[n] <-
  sum((x - s[n])^2)
ls
mean(x)
#[1] 0.573
mx <- sum((x - mean(x))^2)
mx
