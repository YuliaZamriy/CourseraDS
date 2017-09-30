# q1 

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit = lm(y ~ x)
summary(fit)$coef[2,4]
# [1] 0.05296439

# q2
summary(fit)$sigma
# [1] 0.2229981

# q3 incorrect

fit <- lm(mpg ~ wt, mtcars)
summary(fit)

predict(fit, newdata = (wt = mean(mtcars$wt)),interval = ("confidence"))
#       fit      lwr      upr
#1 20.09062 18.99098 21.19027

# q5

predict(fit, newdata = (wt = 3),interval = ("prediction"))
#       fit      lwr      upr
#1 21.25171 14.92987 27.57355

# q6
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2])*2
# [1] -12.97262  -8.40527

fit2 <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit2)[2, ]
# -12.97262  -8.40527 

# q9
fit0 <- lm(mpg ~ 1, mtcars)
deviance(fit)/deviance(fit0)
# [1] 0.2471672

