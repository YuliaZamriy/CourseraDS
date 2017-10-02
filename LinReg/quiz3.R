#1 
fit <- lm(mpg ~ factor(cyl) + wt, mtcars)

coef(fit)
#(Intercept) factor(cyl)6 factor(cyl)8           wt 
#33.990794    -4.255582    -6.070860    -3.205613 

# answer: -6.070860

#2
fit2 <- lm(mpg ~ factor(cyl), mtcars)
cbind(coef(fit),coef(fit2))

#                   [,1]       [,2]
# (Intercept)  33.990794  26.663636
# factor(cyl)6 -4.255582  -6.920779
# factor(cyl)8 -6.070860 -11.563636
# wt           -3.205613  

33.99-6.07 #27.92
26.66-11.56 #15.1

# answer: Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

#3
fit3 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, mtcars)
summary(fit3)
anova(fit, fit3)
# p-value: 0.1239
# The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, 
# which suggests that the interaction terms may not be necessary.

#4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# answer: The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
hatvalues(lm(y ~ x))
#       1         2         3         4         5 
# 0.2286650 0.2438146 0.2525027 0.2804443 0.9945734 
# answer: 0.9945734 

influence(lm(y ~ x))$hat
#       1         2         3         4         5 
#0.2286650 0.2438146 0.2525027 0.2804443 0.9945734 

#6
dfbetas(lm(y ~ x))[5,2]
# [1] -133.8226

influence.measures(lm(y ~ x))
