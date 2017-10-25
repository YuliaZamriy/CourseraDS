# q1

library(MASS)
head(shuttle)

str(shuttle$use)
#Factor w/ 2 levels "auto","noauto": 1 1 1 1 1 1 1 1 1 1 ...

table(shuttle$use, shuttle$wind)
#head tail
#auto     72   73
#noauto   56   55

odds_auto_head <- (72/(72+56))/(56/(72+56))
# 1.285714
odds_auto_tail <- (73/(73+55))/(55/(73+55))
# 1.327273
odds_head_to_tail <- odds_auto_head / odds_auto_tail
# 0.9686888

loguse <- glm(data = shuttle, 
              formula = use ~ wind,
              family = binomial)
summary(loguse)

log(odds_auto_head)
# [1] 0.2513144
loguse$coefficients[1]
# -0.2513144 

log(odds_head_to_tail)
# [1] -0.03181183
loguse$coefficients[2]
# -0.03181183 

# answer:
exp(loguse$coefficients[2])
# 0.9686888 

# official answer:
loguse_off <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
summary(loguse_off)
exp(coef(loguse_off))

# q2
loguse2 <- glm(data = shuttle, 
              formula = use ~ wind + magn,
              family = binomial)
summary(loguse2)

# answer:
exp(loguse2$coefficients[2])
# 0.9684981 

# q3

loguse3 <- glm(data = shuttle, 
              formula = relevel(use, "noauto") ~ wind,
              family = binomial)
summary(loguse3)
# The coefficients reverse their signs
# The coefficients are on the log scale. So changing the sign changes the numerator and denominator for the exponent.

# q4
head(InsectSprays)
table(InsectSprays$spray)
#A  B  C  D  E  F 
#12 12 12 12 12 12 

sum(InsectSprays$count[InsectSprays$spray == 'A'])
# [1] 174
sum(InsectSprays$count[InsectSprays$spray == 'B'])
# [1] 184
174/184
# [1] 0.9456522

glm_spray <- glm(data = InsectSprays,
                 formula = count ~ spray,
                 family = "poisson")
summary(glm_spray)

# answer:
1/exp(glm_spray$coefficients[2])
# 0.9456522 

# q5

x <- rbinom(10,1,0.5)
t <- seq(1,10)
err <- round(rnorm(10),0)
err2 <- (err > 0) * err
count <- x*t + err2
plot(t, count)

glm(count ~ x + offset(t), family = poisson)
glm(count ~ x + offset(log(10) + t), family = poisson)

# The coefficient estimate for x is unchanged
# Note, the coefficients are unchanged, except the intercept, which is shifted by log(10). 
# Recall that, except the intercept, all of the coefficients are interpretted as log relative rates 
# when holding the other variables or offset constant. Thus, a unit change in the offset would cancel out. 
# This is not true of the intercept, which is interperted as the log rate (not relative rate) 
# with all of the covariates set to 0.

# q6

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)

knot <- (x < 0) * x
lm(y ~ x + knot)
yhat <- predict(lm(y ~ x + knot))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
