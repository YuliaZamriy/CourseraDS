n = 100
x = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
# coef for x
sum(ey * ex) / sum(ex ^ 2)
# same as
coef(lm(ey ~ ex - 1))
# same as
coef(lm(y ~ x + x2 + x3)) 


require(datasets)
install.packages("GGally")
library(GGally)
require(ggplot2)
data(swiss)

g <- ggpairs(swiss, lower = list(continuous = wrap("smooth", method = "lm")))
g
g <- ggpairs(swiss, lower = list(continuous = "smooth"))
g

ggpairs(swiss, lower = list(continuous = "smooth_loess"))
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))

summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ . , data = swiss))$coefficients
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)

# simulation on confounding effects
n <- 100
x2 <- 1 : n
x1 <- .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01)

summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))

g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + 
    geom_point(colour="grey50", size = 5) + 
    geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + 
    geom_point(size = 4) 
g

g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + 
    geom_point(colour="grey50", size = 5) + 
    geom_smooth(method = lm, se = FALSE, colour = "black") + 
    geom_point(size = 4) 
g2

# Dummy variables in regression
data(InsectSprays)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

# comparing against spray A
summary(lm(count ~ spray, data = InsectSprays))$coef

summary(lm(count ~ 
               I(1 * (spray == 'B')) + 
               I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + 
               I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

# comparing against zero
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))

# comparing against spray C
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

# creating binary data
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1

fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1

fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1

summary(fit)$coef

# Adjustment

# Simulation 1
n <- 100
t <- rep(c(0, 1), c(n/2, n/2))
x <- c(runif(n/2), runif(n/2))

beta0 <- 0
beta1 <- 2
tau <- 1
sigma <- .2

y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 2
n <- 100
t <- rep(c(0, 1), c(n/2, n/2))
x <- c(runif(n/2), 1.5 + runif(n/2))

beta0 <- 0
beta1 <- 2
tau <- 0
sigma <- .2

y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 1)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 1)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 1)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 3
n <- 100
t <- rep(c(0, 1), c(n/2, n/2))
x <- c(runif(n/2), .9 + runif(n/2))

beta0 <- 0
beta1 <- 2
tau <- -1
sigma <- .2

y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3, col = "blue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "red")
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 4
n <- 100
t <- rep(c(0, 1), c(n/2, n/2))
x <- c(.5 + runif(n/2), runif(n/2))

beta0 <- 0
beta1 <- 2
tau <- 1
sigma <- .2

y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3, col = "blue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "red")
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 5
n <- 100
t <- rep(c(0, 1), c(n/2, n/2))
x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1))

beta0 <- 0
beta1 <- 2
tau <- 0
tau1 <- -4
sigma <- .2

y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 6
p <- 1
n <- 100
x2 <- runif(n)
x1 <- p * runif(n) - (1 - p) * x2

beta0 <- 0
beta1 <- 1
tau <- 4 
sigma <- .01

y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)

plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

install.packages("rgl")
library(rgl)
plot3d(x1, x2, y)

plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)

# Residuals:

data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss)
plot(fit)

par(mfrow = c(1, 1))
n <- 100
x <- rnorm(n)
y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

# Case 1

n <- 100
x <- c(10, rnorm(n))
y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))    

fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

# Case 2

x <- rnorm(n)
y <- x + rnorm(n, sd = .3)
x <- c(5, x)
y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)            

round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)

# Case 3

## Don't everyone hit this server at once.  Read the paper first.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
summary(lm(V1 ~ . -1, data = dat))$coef
fit <- lm(V1 ~ . - 1, data = dat)
plot(predict(fit), resid(fit), pch = '.')

data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss)
plot(fit)

# R**2 vs. number of variables
par(mfrow = c(1, 1))
n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
r <- sapply(1 : n, function(p)
{
    y <- rnorm(n)
    x <- matrix(rnorm(n * p), n, p)
    summary(lm(y ~ x))$r.squared 
}
)
lines(1 : n, r, lwd = 2)
abline(h = 1)

# Variance inflation

n <- 100
nosim <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)

betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

x1 <- rnorm(n)
x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)

betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

##doesn't depend on which y you use,
y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]

c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2],
  summary(lm(y~ x1 + x2 + x3))$cov.unscaled[2,2]) / a

temp <- apply(betas, 1, var)
temp[2 : 3] / temp[1]

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a 

library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 

# Nested models
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)

# swirl

ones <- rep(1, nrow(galton))
lm(child ~ ones + parent - 1, galton)
lm(child ~ parent, galton)
lm(child ~ 1, galton)

# Regress the given variable on the given predictor,
# suppressing the intercept, and return the residual.
regressOneOnOne <- function(predictor, other, dataframe){
    # Point A. Create a formula such as Girth ~ Height -1
    formula <- paste0(other, " ~ ", predictor, " - 1")
    # Use the formula in a regression and return the residual.
    resid(lm(formula, dataframe))
}

# Eliminate the specified predictor from the dataframe by
# regressing all other variables on that predictor
# and returning a data frame containing the residuals
# of those regressions.
eliminate <- function(predictor, dataframe){
    # Find the names of all columns except the predictor.
    others <- setdiff(names(dataframe), predictor)
    # Calculate the residuals of each when regressed against the given predictor
    temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
    # sapply returns a matrix of residuals; convert to a data frame and return.
    as.data.frame(temp)
}

trees <- datasets::trees
trees <- cbind(Constant=rep(1.0, nrow(trees)), trees)

fit <- lm(Volume ~ Girth + Height + Constant - 1, trees)
trees2 <- eliminate("Girth", trees)
head(trees2)
fit2 <- lm(Volume ~ Height + Constant - 1, trees2)
lapply(list(fit, fit2), coef)
lm(formula = Volume ~ Constant - 1, data = eliminate("Height", trees2))


all <- lm(Fertility ~ ., swiss)
summary(all)
summary(lm(Fertility ~ Agriculture, swiss))
cor(swiss$Examination, swiss$Education)
cor(swiss$Agriculture, swiss$Education)

makelms <- function(){
    # Store the coefficient of linear models with different independent variables
    cf <- c(coef(lm(Fertility ~ Agriculture, swiss))[2], 
            coef(lm(Fertility ~ Agriculture + Catholic,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination +Infant.Mortality, swiss))[2])
    print(cf)
}
makelms()

ec <- swiss$Examination + swiss$Catholic
efit <- lm(Fertility ~ . + ec, swiss)
all$coefficients - efit$coefficients
    
# Regressor generation process 1.
rgp1 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducability
    set.seed(4321)
    # Point A:
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    # Point B:
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}

# Regressor generation process 2.
rgp2 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducability
    set.seed(4321)
    # Point C:
    x1 <- rnorm(n)
    x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
    x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
    # Point D:
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}

data(InsectSprays); 

boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")

sA <- InsectSprays$count[InsectSprays$spray=="A"]
sB <- InsectSprays$count[InsectSprays$spray=="B"]
sC <- InsectSprays$count[InsectSprays$spray=="C"]
sD <- InsectSprays$count[InsectSprays$spray=="D"]
sE <- InsectSprays$count[InsectSprays$spray=="E"]
sF <- InsectSprays$count[InsectSprays$spray=="F"]

summary(InsectSprays[,2])
sapply(InsectSprays, class)
fit <- lm(count ~ spray, InsectSprays)
summary(fit)$coef
est <- summary(fit)$coef[,1]
nfit <- lm(count ~ spray - 1, InsectSprays)
summary(nfit)$coef
spray2 <- relevel(InsectSprays$spray, "C")
fit2 <- lm(count ~ spray2, InsectSprays)
summary(fit2)$coef

# spray2B t-value
(fit$coef[2] - fit$coef[3])/1.6011

# hunger dataset
fit <- lm(Numeric ~ Year, hunger)
summary(fit)$coef
lmF <- lm(Numeric[Sex=='Female'] ~ Year[Sex=='Female'], hunger)
lmM <- lm(Numeric[Sex=='Male'] ~ Year[Sex=='Male'], hunger)

plot(hunger$Year,hunger$Numeric,type="n")
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Female")*1+125))
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,col="blue",lwd=3)
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,col="red",lwd=3)

lmBoth <- lm(Numeric ~ Year + Sex, hunger)
summary(lmBoth)

plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Female")*1+125))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2]),col="blue",lwd=3)

lmInter <- lm(Numeric ~ Year + Sex + Sex*Year, hunger)
summary(lmInter)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+125))
abline(c(lmInter$coeff[1],lmInter$coeff[2]),col="red",lwd=3)
abline(c(lmInter$coeff[1] + lmInter$coeff[3],lmInter$coeff[2] +lmInter$coeff[4]),col="blue",lwd=3)

# Outliers

local({
    set.seed(13121098)
    n <- 50
    x <- rnorm(n, sd=.5)
    y <- x + rnorm(n, sd=.3)
    out1 <<- data.frame(y=c(5,y), x=c(0,x))
    out2 <<- data.frame(y=c(0,y), x=c(5,x))
})

local({
    fit.1 <- lm(y ~ x, out1[-1,])
    fit <- lm(y ~ x, out1)
    plot(c(-3, 6), c(-3, 6), type='n', xlab="x", ylab="y", main="Outlier with minor influence")
    points(y ~ x, out1, pch=21, bg="lightblue")
    points(out1[1,2], out1[1,1], cex = 2, bg = "darkorange", col = "black", pch = 21)
    text(out1[1,2], out1[1,1]+.25, "Outlier", pos=3)
    abline(fit.1, lwd=2)
    abline(fit, col='darkorange', lwd=4, lty=2)
    legend('bottomright', c("Fit with outlier omitted", "Fit with outlier included"), lty=1:2, col=c("black", "darkorange"), lwd=c(2,4))
})

local({
    fit.1 <- lm(y ~ x, out2[-1,])
    fit <- lm(y ~ x, out2)
    plot(c(-3, 6), c(-3, 6), type='n', xlab="x", ylab="y", main="Outlier with major influence")
    points(y ~ x, out2, pch=21, bg="lightblue")
    points(out2[1,2], out2[1,1], cex = 2, bg = "darkorange", col = "black", pch = 21)
    text(out2[1,2], out2[1,1]+.25, "Outlier", pos=3)
    abline(fit.1, lwd=2)
    abline(fit, col='darkorange', lwd=4, lty=2)
    legend('bottomright', c("Fit with outlier omitted", "Fit with outlier included"), lty=1:2, col=c("black", "darkorange"), lwd=c(2,4))
})

local({
    plot(c(-3, 6), c(-3, 6), type='n', xlab="x", ylab="y", main="Influence")
    points(y ~ x, out2, pch=21, bg="lightblue")
    points(out2[1,2], out2[1,1], cex = 2, bg = "darkorange", col = "black", pch = 21)
    text(out2[1,2], out2[1,1]-.25, "Outlier", pos=1)
    abline(fitno, lwd=2)
    abline(fit, col='darkorange', lwd=4, lty=2)
    segments(out2[1,"x"]-.1, out2[1,"y"], out2[1,"x"]-.1, predict(fitno, out2[1,]), lty=3, lwd=3)
    segments(out2[1,"x"]+.1, out2[1,"y"], out2[1,"x"]+.1, fit$fitted.values[1], lty=3, lwd=3, col="darkorange")
    legend('bottomright', c("Fit with outlier omitted", "Fit with outlier included"), lty=1:2, col=c("black", "darkorange"), lwd=c(2,4))
})

fit <- lm(y ~ x, out2)
# residuals vs. fitted values
plot(fit, which=1)

fitno <- lm(y ~ x, out2[-1,])
plot(fitno, which=1)

coef(fit) - coef(fitno)
# matches to the first row in:
head(dfbeta(fit)) 

# influence calculation
resno <- out2[1,'y'] - predict(fitno, out2[1,]) # actual y minus predicted y based on model without outlier
1-resid(fit)[1]/resno # resid(fit)[1] - residual for outlier, should be small. 
# 0.6311547 
head(hatvalues(fit)) # measure of influence for each row: near 1 indicates high influence

# standardized residuals
sigma <- sqrt(deviance(fit)/df.residual(fit)) # deviance = residual sum of squares
rstd <- resid(fit)/(sigma*sqrt(1 - hatvalues(fit))) #  multiply by sqrt(1-hatvalues(fit)) to estimate std of individual samples
head(cbind(rstd, rstandard(fit)))
plot(fit, which = 3) # Scale-Location plot shows the square root of standardized residuals against fitted values
plot(fit, which = 2) # QQ plot of standardized residuals against normal with constant variance

# studentized residuals
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
#  Studentized residual for the outlier sample
resid(fit)[1]/(sigma1*sqrt(1 - hatvalues(fit)[1]))
#-7.664261 
# same as value #1 in here:
head(rstudent(fit))

# cook's distance
dy <- predict(fitno, out2) - predict(fit, out2)
sum(dy^2)/(2*sigma^2)
# [1] 23.07105
# same as:
cooks.distance(fit)[1]
plot(fit, which=5)
