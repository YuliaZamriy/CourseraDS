load("./ravensData.rda")
head(ravensData)

lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef

library(manipulate)
x <- seq(-10, 10, length = 1000)
manipulate(
    plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
         type = "l", lwd = 3, frame = FALSE),
    beta1 = slider(-2, 2, step = .1, initial = 2),
    beta0 = slider(-2, 2, step = .1, initial = 0)
)

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)

plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")

exp(logRegRavens$coeff)
exp(confint(logRegRavens))
anova(logRegRavens,test="Chisq")

library(openintro)

data(email)
head(email)

logspam <- glm(email$spam ~ email$to_multiple,family="binomial")
summary(logspam)
spam0 <- exp(logspam$coefficients[1])/(1 + exp(logspam$coefficients[1]))
spam1 <- exp(logspam$coefficients[1] + logspam$coefficients[2])/(1 + exp(logspam$coefficients[1] + logspam$coefficients[2]))

par(mfrow=c(1,1))
p <- seq(0, 1, length = 1000)
logp <- log(p/(1-p))
plot(logp, p, type = 'l')
points(x = logspam$coefficients[1], y = spam0, pch = 16, col = "red")
points(x = logspam$coefficients[1] + logspam$coefficients[2], y = spam1, pch = 16, col = "red")

logspam2 <- glm(spam ~ ., data = email, family="binomial")
summary(logspam2)

logspam3 <- glm(spam ~ 
                    to_multiple +
                    winner +
                    format +
                    re_subj +
                    exclaim_subj +
                    cc +
                    attach +
                    dollar +
                    inherit +
                    password, 
                data = email, family="binomial")
summary(logspam3)

logspam4 <- glm(spam ~ 
                    to_multiple +
                    winner +
                    format +
                    re_subj +
                    attach +
                    password, 
                data = email, family="binomial")
summary(logspam4)

plot(logspam4$fitted.values, email$spam)
plot(logspam4$linear.predictors, logspam4$fitted.values)
dev.off()

str(logspam4$fitted.value)

fit.val.bin <- vector(mode = "numeric", length = length(logspam4$fitted.value))
fit.val.bin[logspam4$fitted.value < 0.1] <- 0.1
fit.val.bin[logspam4$fitted.value >= 0.1 & logspam4$fitted.value < 0.2] <- 0.2
fit.val.bin[logspam4$fitted.value >= 0.2 & logspam4$fitted.value < 0.3] <- 0.3
fit.val.bin[logspam4$fitted.value >= 0.3 & logspam4$fitted.value < 0.4] <- 0.4
fit.val.bin[logspam4$fitted.value >= 0.4 & logspam4$fitted.value < 0.5] <- 0.5
fit.val.bin[logspam4$fitted.value >= 0.5 & logspam4$fitted.value < 0.6] <- 0.6
fit.val.bin[logspam4$fitted.value >= 0.6 & logspam4$fitted.value < 0.7] <- 0.7
fit.val.bin[logspam4$fitted.value >= 0.7 & logspam4$fitted.value < 0.8] <- 0.8
fit.val.bin[logspam4$fitted.value >= 0.8 & logspam4$fitted.value < 0.9] <- 0.9
fit.val.bin[logspam4$fitted.value >= 0.9] <- 1

match_table <- data.frame(cbind(email$spam, logspam4$fitted.value, fit.val.bin))
colnames(match_table) <- c('spam', 'prob', 'prob_bin')
match_table_aggr <- aggregate(spam ~ prob_bin, data = match_table, mean)

plot(match_table_aggr$prob_bin, match_table_aggr$spam)
str(match_table)
gg <- ggplot(data = match_table_aggr, aes(prob_bin, spam))
    gg + geom_bar(stat = "identity")
gg <- ggplot(data = match_table, mapping = aes(x = prob, y = factor(spam)))
    gg + geom_jitter(width = 0, height = 0.2, color = "blue", alpha = 0.2, size = 2) +
         scale_y_discrete("Spam", labels = c("0" = "Not Spam", "1" = "Spam"))
    
# Open Intro code:    
e <- email
e$cc       <- ifelse(email$cc > 0, 1, 0)
e$attach   <- ifelse(email$attach > 0, 1, 0)
e$dollar   <- ifelse(email$dollar > 0, 1, 0)
e$inherit  <- ifelse(email$inherit > 0, 1, 0)
e$password <- ifelse(email$password > 0, 1, 0)

g <- glm(spam ~ 
             to_multiple + 
             winner + 
             format + 
             re_subj + 
             attach + 
             password, # +
         # dollar + inherit + num_char + line_breaks + exclaim_mess + exclaim_subj,
         data = e, family = binomial)

summary(g)

p  <- predict(g, type = "response")
p. <- p

noise <- rnorm(nrow(e), sd = 0.08)
plot(p, e$spam + noise,
     xlim = 0:1,
     ylim = c(-0.5, 1.5),
     axes = FALSE,
     xlab = "Predicted probability",
     ylab = "",
     col = fadeColor(COL[1], "22"),
     pch = 20)
axis(1)
axis(2,
     at = c(0,1),
     labels = c("0 (not spam)", "1 (spam)"))

# Natural Splines
install.packages("splines")
library(splines)
ns1 <- 7
plot(p, e$spam+noise/5,
     type = "n",
     xlim = 0:1,
     ylim = c(-0.07, 1.07),
     axes = FALSE,
     xlab = "Predicted probability",
     ylab = "")
par(las = 0)
mtext("Truth", 2, 5.5)
par(las = 1)
rect(0, 0, 1, 1,
     border = COL[6],
     col = "#00000000",
     lwd = 1.5)
lines(0:1, 0:1,
      lty = 2,
      col = COL[6],
      lwd = 1.5)
arrows(0.83, 0.57,
       0.8, 0.785,
       length = 0.07)
text(0.88, 0.48,
     "What we expect if\nthe logistic model\nis reasonable",
     cex = 0.75)
points(p, e$spam + noise / 5,
       col = fadeColor(COL[1], "18"),
       pch = 20)
axis(1)
at <- seq(0, 1, length.out = 6)
labels <- c("0 (not spam)",
            "0.2  ",
            "0.4  ",
            "0.6  ",
            "0.8  ",
            "1 (spam)")
axis(2, at, labels)
g1 <- lm(e$spam ~ ns(p, ns1))
p  <- seq(0, max(p), length.out = 200)
Y  <- predict(g1,
              data.frame(ns(p, ns1)),
              se.fit = TRUE)
yb <- Y$fit - 1.96 * Y$se.fit
yt <- rev(Y$fit + 1.96 * Y$se.fit)
polygon(c(p, rev(p)),
        c(yb, yt),
        col = COL[3, 3],
        border = "#00000000")
lines(p, Y$fit, lwd = 2.5)
arrows(0.36, 0.54,
       0.45, 0.52,
       length = 0.07)
text(0.25, 0.6,
     "Locally-estimated\nprobabilities with\nconfidence bounds",
     cex = 0.75)
arrows(0.6, 0.36,
       0.7, 0.61,
       length = 0.07)
text(0.6, 0.27,
     paste("The bounds become wide\nbecause not much data",
           "are found this far right",
           sep = "\n"),
     cex = 0.75)

# Poisson Regression

par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE) 

x <- 0 : 10000
lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

load("./gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)
par(mfrow = c(1, 1))
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)

glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

# Mean-variance relationship
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Model agnostic standard errors
install.packages("sandwich")
library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
confint(glm1)
confint.agnostic(glm1)

# Fitting rates in R

glm2 <- glm(gaData$simplystats ~ julian(gaData$date),
            offset=log(visits+1),
            family="poisson",
            data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)

# breaking a stick

n <- 500
x <- seq(0, 4 * pi, length = n)
y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20)
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

# smoothing the edges

splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
xMat <- cbind(1, x, x^2, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

# Harmonics using linear models
##Chord finder, playing the white keys on a piano from octave c4 - c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
t <- seq(0, 2, by = .001)
n <- length(t)
c4 <- sin(2 * pi * notes4[1] * t)
e4 <- sin(2 * pi * notes4[3] * t)
g4 <- sin(2 * pi * notes4[5] * t)
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1)

plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
##(How you would really do it)
a <- fft(chord)
plot(Re(a)^2, type = "l")

# Swirl

makelms <- function(x1, x2, x3){
    # Simulate a dependent variable, y, as x1
    # plus a normally distributed error of mean 0 and 
    # standard deviation .3.
    y <- x1 + rnorm(length(x1), sd = .3)
    # Find the coefficient of x1 in 3 nested linear
    # models, the first including only the predictor x1,
    # the second x1 and x2, the third x1, x2, and x3.
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
}

# Regressor generation process 1.
rgp1 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducibility
    set.seed(4321)
    # Point A
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    # Point B
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
    # set seed for reproducibility
    set.seed(4321)
    # Point C
    x1 <- rnorm(n)
    x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
    x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
    # Point D
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}

rgp1()
#      x1      x1      x1 
# 0.00110 0.00111 0.00112 

rgp2()
#      x1      x1      x1 
# 0.00110 0.00240 0.00981 

library(car)
head(swiss)
mdl <- lm(data = swiss, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
vif(mdl)
mdl2 <- lm(data = swiss, Fertility ~ Agriculture + Education + Catholic + Infant.Mortality)
vif(mdl2)

# underfitting

simbias <- function(seed=8765){
    # The default seed guarantees a nice histogram. This is the only
    # reason that accepting the default, x1c <- simbias(), is required in the lesson. 
    # The effect will be evident with other seeds as well.
    set.seed(seed) 
    temp <- rnorm(100)
    # Point A
    x1 <- (temp + rnorm(100))/sqrt(2)
    x2 <- (temp + rnorm(100))/sqrt(2)
    x3 <- rnorm(100)
    # Function to simulate regression of y on 2 variables.
    f <- function(k){
        # Point B
        y <- x1 + x2 + x3 + .3*rnorm(100)
        # Point C
        c(lm(y ~ x1 + x2)$coef[2],
          lm(y ~ x1 + x3)$coef[2])
    }
    # Point D
    sapply(1:150, f)
}

# Illustrate the effect of bogus regressors on residual squared error.
bogus <- function(){
    temp <- swiss
    # Add 41 columns of random regressors to a copy of the swiss data.
    for(n in 1:41){temp[,paste0("random",n)] <- rnorm(nrow(temp))}
    # Define a function to compute the deviance of Fertility regressed
    # on all regressors up to column n. The function, deviance(model), computes
    # the residual sum of squares of the model given as its argument.
    f <- function(n){deviance(lm(Fertility ~ ., temp[,1:n]))}
    # Apply f to data from n=6, i.e., the legitimate regressors,
    # through n=47, i.e., a full complement of bogus regressors.
    rss <- sapply(6:47, f)
    # Display result.
    plot(0:41, rss, xlab="Number of bogus regressors.", ylab="Residual squared error.",
         main="Residual Squared Error for Swiss Data\nUsing Irrelevant (Bogus) Regressors",
         pch=21, bg='red')
}

# Plot histograms illustrating bias in estimates of a regressor
# coefficient 1) when an uncorrelated regressor is missing and
# 2) when a correlated regressor is missing.
x1hist <- function(x1c){
    p1 <- hist(x1c[1,], plot=FALSE)
    p2 <- hist(x1c[2,], plot=FALSE)
    yrange <- c(0, max(p1$counts, p2$counts))
    plot(p1, col=rgb(0,0,1,1/4), xlim=range(x1c), ylim=yrange, xlab="Estimated coefficient of x1",
         main="Bias Effect of Omitted Regressor")
    plot(p2, col=rgb(1,0,0,1/4), xlim=range(x1c), ylim=yrange, add=TRUE)
    legend(1.1, 40, c("Uncorrelated regressor, x3, omitted", "Correlated regressor, x2, omitted"),
           fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
}

x1c <- simbias()
apply(x1c, 1, mean)
x1hist()
bogus()

fit1 <- lm(data = swiss, Fertility ~ Agriculture)
fit3 <- lm(data = swiss, Fertility ~ Agriculture + Examination + Education)
anova(fit1, fit3)
# RSS is also:
deviance(fit3)

# calculating F-stat
d <- deviance(fit3)/43
n <- (deviance(fit1) - deviance(fit3))/(45-43)
n/d
pf(n/d, 2, 43, lower.tail = FALSE)
# if residuals are not normal, we can get false small p-value
# test normality (null hypothesis)
shapiro.test(fit3$residuals)

fit5 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic, swiss)
fit6 <- lm(Fertility ~ ., swiss)
anova(fit1, fit3, fit5, fit6)

# Binary

# ravens data
ravenData <- read.csv(file.path(.get_course_path(), 
                                "Regression_Models", "Binary_Outcomes", "ravens_data.csv"))
ravenData <- ravenData[order(ravenData$ravenScore), 1:3]
rownames(ravenData) <- NULL

boxplot(ravenScore ~ ravenWin, ravenData, col=0x96, lwd=3, horizontal=TRUE, 
        col.lab="purple", col.main="purple", xlab="Ravens' Score", 
        main="Ravens' Wins and Losses vs Score")
abline(v=23, lwd=5, col="purple")
plot(c(3,23,29,55), c(0.5, 0.5, 1.0, 1.0), type='l', lwd=5, col="purple", col.lab="purple", ylim=c(0.25,1),
     xlab="Ravens' Score", ylab="Probability of a Ravens win", col.main="purple",
     main="Crude estimate of Ravens' win probability\nvs the points which they score.")

mdl <- glm(ravenWinNum ~ ravenScore, binomial ,ravenData)

plot(c(3,23,29,55), c(0.5, 0.5, 1.0, 1.0), type='l', lwd=5, col="purple", col.lab="purple", ylim=c(0.25,1),
     xlab="Ravens' Score", ylab="Probability of a Ravens win", col.main="purple",
     main="Ravens' win vs score probabilities: GLM maximum likelihood estimates\ncompared to crude estimates.")
lines(mdl$data$ravenScore, mdl$fitted.values, lwd=5, col="black")
legend('bottomright', c("Crude estimates", "GLM maximum likelihood estimates"), lwd=5, lty=1,
       col=c("purple", "black"))

lodds <- predict(mdl, data.frame(ravenScore = c(0,3,6)))
exp(lodds)/(1+exp(lodds))
summary(mdl)
# conf interval for odds
exp(confint(mdl))
# Analysis of Deviance
anova(mdl)
# threshold for deviance of ravenScore
qchisq(0.95, 1)
# we reject the null hypothesis that slope is zero
mdl0 <- glm(ravenWinNum ~ 1, binomial, ravenData)

# counts

# checking if lambda (mean) is equal to variance
var(rpois(1000, 50))

par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE, xlab="Count", ylab="Probability of Count", main="lambda = 2")
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE, xlab="Count", ylab="Probability of Count", main="lambda = 10")
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE, xlab="Count", ylab="Probability of Count", main="lambda = 100") 
par(mfrow = c(1, 1))

hits <- read.csv(file.path(.get_course_path(), "Regression_Models",
                           "Count_Outcomes", "leekGroupData.csv"), as.is=TRUE)
hits[,"date"] <- as.Date(hits[,"date"])

plot(visits ~ date, hits, main="Visits per Day to the Leek Group Website.", xlab="Date", ylab="Visits", pch=21, bg='green')
lines(hits$date, predict(loess(visits ~ julian(date), hits, span=1.5)), lwd=5, col="black")

as.integer(head(hits[,'date']))

mdl <- glm(visits ~ date, poisson, hits)
plot(visits ~ date, hits, 
     main="Visits per Day to the Leek Group Website and\nMean Visits per Day as Estimated by Poisson Regression.",
     xlab="Date", ylab="Visits", pch=21, bg='green')
lines(hits$date, mdl$fitted.values, lwd=5, col="black")

summary(mdl)
exp(confint(mdl, 'date'))

idx <- 1:60
par(mfrow=c(1,2))
plot(visits ~ date, hits[idx,], 
     main='"Zero Inflation" 2011',
     xlab="Date", ylab="Visits", pch=21, bg='green')
lines(hits$date[idx], mdl$fitted.values[idx], lwd=5, col="black")
points(as.Date("2011-01-10"), 5, cex=12, lwd=5)
text(as.Date("2011-01-5"), 9, "Zero Inflation", pos=4)
plot(hits$date, hits$visits-mdl$fitted.values, pch=21, bg='green', main="Variance != Mean?", xlab="Date", ylab="Visits over Average")
lines(hits$date, sqrt(mdl$fitted.values), lwd=5, lty=2, col='black')
lines(hits$date, -sqrt(mdl$fitted.values), lwd=5, lty=2, col='black')
rm(idx)
par(mfrow=c(1,1))

plot(visits ~ date, hits, pch=21, bg='lightgreen', main="Bursts of Popularity", xlab="Date", ylab="Visits")
points(simplystats ~ date, hits, pch=21, bg='black')
lines(simplystats ~ date, hits, lwd=3)
legend('topleft', c("Visits", "Visits from Simply Statistics"), pch=21, pt.bg=c("lightgreen", "black"), bg="white")

# find the date for max visits
which.max(hits[,'visits'])
# [1] 704
hits[704,]
#           date visits simplystats
# 704 2012-12-04     94          64
# Can the difference, 94-64=30 visits, be attributed to normal traffic as estimated by our model?

lambda <- mdl$fitted.values[704]
qpois(.95, lambda)
# [1] 33
# 95% of the time we would see 33 or fewer visits, hence 30 visits would not be rare according to our model.

# model for  for the proportion of visits from Simply Statistics
# offset fixes teh coefficient of the offset to 1
mdl2 <- glm(formula = simplystats ~ date, family = poisson, data = hits, offset = log(visits + 1))
qpois(.95, mdl2$fitted.values[704])
# [1] 47
