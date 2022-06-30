# Location and/or Scale Changes
savings <- read.table("savings.data")
g <- lm(sav ~ p15 + p75 + inc + gro, data=savings)
summary(g)
# The coefficient for income is rather small --- let's measure income in thousands of dollars instead and refit:
g = lm(sav ~ p15 + p75 + I(inc/1000) + gro, data=savings)
summary(g)
# One rather thorough approach to scaling is to convert all the variables to standard unit (mean=0 and variance=1) using the "scale" command:
scsav = data.frame(scale(savings))
cov(scsav)
g <- lm(sav~., data=scsav)
summary(g)
# As may be seen, the intercept is zero. This is because the regression plane always runs through the point of the averages, which (because of the centering) is now at the origin.
# 
# Such scaling has the advantage of putting all the predictors and the response on a comparable scale, which makes comparisons simpler.
# 
# It also allows the coefficients to be viewed as kind of partial correlation --- the values will always be between -1 and 1.
# 
# It also avoids some numerical problems that can arise when variables are of very different scales.
# 
# The downside of this scaling is that the regression coefficients now represent the effect of a one standard unit increase in the predictor on the response in standard units --- this might not always be easy to interpret.


# Polynomial Regression
savings <- read.table("savings.data")
plot(savings$gro, savings$sav)
# First fit a linear model:
summary(lm(sav ~ gro, data=savings))
# P-value of gro is significant so move on to a quadratic term:
summary(lm(sav ~ gro + I(gro^2), data=savings))
# Again p-value of gro^2 is significant so move on to a cubic term:
summary(lm(sav ~ gro + I(gro^2) + I(gro^3), data=savings))
# You have to refit the model each time a term is removed. Orthogonal polynomials save some time in this process - the "poly()" function in R constructs these:
g = lm(sav ~ poly(gro, 4), data = savings)
summary(g, correlation = TRUE) #We can see that the correlation between the parameter estimates are zero.

summary(lm(sav ~ poly(gro,inc, degree=2), data=savings))


# Broken Stick Regression
# In the analysis of the savings data, we observed that there were two groups in the data
# 
# We might want to fit a different model to the two parts.
g1 <- lm(sav~p15, data=savings, subset=(p15<35))
g2 = lm(sav~p15, data=savings, subset=(p15>35))
plot(savings$p15, savings$sav, xlab = "Pop'n under 15", ylab="savings rate")
abline(v=35, lty=5)
segments(20, g1$coef[1]+g1$coef[2]*20, 35, g1$coef[1]+g1$coef[2]*35)
segments(48, g2$coef[1]+g2$coef[2]*48, 35, g2$coef[1]+g2$coef[2]*35)

# A possible objection to this subsetted regression fit is that the two parts of the fit do not meet at the join.
# 
# If we believe the fit should be continuous as the predictor varies, then this is unsatisfactory.
# 
# One solution is to use broken line regression:


d = function(x) {
  ifelse(x<35, 0, 1)
}
gb <- lm(sav ~ p15+I((p15-35)*d(p15)), data=savings)
x <- seq(20, 48, by=1)
py <- gb$coef[1]+gb$coef[2]*x+gb$coef[3]*((x-35)*d(x))
lines(x, py, lty=2)

# Regression Spline
# Let's see how the regression spline method perform on a constructed example.
# 
# Suppose we know the true model is: 
# 
# y=sin3(2πx3)+ε,  ε~N(0, 0.01).
# 
# The advantage of using simulated data is that we can see how close our methods come to the truth. We generate the data and display it in plots:
funky = function(x) {
  y = sin(2 * pi * x^3)^3
  return(y)
}
x = seq(0, 1, by = 0.01)
y = funky(x)  + rnorm(101 ,mean = 0, sd = 0.1)
matplot(x,cbind(y,funky(x)),type="pl",ylab="y",pch=18,lty=1,main="True Model")
# We see how an orthogonal polynomial bases of orders 4, 9, and 12 do in fitting this data:
g4 <- lm(y~poly(x,4))
g9 <- lm(y~poly(x,9))
g12 <- lm(y~poly(x,12))
matplot(x,cbind(y,g4$fit,g9$fit,g12$fit),type="plll",ylab="y",pch=18,lty=c(1,2,3),main="orthogonal polynomials")
# We see that order 4 is a clear lack-of-fit.
# 
# Order 12 is much better although the fit is too wiggly in the first section and misses the point of inflection.
#We now create the B-spline basis. You need to have three additional knots at the start and end to get the right basis. I have chosen to the knot locations to put more in regions of greater curvature. I have used 12 basis functions for comparability to the orthogonal polynomial fit.
library(splines)
knots = c(0,0,0,0,0.2,0.4,0.5,0.6,0.7,0.8,0.85,0.9,1,1,1,1)
bx = splineDesign(knots, x)
gs = lm(y ~ bx)
matplot(x,cbind(y,funky(x),gs$fit),type="pll",ylab="y",pch=18,lty=1,main="Spline fit")

# LOWESS and LOESS
ls1 <- lowess(x, y, f=0.05)
ls2 <- lowess(x, y, f=0.2)
ls3 <- lowess(x, y, f=0.4)
ls4 <- lowess(x, y, f=0.6)
matplot(x,cbind(y,ls1$y,ls2$y,ls3$y,ls4$y),type="plll",ylab="y",pch=18,lty=c(1,2,3),main="LOWESS fit")
# The LOESS smoother in R:
loe1 <- loess(y~x, span=0.1)
loe2 <- loess(y~x, span=0.3)
loe3 <- loess(y~x, span=0.5)
loe4 <- loess(y~x, span=0.7)

matplot(x,cbind(y,loe1$fit,loe2$fit,loe3$fit,loe4$fit),type="pllll",ylab="y",pch=18,lty=c(1,2,3,4),main="LOESS fit")








