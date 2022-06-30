# Residual Plots
savings <- read.table("savings.data") # read the data into R
g <- lm(sav ~ p15 + p75 + inc + gro, data=savings) # fit the model with sav as the response and the rest variables as predictors

# Let's plot raw residuals, studentized residuals, and jacknife residuals
rstu = rstandard(g) #studentized residuals
rjack = rstudent(g) #jacknife residuals
par(mfrow = c(2,2)) #splite the graphic windown into 2*2
plot(g$residuals, ylab = 'raw materials'); plot(rstu, ylab = 'studentized residuals');  plot(rjack, ylab = 'jacknife residuals')
# First, the residuals vs. fitted plot and the abs(residuals) vs. fitted plot.
par(mfrow = c(1,2))
plot(x = g$fitted.values, y = g$residuals, ylab = 'raw residuals', xlab = 'fitted value'); abline(h = 0)
plot(x = g$fitted.values, y = abs(g$residuals), ylab = '|residuals|', xlab = 'fitted value')

# Generate four panels to show 
# 1.Constant Variance
# 
# 2.Srong non-constant variance
# 
# 3.Mild non-constant variance
# 
# 4.Non-linearity

# Constant Variance
par(mfrow = c(3,3))
for (i in 1:9){
  plot(1:50, rnorm(50), abline(h = 0))
}

# Srong non-constant variance
for (i in 1:9){
  plot((1:50), (1:50) * rnorm(50), abline(h = 0))
}

# Mild non-constant variance
for (i in 1:9){
  plot((1:50), sqrt((1:50)) * rnorm(50), abline(h = 0))
}

# Non-linearity
for(i in 1:9) {
  plot(1:50,cos((1:50)*pi/25)+rnorm(50)); abline(h=0)
}

# Now let's go back to saving data, and look at the residuals against predictor plots:
par(mfrow = c(2, 2)) # split the graphic window into 2x3 subwindows
plot(x = savings$p15, y = g$residuals, xlab = 'p15', ylab = 'residuals'); abline(h = 0)# residual plot - residual against predictor p15
plot(x = savings$p75, y = g$residuals, xlab = 'p75', ylab = 'residuals'); abline(h = 0) # residual plot - residual against predictor p75
plot(x = savings$inc, y = g$residuals, xlab = 'inc', ylab = 'residuals'); abline(h = 0) # residual plot - residual against predictor inc
plot(x = savings$gro, y = g$residuals, xlab = 'gro', ylab = 'residuals'); abline(h = 0) # residual plot - residual against predictor gro

# It seems the variance decrease when income increase. Let's further explore it: 
par(mfrow=c(1,1))
plot(savings$inc, g$res, xlab="income", ylab="residuals", type="n") # draw a "no plotting" plot of residual against income
points(savings$inc[savings$p15 <35], g$res[savings$p15 <35], pch=1) # label the points with p15<35 as circles
points(savings$inc[savings$p15 >35], g$res[savings$p15 >35], pch=4) # label the points with p15>35 as cross

# A quick way to get various diagnostic plots for the regression is:
par(mfrow = c(2,2)); plot(g) # some default diagnostic plots in R 
 
# Non-constant variance
# Consider the residual vs. fitted plot for the Galapagos data in previous lab.
gala <- read.table("gala.data") # read the data into R
gg <- lm(Species~Area+Elevation+Scruz+Nearest+Adjacent, data=gala) # fit a model with Species as response and other variables as predictors
plot(gg$fit,gg$res,xlab="Fitted",ylab="Residuals",main="untransformed response") # draw the plot of residual against y-hat

# From the shape, we guess that a square root transformation (why?) will give us constant variance:
gs = lm(sqrt(Species)~Area+Elevation+Scruz+Nearest+Adjacent, data=gala)
plot(gs$fit,gs$res,xlab="Fitted",ylab="Residuals",main="squared root response")

# Curvature in the mean of residuals --- added variable plot and partial residual plot
# We use the savings dataset as an example again.
# First we construct a added variable plot for p15:
d <- lm(sav ~ p75 + inc + gro, data=savings)$res # calculate the residual of the model with sav as response and p75, inc, gro as predictors
m = lm(p15 ~ p75 + inc + gro, data=savings)$res #calculate the residual of the model with p15 as response and p75, inc, gro as predictors
plot(m, d, xlab="pop15 residuals", ylab="saving residuals", main="added variable plot") # draw the 1st residuals against the 2nd residuals)

# Compare the slope on the plot to the original regression and show the line on the plot:
lm(d ~ m)$coef# get the intercept and slope of the fitted line with 1st residual as response and 2nd residual as predictor
g$coef # the coefficients of the fitted model with sav as the response and the other variables as predictors
abline(0,g$coef['p15'])  # add the fitted line to the plot

# A partial residual plot is easier to do:
plot(savings$p15, g$residuals + g$coefficients['p15'] * savings$p15,  xlab="pop15", ylab="saving (adjusted)", main="partial residual")
abline(0, g$coef['p15']) # add the fitted line to the plot


# Q-Q plots 
# Q-Q plots can be used to check the assumptions of normality.
par(mfrow = c(2,2))
qqnorm(g$residuals, ylab = 'raw residuals') # draw the normal probability plot for raw residuals
qqline(g$residuals) # Add a straight line to guide the examination
qqnorm(rstu, ylab = 'studentized residuals')# draw the normal probability plot for studentized residuals
qqline(rstu)

# Note that histograms and box-plots are not as sensitive for checking normality:
par(mfrow=c(1, 2)) # split the graphic window into 1x2 subwindows
hist(g$res, 10) # a histogram plot for raw residuals
boxplot(g$res, ylab="Boxplot of savings residuals") # a box-plot for raw residuals

# We can get an idea of the variation to be expected in Q-Q plots in the following experiment. I generate data from different distributions:
#   
# 1.Normal
# 2.Lognormal - an example of a skewed distribution
# 3.Cauchy - an example of a long-tailed (platykurtic) distribution
# 4.Uniform - an example of a short-tailed (leptokurtic) distribution

# Normal
par(mfrow = c(3,3))
for(i in 1:9){
  x = rnorm(50) 
  qqnorm(x) 
  qqline(x)
}

# Lognormal
for(i in 1:9){
  x = exp(rnorm(50)) 
  qqnorm(x) 
  qqline(x)
}

# Cauchy
for(i in 1:9) {x<-rcauchy(50); qqnorm(x); qqline(x)}

# uniform
> for(i in 1:9) {x<-runif(50); qqnorm(x); qqline(x)}

# Correlated errors
air <- read.table("air.data",header=T)  # read the data into R
pairs(air) # draw pair-wise scatter plots
ga <- lm(ozone ~ radiation + temperature + wind, data=air) # fit the model with ozone as response and other variables as predictors
summary(ga) # take a look of the fitted model
plot(ga$res) # draw the plot of residual against index
plot(ga$res[-111],ga$res[-1])  # plot εt-hat against εt+1-hat

# We can compute the Durbin-Watson statistic:
library(lmtest)
dwtest(ozone ~ radiation + temperature + wind, data=air)





