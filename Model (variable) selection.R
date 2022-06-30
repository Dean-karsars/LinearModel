# Tesging-based procedure
data(state)
statedata <- data.frame(state.x77, row.names=state.abb, check.names=T)
g <- lm(Life.Exp ~ ., data=statedata)
summary(g)
# Which predictors should be included - can you tell from the p-values? Looking at the coefficients, can you see what operation would be helpful? Does the murder rate decrease life expectancy - that's obvious a priori but how should these results be interpreted?
# We illustrate the backward method --- at each step, we remove the predictor with the largest p-value over 0.05. [Note: In R, the function "mle.stepwise()" in "wle" library can automatically do the job. See the "help(mle.stepwise)" page in R for details.]
g <- update(g, .~. - Area); summary(g)
g <- update(g, .~. - Illiteracy); summary(g)
g <- update(g, .~. - Income); summary(g)
g <- update(g, .~. - Population); summary(g)
summary(lm(Life.Exp~Illiteracy+Murder+Frost, statedata))

# Criterion-based procedure
g <- lm(Life.Exp ~ ., data=statedata)
step(g)
library(leaps)
x <- statedata[,-4]
y <- statedata[,4]
gcp <- leaps(x,y)
# It's usual to plot the Cp data against p:
plot(gcp$size, gcp$Cp, xlab="p", ylab="Cp"); abline(0,1) 
### 查笔记！！！！