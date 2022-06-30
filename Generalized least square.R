data(longley) # load the data into R
emp <- longley[,7] # extract the Employed variable
gnp <- longley[,2] # extract the GNP variable
pop <- longley[,5] # extract the Population variable
g <- lm(emp~gnp+pop) # fit the model
summary(g,cor=T) # summary of the fitted model
#Note that the model is built under the assumption that errors are uncorrelated
#In data collected over time such as this, successive errors could be correlated.
#Assume cor(εt, εt+1)=0.5, all other off-diagonal elements are zero
x = model.matrix(g)
#start to construct the Σ matrix
sig = diag(16)
sig[abs(row(sig) - col(sig)) == 1] = 0.5
sigi = solve(sig) # the Σ-1
xtxi = solve(t(x) %*% sigi %*% x) # calculate(XTΣ-1X)-1
beta = xtxi %*% t(x) %*% sigi %*% emp

#The simplest way to model the correlation between successive errors is the autoregressive (AR) form:εt+1 = ρεt + δt, where δt~N(0,τ2).
#Under the AR(1) assumption,Σij=ρ|i-j|.for simplicity, lets assume we know that ρ=0.31041.
#start to construct the Σ matrix
sigma = diag(16)
sigma = 0.31041^abs(row(sigma) - col(sigma))
#We will try another way, regressing S-1y on S-1x, to get the results:
sm = chol(sigma)# calculate ST, where Σ=SST. the "chol" command compute the Choleski factorization
smi = solve(t(sm))
sx = smi %*% x
sy = smi %*% emp
g2 = lm(sy ~ sx - 1)
summary(g2, cor=T)
cor((t(sm) %*% g2$residuals)[-1], (t(sm) %*% g2$residuals)[-16]) #Our initial estimate is 0.31041, but once we fit our GLS model we'd need to re-estimate ρ  as:
#And then, compute the model again with ρ =0.3564162

# the process would be iterated until convergence

#This is cumbersome.

#A more convenient approach may be found in the "nlme" package, which contains a GLS fitting function. The approach is to

# model Σ and

#jointly estimate the regression parameters and parameters in Σ using a likelihood-based method.

# We will use the approach to fit the model below (remember to add the "nlme" package if you haven't done it):
library(nlme)
year = longley[,6]
g3 = gls(emp ~ gnp + pop, correlation = corAR1(form = ~year))
