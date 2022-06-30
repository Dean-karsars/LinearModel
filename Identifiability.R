# Read the data into R
saving.x <- read.table("saving.x",header=T) 
p15 <- saving.x[,1]; p75 <- saving.x[,2]; inc <- saving.x[,3]; gro <- saving.x[,4]; sav <- saving.x[,5] 
#We now create a new variable
pa <- 100-p15-p75 # calculate pa
g <- lm(sav~pa+p15+p75+inc+gro) # fit a model with pa and all other predictors
summary(g) # take a look of the fitted model
x <- model.matrix(g) # form the X matrix
xtx <- t(x) %*% x # compute XTX
solve(xtx) # XTX cannot be inverted
#If we didn't know which linear combination was causing the trouble, how would we find out? An eigen-decomposition of XTX can help:
e = eigen(xtx)
e$values
signif(e$vectors,3)
#lack of identifiablity is obviously a problem but it's easy to identify and work around
#more problematic are cases where we are "close to unidentifiability".
#To demonstrate this, suppose we add a small random perturbation from U[-0.005,0.005] to the 3rd decimal place of variable pa, where U denotes the uniform distribution
pae <- pa+0.001*(runif(50)-0.5) # add the small perturbation to pa, the "runif" command generate Uniform[0, 1] sudo random numbers
ge <- lm(sav~pae+p15+p75+inc+gro) # now, fit the same model again
summary(ge, cor=T) # take a look of the fitted model