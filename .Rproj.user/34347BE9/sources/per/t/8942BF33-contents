library(MASS)
savings <- read.table("savings.data")
g <- lm(sav ~ p15 + p75 + inc + gro, data=savings)
boxcox(g, plotit=T)
boxcox(g, plotit=T, lambda=seq(0.5,1.5,by=0.1))
# We can see that there is no good reason to transform.

gala <- read.table("gala.data")
gg <- lm(Species~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
boxcox(gg, plotit=T)
boxcox(gg, lambda=seq(0.0,1.0,by=0.05), plotit=T)
# A square root is also a possibility as this falls just within the confidence intervals. Certainly there is a strong need to transform.


# Transforming predictors
g <- lm(sav ~ p15 + p75 + gro + inc, data=savings)
g2 <- update(g, . ~ . + I(gro*log(gro))) # Add gro*log(gro) to the model
g3 <- update(g, . ~ . + I(p15*log(p15))) ;  summary(g3) # Now see if p15 should be transformed. 
