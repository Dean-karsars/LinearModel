p <- read.table("phy.data", header=T) # note the extra argument to cope with not having row labels
x <- p[, 2] # extract the energy variable, the predictor
y <- p[, 3] # extract the crossx variable, the response
w <- 1/p[, 4]^2 # the weights
g = lm(y ~ x, weights = w)
summary(g)
#Do F-test by hand
wm =  sum(w*y)/sum(w)
(sum(w*(y-wm)*(y-wm)) - sum(w*g$res*g$res)) /(sum(w*g$res*g$res)/8)
#Weighted least square can be regarded as regressing sqrt(wi)xi on sqrt(wi)yi using ordinary least square:
ym<-sqrt(w)*y # calculate sqrt(wi)yi
xm<-sqrt(w)*x  # calculate sqrt(wi)xi
g2 = lm(ym~sqrt(w) + xm -1)
anova(lm(ym ~ sqrt(w) - 1), g2)



