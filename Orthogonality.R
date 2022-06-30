#read in the data
odor <- read.table("odor.data", header=T)
#remember to include the constant term
x = as.matrix(cbind(rep(1,15), odor[, -1]))
#calculate XTX
t(x) %*% x
# change temp to its original scale
x[, 2] <- odor[, 2]*40+80
# calculate XTX
t(x) %*% x
#Now fit the model
g <- lm(odor~temp+gas+pack, data=odor) 
summary(g)
#Now, let us examine the effect of dropping variables when orthogonality exists.
gl = lm(odor ~ gas + pack, data = odor)
summary(gl)
#Can orthogonality still hold if we fit a more complicate model?
summary(lm(odor~temp+gas+pack+I(temp*gas)+I(temp*pack)+I(gas*pack), data=odor), cor=T) # fit model 1
summary(lm(odor~temp+gas+pack+I(temp*gas)+I(temp*pack)+I(gas*pack)+I(temp^2)+I(gas^2)+I(pack^2), data=odor), cor=T) # fit model 2


