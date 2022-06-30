#read the gala dataset and save it as gala
gala = read.table('gala.data')
# Now fit the model - Speciesi	=	β0 + β1*Areai + β2*Elevationi + β3*Nearesti + β4*Scruzi + β5*Adjacenti + εi,
gfit = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
# Summary the fitted model
summary(gfit)
#Let the correlation matrix of beta hat shown in the summary output and set the digits of decimals to 3.
options(digits = 3)
summary(gfit, correlation = TRUE)$cor
#Return the (XTX)-1
summary(gfit)$cov
#Return the covariance matrix of beta hat
summary(gfit)$cov *  (summary(gfit)$sigma)^2
#Return the  Model(Design) Matrix of gfit
model.matrix(gfit)
#See correlations between columns of the design matrix
cor(model.matrix(gfit))
#For Practice Purpose, subtract values from gfit and gala to define variables of y, x, (XTX)-1
y = gala$Species
x = model.matrix(gfit)
xtxi = solve(t(x) %*% x)
xtxi - summary(gfit)$cov
#Compute beta hat(projection matrix method-A bad way)
beta_hat = xtxi %*% t(x) %*% y
#Compute beta hat(Norm equation method-A better way)
beta_hat2 = solve(t(x) %*% x, t(x) %*% y) 
#Compute y hat
y_hat = x %*% beta_hat
#Compute Residuals
res = y - y_hat
#show residuals and y hat are orthogonal
t(res) %*% y_hat
#Compute R-square-3 ways
R_Square1 = 1 - (t(res) %*% res) / (t(y-mean(y)) %*% (y-mean(y)))
R_Square2 = (t(y_hat - mean(y)) %*% (y_hat - mean(y))) / (t(y - mean(y)) %*% (y - mean(y)))
R_Square3 = cor(y, y_hat)^2

#Compute the correlation matrix of beta hat
z = sqrt(diag(xtxi))
xtxi / z %o% z

#Compute the predicted value
xp <- c(1, 0.08, 93, 6.0, 12.0, 0.34)
t(xp) %*% beta_hat





