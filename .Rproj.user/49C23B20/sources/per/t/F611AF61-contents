x <- 10*runif(50) # x~U(0,10), 50 uniform random numbers
y <- x+rnorm(50) # b0=0, b1=1, e~U(0,1)
m1 <- lm(y~x) # fit a simple linear regression model
summary(m1) # should see estimates close to true values
z <- x+rnorm(50) # now add some standard normal error to x
m2 <- lm(y~z)
summary(m2) # what happened to the estimates and the other quantities?
z1 <- x+5*rnorm(50) # add even more error
m3 <- lm(y~z1) 
summary(m3) # relationship between x and y is almost washed out
# We can plot all this information:
matplot(cbind(x,z,z1),y,xlab="x",ylab="y")
abline(m1,lty=1)
abline(m2,lty=2) 
abline(m3,lty=3)
# Note the slopes decrease when the errors in x have larger variance.