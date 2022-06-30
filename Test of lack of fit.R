# Assume Variance known and due to the way the weights are defined, i.e., wi=1/var(yi), the known variance is implicitly equal to one
p <- read.table("phy.data", header=T) # Read the data into R
x <- p[,2]; y <- p[,3]; w <- 1/p[,4]^2 # Extract variables and calculate weights
g <- lm(y~x,weights=w) # Fit a simple regression model with w as weights

# Let's perform the test for lack of fit. Compute the test statistic and its p-value:
summary(g)
RSS_w = sum(w * g$residuals^2)
1-pchisq(RSS_w, g$df.residual)
