savings <- read.table("savings.data") # read the data into R
g <- lm(sav ~ p15 + p75 + inc + gro, data=savings) # fit the model with sav as the response and the rest variables as predictors
plot(g$res, ylab="Residual", main="index plot of residuals") # draw the plot of residuals against index
# Let's find which countries correspond to the largest and smallest residuals.
sort(g$res)[c(1,50)] # sort the residuals from small to large and get the largest and smallest residuals; the command "sort" returns a vector in ascending or descending order
countries <- row.names(savings) # extract the country names
identify(1:50,g$res,countries) # label the points with their country names
# We first extract the X-matrix here using "model.matrix()" and then compute and plot the leverages (also called "hat" values)
x = model.matrix(g)
lev = hat(x)
plot(lev, ylab="Leverages",main="index plot of leverages") # draw the plot of leverages against index
abline(h=2*5/50) # add a 2p/n horizontal line
names(lev) = countries
lev[lev> 2 * sum(lev)/50]


# Studentized residuals
gsum <- summary(g) # get the summary output of the fitted model g
gsum$sig # The σ-hat
# Compute studentized residuals
stud = g$residuals / (gsum$sigma * sqrt(1 - lev))
plot(stud,ylab="studentized Residuals",main="studentized residuals")
# Easy way to compoute studentized residuals
rstandard(g)

# Outlier and jacknife residuals
jack = rstudent(g)
jack[abs(jack)==max(abs(jack))] # find out the country with largest jacknife residual
qt(0.05/2, 44) # compare with tn-p-1(a/2)

# But is it an outlier under the Bonferroni critical value that takes into consideration the condition of multiple testing?
qt(.05/(50*2), 44) # compare with tn-p-1(a/2n)

# Here is an example of a dataset with multiple outliers.
star <- read.table("star.data",h=T) # read the data into R
plot(star$temp,star$light) # draw the scatter plot of log light intensity against temperature
gs = lm(light ~ temp)
abline(gs) # add the fitted line to the scatter plot
plot(rstudent(gs)) # plot of jacknife residuals
sort(rstudent(gs))[c(1,2,3,45,46,47)] # get the three largest and three smallest jacknife residuals
qt(0.05/2,44); qt(0.05/(2*47),44) # critical value under significance level a/2 and a/2n
star$temp[c(17,14,19,2,4,34)] # get the temp values of the six observations with most extreme jacknife residuals
gs1 <- lm(light ~ temp, data=star, subset=(temp>3.6)) # fit a model without using the giant data
plot(star$temp,star$light); abline(gs); abline(gs1, lty=2)  # add the fitted line of the last fitted model

# Influential observation
cook = cooks.distance(g)
plot(cook, ylab="Cooks distance") # draw the plot of Cook's statistics against index
identify(1:50,cook,countries) # identify the three countries with largest Cook's statistics\

# Calculate some values useful for investigating influence
ginf = lm.influence(g) 












