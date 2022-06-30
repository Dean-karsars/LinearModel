#Read the data
saving.x= read.table('saving.x', header = TRUE)
#Assign columns
p15 <- saving.x[,1]
p75 <- saving.x[,2]
inc <- saving.x[,3]
gro <- saving.x[,4]
sav <- saving.x[,5]
#Fit the model with saving as response and the rest as predictors
g = lm(sav ~ p15 + p75 + inc + gro)
summary(g)
#Calculate confidence interval for p75
#1)Calculate critical value
crtical_value = qt(0.975, g$df.residual)
#2)95% confidence interval for p75
c(-1.6915757 -1.0835862 * crtical_value, -1.6915757 + 1.0835862 * crtical_value)
#The following is a convenient way to obtain all the uni-variate intervals:
confint(g)
#confidence region for bi and bj
library(ellipse)
#the 95% confidence region for p15 and p75. "c(2,3)" appoints the 2nd and the 3rd estimates, i.e., p15 and p75, in the object g. "type" and "xlim" are graphic options. 
plot(ellipse(g, c(2,3)), lwd = 3 ,type = 'l', xlim = c(-1, 0))
#add the point (0,0) to the plot. the "points" command can add points on the plot. "cex" and "pch" are graphic options, use "help(par)" to get more information.
points(g$coef[2], g$coef[3], cex=2,pch=18) 
#95% confidence interval of p15. The "abline" command can add straight lines on the plot.
abline(v=c(-0.4612050-2.014103*0.1446425, -0.4612050+2.014103*0.1446425),lwd=3,lty=2) 
#95% confidence interval of p75.
abline(h=c(-1.6915757-2.014103*1.0835862, -1.6915757+2.014103*1.0835862),lwd=3,lty=2)
#let's try to construct a 95% confidence interval for the prediction of mean response at p15=4, p75=3, inc=1100, and gro=3.
x0 <- c(1,4,3,1100,3) 
yo = t(x0) %*% g$coefficients
#calculate the standard error of the estimate
bm = sqrt(t(x0) %*% solve(t(model.matrix(g)) %*% model.matrix(g)) %*% x0) * summary(g)$sigma
#the 95% confidence interval for the prediction of mean response. confidence interval
c(yo - qt(0.975, df = g$df.residual) * bm, yo + qt(0.975, df = g$df.residual) * bm)
#Use predict command to redo previous calculations
predict(object = g, data.frame(p15 = 4, p75 = 3, inc = 1100, gro = 3), se = TRUE, interval = 'confidence')
#generate pointwise confidence band for predictors p75 when other predictors are fixed at their mean values
grid = seq(0, 10, 0.1)
p = predict(g, data.frame(p15 = mean(p15), p75 = grid, inc = mean(inc), gro = mean(gro)), se = TRUE, interval = 'confidence')
matplot(grid, p$fit, lty=c(1,2,2), lwd=3, type="l", xlab="p75", ylab="saving")
rug(p75)
#confidence interval for the prediction of future observation
bmf = sqrt(t(x0) %*% solve(t(model.matrix(g)) %*% model.matrix(g)) %*% x0 + 1) * summary(g)$sigma
c(yo - qt(0.975, df = g$df.residual) * bmf, yo + qt(0.975, df = g$df.residual) * bmf)
#Use predict command to redo previous calculations
predict(g, data.frame(p15 = 4, p75 = 3, inc = 1100, gro = 3), se = TRUE, interval = 'prediction')
























