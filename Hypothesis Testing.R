#Import 'saving' data set
saving.x <- read.table("saving.x", header=T)
#Assign variables
p15 <- saving.x[,1] # save the 1st column as p15
p75 <- saving.x[,2] # save the 2nd column as p75
inc <- saving.x[,3] # save the 3rd column as inc
gro <- saving.x[,4] # save the 4th column as gro
sav <- saving.x[,5] # save the 5th column as sav
#Fit the model, name the model as 'g'
g = lm(sav ~ p15 + p75 + inc + gro)
summary(g)
#calculate the F test statistic from R2 = 0.3385
(0.3385/(1-0.3385)) * (50-5)/(5-1)
#calculate R2 from the F test statistic = 5.756
1/(1 + (1/5.756) * 45/4)
#calculate the RSS omega(big space)
RSS_omega = t(g$residuals) %*% g$residuals
#the degrees of freedom of RSSW, i.e., n-p
df_omega = g$df.residual
#the RSSw, which is TSS in this case
RSSw = t(sav - mean(sav)) %*% (sav - mean(sav))
#the overall F-statistics
overall_f = ((RSSw - RSS_omega)/(5-1)) / (RSS_omega/df_omega)
#the p-value of the overall F-test, the command "pf" returns the cdf value of an F distribution
1 - pf(overall_f, 4, 45)
#A convenient way in R to calculate the F-statistics and its p-value is as follows:
#1)fit the null model
gl = lm(sav ~ 1)
#2)compare the two models W and w, the command "anova" computes the analysis of variance (or deviance) tables for one or more fitted models. Use "help(anova)" to get more information.
anova(gl,g)

#Now, let's do the t-test, H0: bp15=0.
# fit the model without p15
g2 = lm(sav ~ p75 + inc + gro)
#the RSSw
RSS_w = t(g2$residuals) %*% g2$residuals
((RSS_w - RSS_omega)/(g2$df.residual - g$df.residual)) / (RSS_omega/g$df.residual)
sqrt(((RSS_w - RSS_omega)/(g2$df.residual - g$df.residual)) / (RSS_omega/g$df.residual)) # t-statistics is square root of the F-statistics

#Test a subspace H0: beta_p15 = beta_p75
g4 = lm(sav ~ I(p15 + p75) + inc + gro)
anova(g4, g)

#Testing a subset w: H0: beta_gro=1
g5 = lm(sav ~ p15 + p75 + inc + offset(gro))
anova(g5,g)
