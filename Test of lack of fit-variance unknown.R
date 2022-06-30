# Variance unknown
corrosion <- read.table("corrosion.data") # Read the data into R
g <- lm(loss ~ Fe, data=corrosion) # Fit a simple linear model with loss as response and Fe as predictor
ga <- lm(loss ~ factor(Fe), data=corrosion) # Fit the saturated model, the "factor" command treats Fe as a qualitative predictor

# Calculate lack of fit by hand
SSW = sum(g$residuals^2)
SSpe = sum(ga$residuals^2)
F = ((SSW - SSpe) / (g$df.residual - ga$df.residual)) / (SSpe/ga$df.residual)
1 - pf(F, (g$df.residual - ga$df.residual), ga$df.residual)
# Easier way of doing the lack of fit test
anova(g, ga)
# Do you think the 5th order polynomial model is better than the straight line model?
#   
# We actually cannot explain why the corrosion loss should increase then decrease in the region 0<Fe<0.5, and decrease then increase in the region 1.5<Fe<2.0.
# 
# This is probably a consequence of over-fitting the data.
# 
# Because of the complexity of the model, the fitting on the observed Fe's is good, but the prediction on the region without observed Fe's becomes unstable.
# 
# In this case, we may want to stay with the straight line model and consider other reasons that may causes the lack of fit.
# 
# For example, you may want to find out whether the replicates are genuine.
# 
# Perhaps, the low pure error estimate can be explained by some correlation in the measurements.
# 
# The source of variation should also be examined.
# 
# For example, if the specimens with same Fe's are taken from different positions of a same unit, the pure error estimate does not include the variation between different units.
# 
# In the case, it is reasonable to get a pure error estimate lower than expected.
# 
# Other possible explanation is unmeasured predictors is causing the lack of fit. 