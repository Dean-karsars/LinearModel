# Dichotomous Predictor and Analysis of Covariance 
cathedral <- read.table("cathedral.data")
lapply(split(cathedral, cathedral$style),summary)
plot(cathedral$x,cathedral$y,type="n",xlab="Nave height",ylab="Length")
text(cathedral$x,cathedral$y,as.character(cathedral$s))
g = lm(y ~ x * style, data = cathedral) # "*"不代表乘法，(a + b + c) * (a + b + c) <=> a + b + c + a:b + a:c + b:c, ":"代表模型中的交互项
summary(g)
model.matrix(g) #Check the coding method
options(contrasts=c("contr.sum","contr.poly")) # Assign contrast 
# Let's fit the full model again under (-1, 1) coding:
g <- lm(y ~ x+style+x:style, data=cathedral)
summary(g)
model.matrix(g) #Check the coding method
options(contrasts = c('contr.treatment', 'contr.poly'))
g1 <- lm(y ~ x+style, data=cathedral)
summary(g1)
abline(44.298,4.7116)
abline(44.298+80.393,4.7116,lty=2)
par(mfrow=c(2,2))
plot(g)

# Polytomous predictor
twins <- read.table("twin.data",header=T)
plot(twins$B,twins$F,type="n",xlab="Biological IQ",ylab="Foster IQ")
text(twins$B,twins$F,substring(as.character(twins$S),1,1))
g <- lm(Foster ~ Biological*Social, twins)
summary(g)

# Now see if the model can be simplified to the parallel lines model:
gp<- lm(Foster ~ Biological+Social, twins)
anova(gp,g)

# The sequential testing can be done in one go:
anova(g)

#In the analysis, we adopt the treatment coding (it's easier for interpreting the parameters):

contr.treatment(3)


#There are various codings you can choose if you fell they are more reasonable for your data, such as the Helmert coding:
contr.helmert(3)


# and the sum coding:

contr.sum(3)

# Once you decide your coding, you can use it by changing the "contrasts" setting in "options".

