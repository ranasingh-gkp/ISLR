# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression

fix(Boston)
data(Boston)
names(Boston)
lm.fit=lm(Boston$medv~Boston$lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)     #In order to obtain a confidence interval for the coefficient estimates, we can use the confint() command
confint(lm.fit)  #In order to obtain a confidence interval for the coefficient estimates, we can use the confint() command.
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(lstat,medv)  #scatter plot
abline(lm.fit)  # to draw line
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(1,1))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit)) #residuals against the fitted values.
plot(predict(lm.fit), rstudent(lm.fit))  #studentized residuals against the fitted values.
plot(hatvalues(lm.fit)) #Leverage statistics can be computed for any number of predictors using the hatvalues() function
which.max(hatvalues(lm.fit)) #tells us which observation has the largest leverage statistic.
#in hatvalues() graph index shows observation no

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit) #The vif() function, part of the car package, can be used to compute variance inflation factors.
#vif ia the ratio of the variance of ˆβj when fitting the full model divided by the variance of ˆβj if fit on its own
lm.fit1=lm(medv~.-age,data=Boston)#regression using all predictors except age
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)#Alternatively, the update() function can be used

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)#use the anova() function to further quantify the extent to which the quadratic fit is superior to the linear fit
#The anova() function performs a hypothesis test comparing the two models.
#The null hypothesis is that the two models fit the data equally well, and the alternative hypothesis is that the full model is superior.
#Here the F-statistic is 135 and the associated p-value is virtually zero. This provides very clear evidence that the model containing the predictors lstat and lstat2 is far superior to the model that only contains the predictor lstat.
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))# for ploynomial function 1,2,3,4,5
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)#R generates dummy variables automatically
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)#The contrasts() function returns the coding that R uses for the dummy contrasts() variables

# Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
