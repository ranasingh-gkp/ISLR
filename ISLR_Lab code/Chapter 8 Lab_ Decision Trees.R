# Chapter 8 Lab: Decision Trees

# -------Fitting Classification Trees
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
names(Carseats)
head(Carseats)
head.matrix(Carseats)
data(Carseats)
summary(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
#summary function give the number of terminal nodes, and the (training) error rate.
#A small deviance indicates a tree that provides a good fit to the (training) data.
#Here T0 is 27
#The residual mean deviance reported is simply the deviance divided by n???|T0|, which in this case is 400???27 = 373.
plot(tree.carseats)
text(tree.carseats,pretty=0)
#We use the plot() function to display the tree structure, and the text() function to display the node labels.
#The argument pretty=0 instructs R to include the category names for any qualitative predictors, rather than simply displaying a letter for each category.
tree.carseats
#The most important indicator of Sales appears to be shelving location, since the first branch differentiates Good locations from Bad and Medium locations.
#R displays the split criterion (e.g. Price<92.5), the number of observations in that branch, the deviance, the overall prediction for the branch (Yes or No), and the fraction of observations in that branch that take on values of Yes and No.
#Branches that lead to terminal nodes are indicated using asterisks.

#In order to properly evaluate the performance of a classification tree on these data, we must estimate the test error rather than simply computing the training error.
#We split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data.
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
#The predict() function can be used for this purpose. In the case of a classification tree, the argument type="class" instructs R to return the actual class prediction.
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
#This approach leads to correct predictions for around 71.5% of the locations in the test data set.
(86+57)/200
#---tree pruning
#The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration.
#We use the argument FUN=prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, rather than the default for the cv.tree() function, which is deviance.
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
#The cv.tree() function reports the number of terminal nodes of each tree considered (size) as well as the corresponding error rate and the value of the cost-complexity parameter used (k, which corresponds to ?? in (8.4)).
cv.carseats
#The tree with 9 terminal nodes results in the lowest cross-validation error rate, with 50 cross-validation errors.
#We plot the error rate as a function of both size and k.
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
#We now apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree.
#----apply above prune data in model
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
#How well does this pruned tree perform on the test data set? Once again, we apply the predict() function.
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
#If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy:
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# -------Fitting Regression Trees

library(MASS)
set.seed(1)
attach(Boston)
names(Boston)
head(Boston)
head.matrix(Boston)
data(Boston)
#First, we create a training set, and fit the tree to the training data.
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
#only three of the variables have been used in constructing the tree. 
#In the context of a regression tree, the deviance is simply the sum of squared errors for the tree. We now plot the tree.
plot(tree.boston)
text(tree.boston,pretty=0)
#The tree predicts a median house price of $46, 400 for larger homes in suburbs in which residents have high socioeconomic status (rm>=7.437 and lstat<9.715).
#----pruning
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
#However, if we wish to prune the tree, we could do so as follows, using the prune.tree() function:
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
#In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set.
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
boston.test
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Random Forests
#Therefore, the randomForest() function can be used to perform both random forests and bagging.
#--Bagging
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
#The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree-in other words, that bagging should be done.
bag.boston
#performance on test data
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#The test setMSE associated with the bagged regression tree is 13.16, almost half that obtained using an optimally-pruned single tree.
#We could change the number of trees grown by randomForest() using the ntree argument:
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
set.seed(1)
#---random forest(use smaller value of mtry)
#Growing a random forest proceeds in exactly the same way, except that we use a smaller value of the mtry argument.
#By default, randomForest() u???ses p/3 variables when building a random forest of regression trees, and p variables when building a random forest of classification trees.
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
#The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model.
#The latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees
varImpPlot(rf.boston)
#results indicate that across all of the trees considered in the random forest, the wealth level of the community (lstat) and the house size (rm) are by far the two most important variables.
#In the case of regression trees, the node impurity is measured by the training RSS, and for classification trees by the deviance.


# -----Boosting
#We run gbm() with the option distribution="gaussian" since this is a regression problem; if it were a binary classification problem, we would use distribution="bernoulli".
install.packages("gbm")
library(gbm)
set.seed(1)
#The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree.
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
#In this case, as we might expect, median house prices are increasing with rm and decreasing with lstat.
#We now use the boosted model to predict medv on the test set
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#If we want to, we can perform boosting with a different value of the shrinkage parameter ?? in (8.10). The default value is 0.001, but this is easily modified. Here we take ?? = 0.2.
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
