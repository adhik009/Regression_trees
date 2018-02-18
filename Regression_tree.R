## last modified 02.05.2018
## Regression by using tree based methods
## Data used is College from ISLR package, which has graduation rates and various
## numerical and categorical predictor variables


library(ISLR)

data("College")
str(College)

head(College)
dim(College)



# We need tree package to fit tree based models in R
#install.packages("tree")
library(tree)

tree.college <- tree(Grad.Rate ~., data = College)
summary(tree.college)

## Now lets plot the tree
plot(tree.college)
text(tree.college, pretty = 0)

tree.college


#########  TRAINING THE REGRESSION TREE MODEL ##############
## To evaluate model performance we need a training and test set
## Lets create a training and test set

set.seed(26)
train <- sample(1:nrow(College), 400)
College.test <- College[-train,]

## Now lets train the model using training dataset
tree.college.train <- tree(Grad.Rate ~., data = College, subset = train)
summary(tree.college.train)

### The tree has 17 nodes or branches and it looks overcrowded
### Residual Mean Deviance = 120.6
### Let's see if we need that many nodes or not using cross-validation



############## CROSS-VALIDATION #############
## Cross validation to see if pruning the tree is necessary
cv.college <- cv.tree(tree.college.train)
cv.college

## plotting the elbow plot to see effects of CV
plot(cv.college$size,cv.college$dev,type = 'b')

## size indicates tree size and dev indicates CV error rates
## From the plot we can see that similar cross-validation error rates
## can be obtained by smaller tree with size of 5 only
## So, lets prune the tree

prune.college <- prune.tree(tree.college.train, best = 5)
# best = 5, indicates that we only want 5 best branches

## lets plot the tree to see if it matches our expectation
plot(prune.college)
text(prune.college, pretty = 0)

### this is our final trained regression tree model
### Now, lets use it to predict the test data




######## TESTING THE MODEL #############

## Predicting graduation rates of US colleges using a regression tree
yhat.pruned <- predict(prune.college, newdata = College.test)

## lets plot the predictions and real data
real.data <- College.test$Grad.Rate
plot(yhat.pruned, real.data)
abline(0,1)

## Correlation test of predicted and real data
cor.test(yhat.pruned, real.data)

## correlation is significant at r = 0.54

## calculation of MSE
y.hat_MSE <- mean((real.data - yhat.pruned)^2)
y.hat_MSE
## The MSE is 224.82


## Now, lets check if that would be different, if we used the unpruned tree
## on the test set
yhat.unpruned <- predict(tree.college.train, newdata = College.test)
real.data <- College.test$Grad.Rate
plot(yhat.unpruned, real.data)
abline(0,1)
cor.test(yhat.unpruned, real.data)
## r = 0.574


## calculation of MSE using the unpruned tree
error.1 <- mean((real.data-yhat.unpruned)^2)
show(error.1)
### The MSE is 219.6053

### So, the result doesn't change much wether we prune the tree or not
### To keep the model parsimonious, we will keep the pruned tree


