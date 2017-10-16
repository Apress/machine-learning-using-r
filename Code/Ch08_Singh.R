## ------------------------------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96)
knitr::opts_chunk$set(
  fig.path = "Images/"
)

## ----caret_install, eval=FALSE-------------------------------------------
## install.packages("caret", dependencies = c("Depends", "Suggests"))

## ----caret_01, eval= FALSE-----------------------------------------------
## rfControl <- trainControl(# Example, 10-fold Cross Validation
## method = "repeatedcv",   # Others are available, such as repeated K-fold cross-validation, leave-one-out etc
## number = 10,             # Number of folds
## repeats = 10             # repeated ten times
## )

## ----caret_02, eval= FALSE-----------------------------------------------
## set.seed(917)
## randomForectFit1 <- train(Class ~ .,               # Define the model equation
## data = training,         # Define the modeling data
## method = "rf",          # List the model you want to use, caret provide list of options in train Model listor
## trControl = rfControl,  # This defines the conditions on how to control the training
## ...  )                    # Other options specific to the modeling technique
## randomForectFit1

## ----ch08_hyperparameter_01, warning=FALSE-------------------------------
setwd("C:/Personal/Machine Learning/Final Artwork and Code/Chapter 8");
library(caret)
library(randomForest)
set.seed(917);
# Load Dataset
Purchase_Data <- read.csv("Dataset/Purchase Prediction Dataset.csv",header=TRUE)

#Remove the missing values
data <- na.omit(Purchase_Data)

#Pick a sample of records
Data <- data[sample(nrow(data),size=10000),]

#Model 1 with tree size = 20
fit_20 <- randomForest(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration,
data=Data, 
importance=TRUE, 
ntree=20)
#Print the result for  ntree=20
print(fit_20)


#Model 1 with tree size = 50
fit_50 <- randomForest(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration,
data=Data, 
importance=TRUE, 
ntree=50)
#Print the result for  ntree=50
print(fit_50)


## ----ch08_hyper_01, warning=FALSE----------------------------------------
# Manually search parametres
library(data.table)
# load the packages
library(randomForest)
library(mlbench)
library(caret)
# Load Dataset

dataset <- Data
metric <- "Accuracy"
# Manual Search
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(dataset)-2)))
modellist <- list()
for (ntree in c(100, 150, 200, 250)) {
  set.seed(917);
  fit <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=trainControl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results by resampling
results <- resamples(modellist)
#Summary of Results
summary(results)
#Dot Plot of results
dotplot(results)

## ----ch08_hyper_02,warning=FALSE-----------------------------------------
# Tune algorithm parameters using a manual grid search.
seed <- 917;
dataset <- Data
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# design the parameter tuning grid
grid <- expand.grid(size=c(5,10,20,50), k=c(1,2,3,4,5))
# train the model
model <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="lvq", trControl=control, tuneGrid=grid)
# summarize the model
print(model)
# plot the effect of parameters on accuracy
plot(model)

## ----ch08_hyper_03,warning=FALSE-----------------------------------------
# Tune algorithm parameters using an automatic grid search.
set.seed(917);
dataset <- Data

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="lvq", trControl=control, tuneLength=5)
# summarize the model
print(model)
# plot the effect of parameters on accuracy
plot(model)

## ----ch08_hyper_04,warning=FALSE-----------------------------------------
# Select the best tuning configuration
dataset <- Data

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(917);
tunegrid <- expand.grid(.cp=seq(0,0.1,by=0.01))
fit.cart <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="rpart", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
fit.cart
# display the best configuration
print(fit.cart$bestTune)

plot(fit.cart)

## ----ch08_hyper_05,warning=FALSE-----------------------------------------

# Randomly search algorithm parameters

# Select the best tuning configuration
dataset <- Data

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# train the model
model <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="rf", trControl=control)
# summarize the model
print(model)
# plot the effect of parameters on accuracy
plot(model)

## ----ch08_hyper_06,warning=FALSE-----------------------------------------
library(caret)
library(randomForest)
library(class)
# Load Dataset
Purchase_Data <- read.csv("Dataset/Purchase Prediction Dataset.csv",header=TRUE)

data <- na.omit(Purchase_Data)

#Create a sample of 10K records
set.seed(917);
Data <- data[sample(nrow(data),size=10000),]
# Select the best tuning configuration
dataset <- Data

# Customer Parameter Search

# load the packages
library(randomForest)
library(mlbench)
library(caret)

# define the custom caret algorithm (wrapper for Random Forest)
customRF <- list(type="Classification", library="randomForest", loop=NULL)
customRF$parameters <- data.frame(parameter=c("mtry", "ntree"), class=rep("numeric", 2), label=c("mtry", "ntree"))
customRF$grid <- function(x, y, len=NULL, search="grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
randomForest(x, y, mtry=param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {  predict(modelFit, newdata)}
customRF$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {   predict(modelFit, newdata, type = "prob")}
customRF$sort <- function(x){ x[order(x[,1]),]}
customRF$levels <- function(x) {x$classes}

# Load Dataset

dataset <- Data

metric <- "Accuracy"

# train model
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:4), .ntree=c(100, 150, 200, 250))
set.seed(917)
custom <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=trainControl)
print(custom)
plot(custom)


## ----ch08_bias_vs_variance,warning=FALSE---------------------------------
mu <- 2
Z <- rnorm(20000, mu)

MSE <- function(estimate, mu) {
return(sum((estimate - mu)^2) / length(estimate))
}

n <- 100
shrink <- seq(0,0.5, length=n)
mse <- numeric(n)
bias <- numeric(n)
variance <- numeric(n)

for (i in 1:n) {
mse[i] <- MSE((1 - shrink[i]) * Z, mu)
bias[i] <- mu * shrink[i]
variance[i] <- (1 - shrink[i])^2
}

# Bias-Variance tradeoff plot

plot(shrink, mse, xlab='Shrinkage', ylab='MSE', type='l', col='pink', lwd=3, lty=1, ylim=c(0,1.2))
lines(shrink, bias^2, col='green', lwd=3, lty=2)
lines(shrink, variance, col='red', lwd=3, lty=2)
legend(0.02,0.6, c('Bias^2', 'Variance', 'MSE'), col=c('green', 'red', 'pink'), lwd=rep(3,3), lty=c(2,2,1))

## ----ch08_bagging, warning= FALSE----------------------------------------
library(caret)
library(randomForest)
library(class)
library(ipred)
# Load Dataset
Purchase_Data <- read.csv("Dataset/Purchase Prediction Dataset.csv",header=TRUE)

data <- na.omit(Purchase_Data)

#Create a sample of 10K records
set.seed(917);
Data <- data[sample(nrow(data),size=10000),]
# Select the best tuning configuration
dataset <- Data
# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# Bagged CART
set.seed(917)
fit.treebag <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(917)
fit.rf <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)


## ----ch08_boosting, warning=FALSE----------------------------------------
# Load Dataset
Purchase_Data <- read.csv("Dataset/Purchase Prediction Dataset.csv",header=TRUE)

data <- na.omit(Purchase_Data)

#Create a sample of 10K records
set.seed(917);
Data <- data[sample(nrow(data),size=10000),]
# Select the best tuning configuration
dataset <- Data
library(caret)
library(C50)
library(gbm)
dataset <- Data;
# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0
set.seed(917)
fit.c50 <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="C5.0", metric=metric, trControl=control)
fit.c50
plot(fit.c50)
# Stochastic Gradient Boosting
set.seed(917)
fit.gbm <- train(factor(ProductChoice) ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
fit.gbm
plot(fit.gbm)

# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

## ----ch08_blending, warning=FALSE----------------------------------------
# Blending (linear combination of models)

# load libraries
library(caret)
library(caretEnsemble)
library(MASS)

#dataset$choice <- ifelse(dataset$ProductChoice == 1,"A",ifelse(dataset$ProductChoice == 2,"B",ifelse(dataset$ProductChoice == 3, "C","D")))
set.seed(917);
Data <- data[sample(nrow(data),size=10000),];

dataset <- Data;

dataset$choice <- ifelse(dataset$ProductChoice == 1 | dataset$ProductChoice == 2 ,"A","B")

dataset$choice <-as.factor(dataset$choice)
# define training control
train_control <- trainControl(method="cv", number=4, savePredictions=TRUE, classProbs=TRUE)
# train a list of models
methodList <- c('knn','rpart')
models <- caretList(choice ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, trControl=train_control, methodList=methodList)
# create ensemble of trained models
ensemble <- caretEnsemble(models)
# summarize ensemble
summary(ensemble)

## ----ch08_stacking_01,warning=FALSE--------------------------------------
# Example of Stacking algorithms
library(kernlab);
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(917)
models <- caretList(choice ~ MembershipPoints + CustomerAge +  PurchaseTenure + CustomerPropensity + LastPurchaseDuration, data=dataset, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)


## ----cho08_stacking_02,warning=FALSE-------------------------------------
# correlation between results
modelCor(results)
splom(results)

## ----ch08_stacking_03,warning=FALSE--------------------------------------
# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(917)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)


## ----ch08_stacking_04,warning=FALSE--------------------------------------

# stack using random forest
set.seed(917)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)

## ----ch08_bayesian_optimization_01,warning=FALSE-------------------------

library(caret)
library(randomForest)
library(class)
library(ipred)
library(GPfit)
# Load Dataset
House_price <- read.csv("Dataset/House Sale Price Dataset.csv",header=TRUE)

dataset <- na.omit(House_price)

#Create a sample of 10K records
set.seed(917);


rand_ctrl <- trainControl(method = "repeatedcv", repeats = 5, search = "random")

rand_search <- train(HousePrice ~ StoreArea + BasementArea + SellingYear + SaleType + ConstructionYear + Rating, data = dataset, method = "svmRadial",
                                           ## Create 20 random parameter values
                                             tuneLength = 20,
                                         metric = "RMSE",
                                            preProc = c("center", "scale"),
                                           trControl = rand_ctrl)
rand_search

ggplot(rand_search) + scale_x_log10() + scale_y_log10()

getTrainPerf(rand_search)

## ----ch08_bayesian_optimization_02,warning=FALSE-------------------------
# Define the resampling method
ctrl <- trainControl(method = "repeatedcv", repeats = 5)

## Use this function to optimize the model. The two parameters are 
## evaluated on the log scale given their range and scope. 
svm_fit_bayes <- function(logC, logSigma) {
 ## Use the same model code but for a single (C, sigma) pair. 
   txt <- capture.output(
     mod <- train(HousePrice ~ StoreArea + BasementArea + SellingYear + SaleType + ConstructionYear + Rating , data = dataset,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  metric = "RMSE",
                  trControl = ctrl,
                  tuneGrid = data.frame(C = exp(logC), sigma = exp(logSigma)))
   )
   ## The function wants to _maximize_ the outcome so we return 
   ## the negative of the resampled RMSE value. `Pred` can be used
   ## to return predicted values but we'll avoid that and use zero
   list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
 }

## Define the bounds of the search. 
 lower_bounds <- c(logC = -5, logSigma = -9)
 upper_bounds <- c(logC = 20, logSigma = -0.75)
 bounds <- list(logC = c(lower_bounds[1], upper_bounds[1]),
                logSigma = c(lower_bounds[2], upper_bounds[2]))

 ## Create a grid of values as the input into the BO code
 initial_grid <- rand_search$results[, c("C", "sigma", "RMSE")]
 initial_grid$C <- log(initial_grid$C)
 initial_grid$sigma <- log(initial_grid$sigma)
 initial_grid$RMSE <- -initial_grid$RMSE
 names(initial_grid) <- c("logC", "logSigma", "Value")
 
 ## Run the optimization with the initial grid and with 30
 
 
 library(rBayesianOptimization)
 
 set.seed(917)
 ba_search <- BayesianOptimization(svm_fit_bayes,
                                                                      bounds = bounds,
                                                                       init_grid_dt = initial_grid, 
                                                                       init_points = 0, 
                                                                       n_iter = 30,
                                                                       acq = "ucb", 
                                                                       kappa = 1, 
                                                                       eps = 0.0,
                                                                       verbose = TRUE)
ba_search

## ----ch08_bayesian_optimization_03,warning=FALSE-------------------------
final_search <- train(HousePrice ~ StoreArea + BasementArea + SellingYear + SaleType + ConstructionYear + Rating, data = dataset,
                                              method = "svmRadial",
                                              tuneGrid = data.frame(C = exp(ba_search$Best_Par["logC"]), 
                                              sigma = exp(ba_search$Best_Par["logSigma"])),
                                              metric = "RMSE",
                                              preProc = c("center", "scale"),
                                             trControl = ctrl)

final_search
compare_models(final_search, rand_search)

