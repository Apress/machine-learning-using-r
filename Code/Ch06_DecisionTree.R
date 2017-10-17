## ----opts, echo = FALSE--------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96)
knitr::opts_chunk$set(
  fig.path = "~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Images/003/"
)

## ---- fig.cap="Image 6.x: Gini-Index Function"---------------------------
curve(x * (1- x) + (1 - x) * x, xlab = "P", ylab = "Gini-Index", lwd = 5)

## ---- fig.cap="Image 6.x: Entropy Function"------------------------------
curve(-x * log2(x) - (1 - x) * log2(1 - x), xlab = "x", ylab = "Entropy", lwd = 5)

## ---- message=FALSE, warning=FALSE---------------------------------------

library(C50)
library(splitstackshape)
library(rattle)
library(rpart.plot)
library(data.table)

Data_Purchase <- fread("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/Purchase Prediction Dataset.csv",header=T, verbose = FALSE, showProgress = FALSE)
str(Data_Purchase)

#Check the distribution of data before grouping
table(Data_Purchase$ProductChoice)

## ------------------------------------------------------------------------

#Pulling out only the relevant data to this chapter
Data_Purchase <- Data_Purchase[,.(CUSTOMER_ID,ProductChoice,MembershipPoints,IncomeClass,CustomerPropensity,LastPurchaseDuration)]

#Delete NA from subset
Data_Purchase <- na.omit(Data_Purchase)
Data_Purchase$CUSTOMER_ID <- as.character(Data_Purchase$CUSTOMER_ID)

#Stratified Sampling
Data_Purchase_Model<-stratified(Data_Purchase, group=c("ProductChoice"),size=10000,replace=FALSE)

print("The Distribution of equal classes is as below")
table(Data_Purchase_Model$ProductChoice)

Data_Purchase_Model$ProductChoice <- as.factor(Data_Purchase_Model$ProductChoice)
Data_Purchase_Model$IncomeClass <- as.factor(Data_Purchase_Model$IncomeClass)
Data_Purchase_Model$CustomerPropensity <- as.factor(Data_Purchase_Model$CustomerPropensity)

#Build the decision tree on Train Data (Set_1) and then test data (Set_2) will be used for performance testing

set.seed(917);
train <- Data_Purchase_Model[sample(nrow(Data_Purchase_Model),size=nrow(Data_Purchase_Model)*(0.7), replace = TRUE, prob = NULL),]
train <- as.data.frame(train)

test <- Data_Purchase_Model[!(Data_Purchase_Model$CUSTOMER_ID %in% train$CUSTOMER_ID),]


## ---- message=FALSE,warning=FALSE,eval=FALSE-----------------------------
## 
## library(RWeka)
## 
## WPM("refresh-cache")
## WPM("install-package", "simpleEducationalLearningSchemes")
## 
## 
## ## make classifier
## ID3 <- make_Weka_classifier("weka/classifiers/trees/Id3")
## 
## ID3Model <- ID3(ProductChoice ~ CustomerPropensity + IncomeClass , data = train)
## 
## summary(ID3Model)
## 

## ----eval=FALSE----------------------------------------------------------
## 
## library(gmodels)
## purchase_pred_test <- predict(ID3Model, test)
## CrossTable(test$ProductChoice, purchase_pred_test,
## prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
## dnn = c('actual default', 'predicted default'))
## 

## ------------------------------------------------------------------------

model_c50 <- C5.0(train[,c("CustomerPropensity","LastPurchaseDuration", "MembershipPoints")],
             train[,"ProductChoice"],
            control = C5.0Control(CF = 0.001, minCases = 2))


## ---- warning=FALSE,message=FALSE----------------------------------------

summary(model_c50)
#plot(model_c50)


## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(model_c50, train,type = "class")
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------
purchase_pred_test <- predict(model_c50, test)
CrossTable(test$ProductChoice, purchase_pred_test,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------

CARTModel <- rpart(ProductChoice ~ IncomeClass + CustomerPropensity + LastPurchaseDuration + MembershipPoints, data=train)

summary(CARTModel)

library(rpart.plot)
library(rattle)

#fancyRpartPlot(CARTModel)


## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(CARTModel, train,type = "class")
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------

#CHAID installation from source
##install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

ctrl <- chaid_control(minsplit = 200, minprob = 0.1)
CHAIDModel <- chaid(ProductChoice ~ CustomerPropensity + IncomeClass, data = train, control = ctrl)
print(CHAIDModel)
#plot(CHAIDModel)


## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(CHAIDModel, train)
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

## ------------------------------------------------------------------------
purchase_pred_test <- predict(CHAIDModel, test)
CrossTable(test$ProductChoice, purchase_pred_test,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)

ModelC50_boostcv10 <- C5.0(train[,c("CustomerPropensity","LastPurchaseDuration", "MembershipPoints")], train[,"ProductChoice"], trials = 10)

## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(ModelC50_boostcv10, train)
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

## ------------------------------------------------------------------------
purchase_pred_test <- predict(ModelC50_boostcv10, test)
CrossTable(test$ProductChoice, purchase_pred_test,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------

control <- trainControl(method="repeatedcv", number=5, repeats=2)

# Bagged CART
set.seed(100)
CARTBagModel <- train(ProductChoice ~ CustomerPropensity + LastPurchaseDuration + MembershipPoints, data=train, method="treebag", trControl=control)

## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(CARTBagModel, train)
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

## ------------------------------------------------------------------------
purchase_pred_test <- predict(CARTBagModel, test)
CrossTable(test$ProductChoice, purchase_pred_test,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------
# Random Forest
set.seed(100)

rfModel <- train(ProductChoice ~ CustomerPropensity + LastPurchaseDuration + MembershipPoints, data=train, method="rf", trControl=control)


## ------------------------------------------------------------------------

library(gmodels)

purchase_pred_train <- predict(rfModel, train)
CrossTable(train$ProductChoice, purchase_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

## ------------------------------------------------------------------------
purchase_pred_test <- predict(rfModel, test)
CrossTable(test$ProductChoice, purchase_pred_test,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


