## ------------------------------------------------------------------------
library(data.table)
library(splitstackshape)
library(e1071)

Data_Purchase <- fread("C:\\Users\\Karthik\\Dropbox\\Book Writing - Drafts\\Chapter Drafts\\Chap 6 29 Sep\\Dataset\\Purchase Prediction Dataset.csv",header=T, verbose = FALSE, showProgress = FALSE)
str(Data_Purchase)

#Check the distribution of data before grouping
table(Data_Purchase$ProductChoice)


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

set.seed(917);
train <- Data_Purchase_Model[sample(nrow(Data_Purchase_Model),size=nrow(Data_Purchase_Model)*(0.7), replace = TRUE, prob = NULL),]
train <- as.data.frame(train)

test <- as.data.frame(Data_Purchase_Model[!(Data_Purchase_Model$CUSTOMER_ID %in% train$CUSTOMER_ID),])

## ------------------------------------------------------------------------
model_naiveBayes <- naiveBayes(train[,c(3,4,5)], train[,2])
model_naiveBayes


## ------------------------------------------------------------------------
model_naiveBayes_pred <- predict(model_naiveBayes, train)

library(gmodels)

CrossTable(model_naiveBayes_pred, train[,2],
prop.chisq = FALSE, prop.t = FALSE,
dnn = c('predicted', 'actual'))

## ------------------------------------------------------------------------

model_naiveBayes_pred <- predict(model_naiveBayes, test)

library(gmodels)

CrossTable(model_naiveBayes_pred, test[,2],
prop.chisq = FALSE, prop.t = FALSE,
dnn = c('predicted', 'actual'))


