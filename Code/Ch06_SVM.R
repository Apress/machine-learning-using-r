## ------------------------------------------------------------------------

library(e1071)
library(rpart)

breast_cancer_data <- read.table("~/Dropbox/Book Writing - Drafts/Chapter Drafts/Final Artwork and Code/Chapter 6/Dataset/breast-cancer-wisconsin.data.txt",sep=",")
breast_cancer_data$V11 = as.factor(breast_cancer_data$V11)

summary(breast_cancer_data)


## ------------------------------------------------------------------------

## split data into a train and test set
index <- 1:nrow(breast_cancer_data)
test_data_index <- sample(index, trunc(length(index)/3))
test_data <- breast_cancer_data[test_data_index,]
train_data <- breast_cancer_data[-test_data_index,]


## ------------------------------------------------------------------------

svm.model <- svm(V11 ~ ., data = train_data, cost = 100, gamma = 1)


## ------------------------------------------------------------------------

library(gmodels)

svm_pred_train <- predict(svm.model, train_data[,-11])
CrossTable(train_data$V11, svm_pred_train,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ------------------------------------------------------------------------

svm_pred_test <- predict(svm.model, test_data[,-11])
CrossTable(test_data$V11, svm_pred_test,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


## ----ch06_multiclassSVM--------------------------------------------------
# Read the house Worth Data
Data_House_Worth <- read.csv("~/Dropbox/Book Writing - Drafts/Chapter Drafts/Final Artwork and Code/Chapter 6/Dataset/House Worth Data.csv",header=TRUE);

library( 'e1071' )
#Fit a multiclass SVM
svm_multi_model <- svm( HouseNetWorth ~ StoreArea + LawnArea, Data_House_Worth )

#Display the model
svm_multi_model

#get the predicted vaule for all the set
res <- predict( svm_multi_model, newdata=Data_House_Worth )

#Classification Matrix
table(Data_House_Worth$HouseNetWorth,res)

#Classification Rate

sum(diag(table(Data_House_Worth$HouseNetWorth,res)))/nrow(Data_House_Worth)

