## ------------------------------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96)
knitr::opts_chunk$set(
  fig.path = "Images/"
)

## ----chap7_Data_load, warnings=FALSE, message=FALSE----------------------
setwd("C:/Personal/Machine Learning/Final Artwork and Code/Chapter 7");

library(data.table)

Data_House_Price <- fread("Dataset/House Sale Price Dataset.csv",header=T, verbose = FALSE, showProgress = FALSE)

str(Data_House_Price)


## ----chap7_Data_load_01, warning=FALSE,message=FALSE---------------------

dim(Data_House_Price)

#Check the distribution of dependent variable
hist(Data_House_Price$HousePrice/1000000, breaks=20, col="blue", xlab="House Sale Price(Million)", 
    main="Distribution of House Sale Price")

#Also look at the summary of the Dependent Variable
summary(Data_House_Price$HousePrice)

#Pulling out relevant columns and assigning required fields in the dataset
Data_House_Price <- Data_House_Price[,.(HOUSE_ID,HousePrice,StoreArea,StreetHouseFront,BasementArea,LawnArea,Rating,SaleType)]

#Omit Anu missing value
Data_House_Price <- na.omit(Data_House_Price)

Data_House_Price$HOUSE_ID <- as.character(Data_House_Price$HOUSE_ID)


## ----chap7_Data_load_03, warning=FALSE,message=FALSE---------------------

Data_Purchase <- fread("Dataset/Purchase Prediction Dataset.csv",header=T, verbose = FALSE, showProgress = FALSE)
str(Data_Purchase)

## ----chap7_Data_load_02, warning=FALSE,message=FALSE---------------------

dim(Data_Purchase);

#Check the distribution of data before grouping
table(Data_Purchase$ProductChoice)
barplot(table(Data_Purchase$ProductChoice),main="Distribution of ProductChoice", xlab= "ProductChoice Options", col="Blue")


#Pulling out only the relevant data to this chapter

Data_Purchase <- Data_Purchase[,.(CUSTOMER_ID,ProductChoice,MembershipPoints,IncomeClass,CustomerPropensity,LastPurchaseDuration)]

#Delete NA from subset

Data_Purchase <- na.omit(Data_Purchase)

Data_Purchase$CUSTOMER_ID <- as.character(Data_Purchase$CUSTOMER_ID)


## ----ch07_population_stability_01, warning=FALSE,message=FALSE-----------
#Create set 1 and set 2 : First 2/3 as set 1 and remaining 1/3 as set 2
summary(Data_House_Price$HousePrice)

set_1 <- Data_House_Price[1:floor(nrow(Data_House_Price)*(2/3)),]$HousePrice
summary(set_1)

set_2 <- Data_House_Price[floor(nrow(Data_House_Price)*(2/3) + 1):nrow(Data_House_Price),]$HousePrice
summary(set_2)

## ----ch07_population_stability_02, warning=FALSE,message=FALSE-----------
#Defining a function to give ks test result and ECDF plots on log scale
library(rgr)
ks_test <- function (xx1, xx2, xlab = "House Price", x1lab = deparse(substitute(xx1)),x2lab = deparse(substitute(xx2)), ylab = "Empirical Cumulative Distribution Function",log = TRUE, main = "Empirical EDF Plots - K-S Test", pch1 = 3, col1 = 2, pch2 = 4, col2 = 4, cex = 0.8, cexp = 0.9, ...) 
{
  temp.x <- remove.na(xx1)
  x1 <- sort(temp.x$x[1:temp.x$n])
  nx1 <- temp.x$n
  y1 <- ((1:nx1) - 0.5)/nx1
  temp.x <- remove.na(xx2)
  x2 <- sort(temp.x$x[1:temp.x$n])
  nx2 <- temp.x$n
  y2 <- ((1:nx2) - 0.5)/nx2
  xlim <- range(c(x1, x2))
  if (log) {
    logx <- "x"
    if (xlim[1] <= 0) 
      stop("\n  Values cannot be .le. zero for a log plot\n")
  }
  else logx <- ""
  plot(x1, y1, log = logx, xlim = xlim, xlab = xlab, ylab = ylab, 
       main = main, type = "n", ...)
  points(x1, y1, pch = pch1, col = col1, cex = cexp)
  points(x2, y2, pch = pch2, col = col2, cex = cexp)
  temp <- ks.test(x1, x2)
  print(temp)
}

## ----ch07_population_stability_03, warning=FALSE,message=FALSE-----------
#Perform K-S test on set_1 and set_2 and also display Empirical Cummulative Distribution Plots
ks_test(set_1,set_2)

## ----ch07_population_stability_04, warning=FALSE,message=FALSE-----------
#Maniplate the set 2
set_2_new <- set_2*exp(set_2/100000)

# Now do the k-s test again
ks_test(set_1,set_2_new)

## ----ch07_population_stability_05, warning=FALSE,message=FALSE-----------
#Let's create set 1 and set 2 from our Purchase Prediction Data
print("Distribution of ProductChoice values before partition")
table(Data_Purchase$ProductChoice)

set_1 <- Data_Purchase[1:floor(nrow(Data_Purchase)*(2/3)),]$ProductChoice
table(set_1)

set_2 <- Data_Purchase[floor(nrow(Data_Purchase)*(2/3) + 1):nrow(Data_Purchase),]$ProductChoice
table(set_2)

## ----ch07_population_stability_06, warning=FALSE,message=FALSE-----------
#PSI='??((n1i/N1)'??(n2i/N2))'??ln((n1i/N1)/(n2i/N2))

temp1 <- (table(set_1)/length(set_1) - table(set_2)/length(set_2))

temp2 <- log((table(set_1)/length(set_1))*(table(set_2)/length(set_2)))

psi <- abs(sum(temp1*temp2))

if(psi < 0.1 ){
  cat("The population is stable with a PSI of   " ,psi)
} else if (psi >=0.1 & psi <= 0.2) {
  cat("The population need further investigation with a PSI of   " ,psi)
} else {
  cat("The population has gone thorugh significant changes with a PSi of  " ,psi)
}


## ----ch07_Linear_reg_model_01, warning=FALSE,message=FALSE---------------
# Create a model on Set 1 = Train data


linear_reg_model <- lm(HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea +  Rating	+	SaleType ,data=Data_House_Price[1:floor(nrow(Data_House_Price)*(2/3)),])

summary(linear_reg_model)

## ----ch07_mae_01, warning=FALSE,message=FALSE----------------------------
#Create the test data which is set 2
test <- Data_House_Price[floor(nrow(Data_House_Price)*(2/3) + 1):nrow(Data_House_Price),]

#Fit the linear regression model on this and get predicted values

predicted_lm <- predict(linear_reg_model,test, type= "response")

actual_predicted <- as.data.frame(cbind(as.numeric(test$HOUSE_ID),as.numeric(test$HousePrice),as.numeric(predicted_lm)))

names(actual_predicted) <- c("HOUSE_ID","Actual","Predicted")

#Find the absolute residual and then take mean of that
library(ggplot2)

#Plot Actual vs Predicted values for Test Cases
ggplot(actual_predicted,aes(x = actual_predicted$HOUSE_ID,color=Series)) + 
  geom_line(data = actual_predicted, aes(x = actual_predicted$HOUSE_ID, y = Actual, color = "Actual")) +
  geom_line(data = actual_predicted, aes(x = actual_predicted$HOUSE_ID, y = Predicted, color = "Predicted"))  +  xlab('HOUSE_ID') + ylab('House Sale Price')


## ----ch07_mae_02, warning=FALSE,message=FALSE----------------------------
#Remove NA from test, as we have not done any treatment for NA
actual_predicted <- na.omit(actual_predicted)

#First take Actual - Predicted, then take mean of absolute errors(residual)

mae <- sum(abs(actual_predicted$Actual - actual_predicted$Predicted))/nrow(actual_predicted)

cat("Mean Absolute Error for the test case is  ", mae)

## ----ch07_rmse_01, warning=FALSE,message=FALSE---------------------------
#As we have already have actual and predicted value we can directly calculate the RSME value

rmse <- sqrt(sum((actual_predicted$Actual-actual_predicted$Predicted)^2)/nrow(actual_predicted))

cat("Root Mean Square Error for the test case is  ", rmse)

## ----ch07_r_sqr_01, warning=FALSE,message=FALSE--------------------------
#Model training data ( we will show our analysis on this dataset)

train <- Data_House_Price[1:floor(nrow(Data_House_Price)*(2/3)),.(HousePrice,StoreArea,StreetHouseFront,BasementArea,LawnArea,StreetHouseFront,LawnArea,Rating,SaleType)];

#Omiting the NA from dataset

train <- na.omit(train)

# Get a linear regression model
linear_reg_model <- lm(HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea + StreetHouseFront +	LawnArea +  Rating	+	SaleType ,data=train)

# Show the function call to identify what model we will be working on

print(linear_reg_model$call)

#System generated Square value
cat("The system generated R square value is " , summary(linear_reg_model)$r.squared)

## ----ch07_r_sqr_02, warning=FALSE,message=FALSE--------------------------
#calculate Total Sum of Squares

SST <- sum((train$HousePrice - mean(train$HousePrice))^2);

#Calculate Regression Sum of Squares

SSR <- sum((linear_reg_model$fitted.values - mean(train$HousePrice))^2);

#Calulate residual(Error) Sum of Squares

SSE <- sum((train$HousePrice - linear_reg_model$fitted.values)^2);


## ----ch07_r_sqr_03, warning=FALSE,message=FALSE--------------------------
#calulate R-squared

R_Sqr <- 1- (SSE/SST)

#Dipslay the calulated R-Sqr

cat("The calculated R Square is  ", R_Sqr)

## ----ch07_classi_01, warning=FALSE,message=FALSE-------------------------
#Remove the data having NA. NA is ignored in modeling algorithms
Data_Purchase<-na.omit(Data_Purchase)

#Sample equal sizes from Data_Purchase to reduce class imbalance issue
library(splitstackshape)
Data_Purchase_Model<-stratified(Data_Purchase, group=c("ProductChoice"),size=10000,replace=FALSE)

print("The Distribution of equal classes is as below")
table(Data_Purchase_Model$ProductChoice)

#Build the multinomial model on Train Data (Set_1) and then test data (Set_2) will be used for performance testing
set.seed(917);
train <- Data_Purchase_Model[sample(nrow(Data_Purchase_Model),size=nrow(Data_Purchase_Model)*(0.7), replace = TRUE, prob = NULL),]
dim(train)

test <- Data_Purchase_Model[!(Data_Purchase_Model$CUSTOMER_ID %in% train$CUSTOMER_ID),]
dim(test)

#fit a multinomial logictic model
library(nnet)
mnl_model <- multinom (ProductChoice ~ MembershipPoints + IncomeClass + CustomerPropensity + LastPurchaseDuration, data = train)

#Display the summary of model statistics
mnl_model

#Predict the probabilitilies
predicted_test <- as.data.frame(predict(mnl_model, newdata = test, type= "probs"))

#Display the predcited probabilities
head(predicted_test)

#Do the prediction based in highest probability
test_result <- apply(predicted_test,1,which.max)

table(test_result)
#Combine to get predicted and actuals at one place

result <- as.data.frame(cbind(test$ProductChoice,test_result))

colnames(result) <- c("Actual Class", "Predicted Class")

head(result)

## ----ch07_classi_02, warning=FALSE,message=FALSE-------------------------
#Create the classification matrix
cmat <- as.matrix(table(Actual = result$`Actual Class`, Predicted = result$`Predicted Class`))

#Calculated above mentioned measures in order
n <- sum(cmat) ;
cat("Number of Cases  ", n);

nclass <- nrow(cmat);
cat("Number of classes  ", nclass);

correct_class <- diag(cmat);
cat("Number of Correct Classification  ", correct_class);

rowsums <- apply(cmat, 1, sum);
cat("Number of Instances per class  ", rowsums);

colsums <- apply(cmat, 2, sum);
cat("Number of Instances per predicted class  ", colsums);

actual_dist <- rowsums / n;
cat("Distribution of actuals  ", actual_dist);

predict_dist <- colsums / n;
cat("Distribution of predicted  ", predict_dist);

## ----ch07_classi_03, warning=FALSE,message=FALSE-------------------------
#print the classification matrix - on test data

print(cmat)

#Print Classification Rate

classification_rate <- sum(correct_class)/n;
print(classification_rate)

## ----ch07_classi_05, warning=FALSE,message=FALSE-------------------------
#The analysis is shown for ProductChoice == 1
Actual_Class <- ifelse(result$`Actual Class` == 1,"One","Rest"); 
Predicted_Class <- ifelse(result$`Predicted Class` == 1, "One", "Rest");

ss_analysis <- as.data.frame(cbind(Actual_Class,Predicted_Class));

#Create classification matrix for ProductChoice == 1

cmat_ProductChoice1 <- as.matrix(table(Actual = ss_analysis$Actual_Class, Predicted = ss_analysis$Predicted_Class));

print(cmat_ProductChoice1)

classification_rate_ProductChoice1 <- sum(diag(cmat_ProductChoice1))/n;

cat("Classification rate for ProductChoice 1 is  ", classification_rate_ProductChoice1)

# Calculate TPR and TNR

TPR <- cmat_ProductChoice1[1,1]/(cmat_ProductChoice1[1,1] + cmat_ProductChoice1[1,2]);

cat(" Sensitivity or True Positive Rate is ", TPR);

TNR <- cmat_ProductChoice1[2,2]/(cmat_ProductChoice1[2,1] + cmat_ProductChoice1[2,2])

cat(" Specificity or True Negative Rate is ", TNR);


## ----ch07_classi_06, warning=FALSE,message=FALSE-------------------------
# create a new model on train with "One" =1 and "Rest" = 0

train$ProductChoice_binom <- ifelse(train$ProductChoice == 1,1,0);
test$ProductChoice_binom <- ifelse(test$ProductChoice == 1,1,0);

glm_ProductChoice_binom <- glm( ProductChoice_binom ~ MembershipPoints + IncomeClass + CustomerPropensity + LastPurchaseDuration, data=train, family = binomial(link="logit"))

#Print the summary of binomial logistic model
summary(glm_ProductChoice_binom)

#Now create the performance data set to create AUC curve
library(ROCR)
test_binom <- predict(glm_ProductChoice_binom,newdata=test, type = "response")
pred <- prediction(test_binom, test$ProductChoice_binom)
perf <- performance(pred,"tpr","fpr")

# calculating AUC
auc <- unlist(slot(performance(pred,"auc"),"y.values"));

cat("The Area Under ROC curve for this model is  ",auc);

#Plotting the AUC curve
library(ggplot2)
library(plotROC)
debug <- as.data.frame(cbind(test_binom,test$ProductChoice_binom))
ggplot(debug, aes(d = V2, m = test_binom)) + geom_roc()

## ----ch07_prob_01, warning=FALSE,message=FALSE---------------------------
library(caret)
library(randomForest)
set.seed(917);

#Model training data ( we will show our analysis on this dataset)

train <- Data_House_Price[1:floor(nrow(Data_House_Price)*(2/3)),.(HousePrice,StoreArea,StreetHouseFront,BasementArea,LawnArea,StreetHouseFront,LawnArea,Rating,SaleType)];

#Create the test data which is set 2
test <- Data_House_Price[floor(nrow(Data_House_Price)*(2/3) + 1):nrow(Data_House_Price),.(HousePrice,StoreArea,StreetHouseFront,BasementArea,LawnArea,StreetHouseFront,LawnArea,Rating,SaleType)]

#Omiting the NA from dataset
train <- na.omit(train)
test <- na.omit(test)

#Create the k subsets, let's take k as 10 (i.e., 10-fold cross validation)
k_10_fold <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

#Fit the model on folds and use rmse as metric to fit the model
model_fitted <- train(HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea + StreetHouseFront +	LawnArea +  Rating	+	SaleType, data=train, family = identity,trControl = k_10_fold, tuneLength = 5)

#Display the summary of the cross validation
model_fitted

## ----ch07_prob_02, warning=FALSE,message=FALSE---------------------------

#Create the the boot experiment, let's take samples as as 10 (i.e., 10-sample bootstarped)
boot_10s <- trainControl(method = "boot", number = 10, savePredictions = TRUE)

#Fit the model on bootstraps and use rmse as metric to fit the model
model_fitted <- train(HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea + StreetHouseFront +	LawnArea +  Rating	+	SaleType, data=train, family = identity,trControl = boot_10s, tuneLength = 5)

#Display the summary of the boostraped model
model_fitted

## ----ch07_kappa, warning=FALSE,message=FALSE-----------------------------
library(caret)
library(mlbench)

#We will use the Purchase Prediction Data with a very simple model to illustarte the kappa and accuracy measure 

set.seed(917);
train_kappa <- Data_Purchase_Model[sample(nrow(Data_Purchase_Model),size=5000, replace = TRUE, prob = NULL),]

#train() function confuses between numeric levels, hence convert the dependent into text
train_kappa$ProductChoice_multi <- ifelse(train_kappa$ProductChoice == 1,"A",
                             ifelse(train_kappa$ProductChoice == 2, "B",
                                    ifelse(train_kappa$ProductChoice == 3,"C","D")));

train_kappa <- na.omit(train_kappa)
#Set the experiment
cntrl <- trainControl(method="cv", number=5, classProbs = TRUE)

#Distribution of ProductChoices
table(train_kappa$ProductChoice_multi)

#Making the column names as legitemate names
colnames(train_kappa) <- make.names(names(train_kappa), unique = TRUE, allow_ = TRUE)

#Convert all the factors into factors in R
train_kappa$ProductChoice_multi <- as.factor(train_kappa$ProductChoice_multi)
train_kappa$CustomerPropensity <- as.factor(train_kappa$CustomerPropensity)
train_kappa$LastPurchaseDuration <- as.factor(train_kappa$LastPurchaseDuration)

#Fit the model with method as RandomForest.
model_fitted <- train(ProductChoice_multi ~ CustomerPropensity + LastPurchaseDuration, data=train_kappa, method="rf", metric="Accuracy",trControl=cntrl)

# The result displayed the kappa metrics
print(model_fitted)

#Display the predicted values
pred <- predict(model_fitted, newdata=train_kappa)
confusionMatrix(data=pred, train_kappa$ProductChoice_multi)

