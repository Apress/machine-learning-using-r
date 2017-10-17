
## ----ch06_correlation_01-------------------------------------------------

Data_HousePrice <- read.csv("~/Dropbox/Book Writing - Drafts/Chapter Drafts/Final Artwork and Code/Chapter 6/Dataset/House Sale Price Dataset.csv",header=TRUE);

#Create a vectors with Y-Dependent, X-Independent
y <- Data_HousePrice$HousePrice;
x <- Data_HousePrice$StoreArea;

#Scatter Plot
plot(x,y, main="Scatterplot HousePrice vs StoreArea", xlab="StoreArea(sqft)", ylab="HousePrice($)", pch=19,cex=0.3,col="red")

#Add a fit line to show the relationship direction
abline(lm(y~x)) # regression line (y~x) 
lines(lowess(x,y), col="green") # lowess line (x,y)

#Report the correlation coefficient of this relation
cat("The correlation among HousePrice and StoreArea is  ",cor(x,y));

## ----ch06_simple_linear_reg_01-------------------------------------------
# fit the model
fitted_Model <- lm(y~x)
#Display the summary of the model
summary(fitted_Model)

## ----ch06_simple_linear_reg_02-------------------------------------------
res <- stack(data.frame(Observed = y, Predicted = fitted(fitted_Model)))
res <- cbind(res, x = rep(x, 2))

#Plot using lattice xyplot(function)
library("lattice")
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)

## ----ch06_multiple_regression_01-----------------------------------------
# Use lm to create a multiple linear regression
Data_lm_Model <- Data_HousePrice[,c("HOUSE_ID","HousePrice","StoreArea","StreetHouseFront","BasementArea","LawnArea","Rating","SaleType")];

#Display the missing value spread in data

#library(Amelia)
#missmap(Data_lm_Model, main = "Missing Values", col = "darkred")
sapply(Data_lm_Model, function(x) sum(is.na(x)))

Data_lm_Model <- na.omit(Data_lm_Model)
rownames(Data_lm_Model) <- NULL
#categorical variables has to be set as factors
Data_lm_Model$Rating <- factor(Data_lm_Model$Rating)
Data_lm_Model$SaleType <- factor(Data_lm_Model$SaleType)

## ----ch06_multiple_linear------------------------------------------------

fitted_Model_multiple <- lm(HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea +  Rating	+	SaleType,data=Data_lm_Model)

#Display summary of the model
fitted_Model_multiple$call


## ----ch06_multiple_regression_02-----------------------------------------
#Get the fitted values and create a data frame of actual and predictedget predicted values

actual_predicted <- as.data.frame(cbind(as.numeric(Data_lm_Model$HOUSE_ID),as.numeric(Data_lm_Model$HousePrice),as.numeric(fitted(fitted_Model_multiple))))

names(actual_predicted) <- c("HOUSE_ID","Actual","Predicted")

#Ordered the house by increasing Actual house price
actual_predicted <- actual_predicted[order(actual_predicted$Actual),]
#Find the absolute residual and then take mean of that
library(ggplot2)

#Plot Actual vs Predicted values for Test Cases
ggplot(actual_predicted,aes(x = 1:nrow(Data_lm_Model),color=Series)) + 
  geom_line(data = actual_predicted, aes(x = 1:nrow(Data_lm_Model), y = Actual, color = "Actual")) +
  geom_line(data = actual_predicted, aes(x = 1:nrow(Data_lm_Model), y = Predicted, color = "Predicted"))  +  xlab('House Number') + ylab('House Sale Price')

## ----ch06_multiple_regression_03-----------------------------------------
summary(fitted_Model_multiple)

## ----ch06_multiple_regression_04,error=TRUE------------------------------
library(car);
#### Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(Data_lm_Model)-length(fitted_Model_multiple$coefficients)-2)) 
plot(fitted_Model_multiple, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fitted_Model_multiple,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance",id.location = FALSE) 

#Outlier Plot
outlier.test(fitted_Model_multiple)


## ----ch06_multiple_regression_05-----------------------------------------
#Pull the records with highest leverage

Debug <- Data_lm_Model[c(342,621,1023),]

print(" The observed values for three high leverage points");
Debug

print("Model fitted values for these high leverage points");
fitted_Model_multiple$fitted.values[c(342,621,1023)]

print("Summary of Observed values");

summary(Debug)

## ----ch06_multiple_regression_06-----------------------------------------
library(stats)
library(IDPmisc)
library(MASS)
sresid <- studres(fitted_Model_multiple)

#Remove irregular values (NAN/Inf/NAs)
sresid <- NaRV.omit(sresid)
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals",breaks=25)

xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

## ----ch06_multiple_regression_07-----------------------------------------
# test on normality 
#K-S one sample test
ks.test(fitted_Model_multiple$residuals,pnorm,alternative="two.sided")

#Shapiro Wilk Test
shapiro.test(fitted_Model_multiple$residuals)

#Anderson Darling Test
library(nortest)
ad.test(fitted_Model_multiple$residuals)


## ----ch06_multiple_regression_08-----------------------------------------
library(car)
# calculat teh vif factor
# Evaluate Collinearity
print(" Variance inflation factors are ");
vif(fitted_Model_multiple); # variance inflation factors


print("Tolerance factors are ");
1/vif(fitted_Model_multiple)

# Apply the cut-off to Vif
print("Apply the cut-off of 4 for vif")
vif(fitted_Model_multiple) > 4

# Apply the cut-off to Tolerance
print("Apply the cut-off of 0.2 for vif")
(1/vif(fitted_Model_multiple)) < 0.2


## ----ch06_multiple_regression_09-----------------------------------------
# Test for Autocorrelated Errors
durbinWatsonTest(fitted_Model_multiple)

#ACF Plots
plot(acf(fitted_Model_multiple$residuals))

## ----ch06_multiple_regression_10,error=TRUE------------------------------
# Evaluate homoscedasticity
# non-constant error variance test - breush pagan test
ncvTest(fitted_Model_multiple)

# plot studentized residuals vs. fitted values 
plot(fitted_Model_multiple$residuals,fitted_Model_multiple$fitted.values)

#Bartlett's test
## We can create three groups in data to see if the variance varies across these three groups 
gp<- numeric()

for( i in 1:1069)
{
  if(i<=400){
    gp[i] <- 1;
  }else if(i<=800){
    gp[i] <- 2;
  }else{
    gp[i] <- 3;
  }
}
Data_lm_Model$gp <- factor(gp)
bartlett.test(fitted_Model_multiple$fitted.values,Data_lm_Model$gp)

#also show ARCH test - More relavnt for a time series model
library(FinTS)
ArchTest(fitted_Model_multiple$residuals)

## ----ch06_polynomial_reg_01----------------------------------------------
#Dependent variable : Price of a commodity

y <- as.numeric(c("3.3","2.8","2.9","2.3","2.6","2.1","2.5","2.9","2.4","3.0","3.1","2.8","3.3","3.5","3"));

#Independent variable : Quantity Sold

x<- as.numeric(c("50","55","49","68","73","71","80","84","79","92","91","90","110","103","99"));

#Plot Linear relationship

linear_reg <- lm(y~x)

summary(linear_reg)

res <- stack(data.frame(Observed = as.numeric(y), Predicted = fitted(linear_reg)))
res <- cbind(res, x = rep(x, 2))

#Plot using lattice xyplot(function)
library("lattice")
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)



## ----r ch06_polynomial_reg_02--------------------------------------------
#Plot Linear relationship

linear_reg <- lm(y~x + I(x^2) )

summary(linear_reg)

res <- stack(data.frame(Observed = as.numeric(y), Predicted = fitted(linear_reg)))
res <- cbind(res, x = rep(x, 2))

#Plot using lattice xyplot(function)
library("lattice")
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)


## ----ch06_logit_curve----------------------------------------------------
curve((1/(1+ exp(-x))),-10,10,col = "violet")

## ----ch06_logistic_reg_01------------------------------------------------
#Load the data and prepare a dataset for logistic regression
Data_Purchase_Prediction <- read.csv("~/Dropbox/Book Writing - Drafts/Chapter Drafts/Final Artwork and Code/Chapter 6/Dataset/Purchase Prediction Dataset.csv",header=TRUE);

Data_Purchase_Prediction$choice <- ifelse(Data_Purchase_Prediction$ProductChoice == 1,1,
                                          ifelse(Data_Purchase_Prediction$ProductChoice == 3,0,999));

Data_Logistic <- Data_Purchase_Prediction[Data_Purchase_Prediction$choice %in% c("0","1"),c("CUSTOMER_ID","choice","MembershipPoints","IncomeClass","CustomerPropensity","LastPurchaseDuration")]

table(Data_Logistic$choice,useNA="always")


Data_Logistic$MembershipPoints <- factor(Data_Logistic$MembershipPoints)
Data_Logistic$IncomeClass <- factor(Data_Logistic$IncomeClass)
Data_Logistic$CustomerPropensity <- factor(Data_Logistic$CustomerPropensity)
Data_Logistic$LastPurchaseDuration <- as.numeric(Data_Logistic$LastPurchaseDuration)

print("Before we start the model let's see the distribution of categorical variables over dependent categorical variables");

table(Data_Logistic$MembershipPoints,Data_Logistic$choice)
table(Data_Logistic$IncomeClass,Data_Logistic$choice)
table(Data_Logistic$CustomerPropensity,Data_Logistic$choice)


## ----ch06_logistic_reg_03------------------------------------------------
library(dplyr)
#Get the average rate of Lapse by Rating and plot that

summary_Rating <- summarise(group_by(Data_Logistic,IncomeClass),Average_Rate=mean(choice))

print("Summary of Average Purchase Rate by IncomeClass")
summary_Rating

plot(summary_Rating$IncomeClass,summary_Rating$Average_Rate,type="b", xlab="Income Class", ylab="Average Purchase Rate observed", main="Purchase Rate and Income Class")


## ----ch06_logistic_reg_04------------------------------------------------
#Remove the Missing values - NAs

Data_Logistic <- na.omit(Data_Logistic)
rownames(Data_Logistic) <- NULL

#Divide the data into Train and Test
set.seed(917);
index <- sample(1:nrow(Data_Logistic),round(0.7*nrow(Data_Logistic)))
train <- Data_Logistic[index,]
test <- Data_Logistic[-index,]

##Fitting a logistic model

Model_logistic <- glm( choice ~ MembershipPoints + IncomeClass + CustomerPropensity + LastPurchaseDuration, data = train, family = binomial(link = 'logit'));

summary(Model_logistic)


## ----ch06_classifier_cut_off_01------------------------------------------
#install and load package
library(pROC)

#apply roc function
cut_off <- roc(response=train$choice, predictor=Model_logistic$fitted.values)

#Find threshold that minimizes error
e <- cbind(cut_off$thresholds,cut_off$sensitivities+cut_off$specificities)
best_t <- subset(e,e[,2]==max(e[,2]))[,1]

#Plot ROC Curve
plot(1-cut_off$specificities,cut_off$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="green",lwd=2,
     main = "ROC Curve for Train")
abline(a=0,b=1)

abline(v = best_t) #add optimal t to ROC curve
cat(" The best value of cut-off for classifier is ", best_t)

## ----ch06_classifier_cut_off_02------------------------------------------

# Predict the probabilities for test and apply the cut-off
predict_prob <- predict(Model_logistic, newdata=test, type="response")

#Apply the cut off to get the class

class_pred <- ifelse(predict_prob > 0.41,1,0)

#Classiifcation table
table(test$choice,class_pred)

#Clasisifcation rate
sum(diag(table(test$choice,class_pred))/nrow(test))

## ----ch06_logistic_reg_07------------------------------------------------
#Wald test
library(survey)
regTermTest(Model_logistic,"MembershipPoints", method = "Wald")

## ----ch06_logistic_reg_05------------------------------------------------
#Anova table of significance
anova(Model_logistic, test="Chisq")


## ----ch06_rsq_equivalent-------------------------------------------------
# R square equivalent for logistic regression
library(pscl)
pR2(Model_logistic)

## ----ch06_logistic_reg_08------------------------------------------------
#The function code is provided seperately in the appendix
source("actual_pred_plot.R")

MODEL_PREDICTION <- predict(Model_logistic, Data_Logistic, type = 'response');
Data_Logistic$MODEL_PREDICTION <- MODEL_PREDICTION
#Print the plots MembershipPoints
actual_pred_plot  (var.by= as.character("MembershipPoints"),
                var.response='choice',
                data=Data_Logistic,
                var.predict.current='MODEL_PREDICTION',
                var.predict.reference=NULL,
                var.split=NULL,
                var.by.buckets=NULL,
                sort.factor=FALSE,
                errorbars=FALSE,
                subset.to=FALSE,
                barline.ratio=1,
                title="Actual vs. Predicted Purchase Rates",
                make.plot=TRUE
                )

## ----ch06_logistic_reg_09------------------------------------------------
#Print the plots IncomeClass
actual_pred_plot  (var.by= as.character("IncomeClass"),
                var.response='choice',
                data=Data_Logistic,
                var.predict.current='MODEL_PREDICTION',
                var.predict.reference=NULL,
                var.split=NULL,
                var.by.buckets=NULL,
                sort.factor=FALSE,
                errorbars=FALSE,
                subset.to=FALSE,
                barline.ratio=1,
                title="Actual vs. Predicted Purchase Rates",
                make.plot=TRUE
                )

## ----ch06_logistic_reg_10------------------------------------------------
#Print the plots CustomerPropensity
actual_pred_plot  (var.by= as.character("CustomerPropensity"),
                var.response='choice',
                data=Data_Logistic,
                var.predict.current='MODEL_PREDICTION',
                var.predict.reference=NULL,
                var.split=NULL,
                var.by.buckets=NULL,
                sort.factor=FALSE,
                errorbars=FALSE,
                subset.to=FALSE,
                barline.ratio=1,
                title="Actual vs. Predicted Purchase Rates",
                make.plot=TRUE
                )

## ----ch06_logistic_reg_11------------------------------------------------
library(gains)
library(ROCR)
library(calibrate)
#############################################  Gains Chart   ###########################################
MODEL_PREDICTION <- predict(Model_logistic, Data_Logistic, type = 'response');

Data_Logistic$MODEL_PREDICTION <- MODEL_PREDICTION;

lift = with(Data_Logistic, gains(actual = Data_Logistic$choice, predicted = Data_Logistic$MODEL_PREDICTION , optimal = TRUE));

pred = prediction(MODEL_PREDICTION,as.numeric(Data_Logistic$choice));

# Function to create performance objects. All kinds of predictor evaluations are performed using this function.
gains = performance(pred, 'tpr', 'rpp');
# tpr: True positive rate
# rpp: Rate of positive predictions
auc = performance(pred, 'auc');
auc = unlist(slot(auc, 'y.values')); # The same as: auc@y.values[[1]] 
auct = paste(c('AUC = '), round(auc, 2), sep = '')


##
#par(mfrow=c(1,2), mar=c(6,5,4,2));
##
plot(gains, col='red', lwd=2, xaxs='i', yaxs='i', main = paste('Gains Chart ', sep = ''),ylab='% of Positive Response', xlab='% of customers/population');
axis(side = 1, pos = 0, at = seq(0, 1, by = 0.10));
axis(side = 2, pos = 0, at = seq(0, 1, by = 0.10));

lines(x=c(0,1), y=c(0,1), type='l', col='black', lwd=2,
      ylab='% of Positive Response', xlab='% of customers/population');

legend(0.6, 0.4, auct, cex = 1.1, box.col = 'white')

gains = lift$cume.pct.of.total
deciles = length(gains);

for (j in 1:deciles)
{
  x = 0.1;
  y = as.numeric(as.character(gains[[j]]));
  lines(x = c(x*j, x*j),
        y = c(0, y),
        type = 'l', col = 'blue', lwd = 1);
  lines(x = c(0, 0.1*j),
        y = c(y, y),
        type = 'l', col = 'blue', lwd = 1);
  # Annotating the chart by adding the True Positive Rate exact numbers at the specified deciles.
  textxy(0, y, paste(round(y,2)*100, '%',sep=''), cex=0.9);
}

########################################  Lift Chart  ####################################

plot(lift,  
     xlab = '% of customers/population', 
     ylab = 'Actual Lift', 
     main = paste('Lift Chart \n', sep = ' '),
     xaxt = 'n');
axis(side = 1, at = seq(0, 100, by = 10), las = 1, hadj = 0.4);


## ----ch06_logistic_reg_12------------------------------------------------
#The function code is provided seperately in R-code for chapter 6
source("concordance.R")

#Call the concordance function to get these ratios
concordance(Model_logistic)

## ----ch06_logistic_reg_15------------------------------------------------
#Remove the data having NA. NA is ignored in modeling algorithms
Data_Purchase<-na.omit(Data_Purchase_Prediction)

rownames(Data_Purchase)<- NULL

#Random Sample for easy computation
Data_Purchase_Model <- Data_Purchase[sample(nrow(Data_Purchase),10000),]

print("The Distribution of product is as below")
table(Data_Purchase_Model$ProductChoice)

#fit a multinomial logictic model
library(nnet)
mnl_model <- multinom (ProductChoice ~ MembershipPoints + IncomeClass + CustomerPropensity + LastPurchaseDuration + CustomerAge + MartialStatus, data = Data_Purchase)

#Display the summary of model statistics
mnl_model

## ----ch06_multinom_04----------------------------------------------------
#Predict the probabilitilies
predicted_test <- as.data.frame(predict(mnl_model, newdata = Data_Purchase, type= "probs"))

head(predicted_test)
#Do the prediction based in highest probability
test_result <- apply(predicted_test,1,which.max)

result <- as.data.frame(cbind(Data_Purchase$ProductChoice,test_result))

colnames(result) <- c("Actual Class", "Predicted Class")

table(result$`Actual Class`,result$`Predicted Class`)

## ----ch06_logistic_reg_16------------------------------------------------
prior <- table(Data_Purchase_Model$ProductChoice)/nrow(Data_Purchase_Model)

prior_mat <- rep(prior,nrow(Data_Purchase_Model))

pred_ratio <- predicted_test/prior_mat
#Do the prediction based in highest ratio
test_result <- apply(pred_ratio,1,which.max)

result <- as.data.frame(cbind(Data_Purchase$ProductChoice,test_result))

colnames(result) <- c("Actual Class", "Predicted Class")

table(result$`Actual Class`,result$`Predicted Class`)


