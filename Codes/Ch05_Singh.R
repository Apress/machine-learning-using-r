## ------------------------------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96)
knitr::opts_chunk$set(
  fig.path = "Images/"
)

## ----ch_05_Loading the data, warning=FALSE-------------------------------
setwd("C:/Personal/Machine Learning/Final Artwork and Code/Chapter 5")

##Input the data and store in data table

library(data.table)

data <-fread ("Dataset/Loan Default Prediction.csv",header=T, verbose = FALSE, showProgress = TRUE)

dim(data)


## ----ch_05_1_Type_of_Data_01---------------------------------------------

#Summary of the data
summary(data$loss)



## ----ch_05_1_Type_of_Data_02---------------------------------------------
hist(data$loss, 
     main="Histogram for Loss Distribution ", 
     xlab="Loss", 
     border="blue", 
     col="red",
     las=0,
     breaks=100,
     prob = TRUE)


## ----ch_05_1_Type_of_Data_03---------------------------------------------

#Sub-set the data into NON Loss and Loss ( e.g., loss > 0)

subset_loss <- subset(data,loss !=0)

#Distrbution of cases where there is some loss regestered

hist(subset_loss$loss, 
     main="Histogram for Loss Distribution (Default cases) ", 
     xlab="Loss", 
     border="blue", 
     col="red",
     las=0,
     breaks=100,
     prob = TRUE)

## ----ch_05_1_Type_of_Data_04---------------------------------------------

#Create the default variable

data[,default := ifelse(data$loss == 0, 0,1)]

#Distribution of defaults
table(data$default)
                      
#Event rate is defined as ratio of default cases in total population

print(table(data$default)*100/nrow(data))


## ----ch_05_2_Feature_type_01---------------------------------------------

continuos <- character()
categorical <- character()
#Write a loop to go over all features and find unique values
p<-1
q<-1
for (i in names(data))
{
  unique_levels = length(unique(data[,get(i)]))
  
  if(i %in% c("id","loss","default"))
  {
    next;
  }
  
  else
  {
    if (unique_levels <=30 | is.character(data[,get(i)]))
    {
  #    cat("The feature ", i, " is a categorical variable")
      categorical[p] <- i
      p=p+1
  #  Makingh the 
      data[[i]] <- factor(data[[i]])
    }
    else
    {
  #    cat("The feature ", i, " is a continuos variable")
      continuos[q] <- i
      q=q+1
      
    }
  }
}

# subtract 1 as one is dependent variable = default
cat("\nTotal number of continuos variables in feature set ", length(continuos) - 1)

# subtract 2 as one is loss and one is id
cat("\nTotal number of categorical variable in feature set ", length(categorical) - 2)


## ----ch05_feature_ranking_01, warning=FALSE------------------------------

library(MLmetrics)

performance_metric_gini <- data.frame(feature = character(), Gini_value = numeric())

#Write a loop to go over all features and find unique values
for (feature in names(data))
{
  if(feature %in% c("id","loss","default"))
  {
    next;
  }
  else
  {
      tryCatch({glm_model <- glm(default ~ get(feature),data=data,family=binomial(link="logit"));
  
      predicted_values <- predict.glm(glm_model,newdata=data,type="response");
      
      Gini_value <- Gini(predicted_values,data$default);
      
      performance_metric_gini <- rbind(performance_metric_gini,cbind(feature,Gini_value));},error=function(e){})
      
  }
}

performance_metric_gini$Gini_value <- as.numeric(as.character(performance_metric_gini$Gini_value))
#Rank the features by value of Gini Coefficient

Ranked_Features <- performance_metric_gini[order(-performance_metric_gini$Gini_value),]

print("Top 5 Features by Gini Coefficients\n")

head(Ranked_Features)


## ----ch05_feature_ranking_02, message=FALSE,warning=FALSE----------------
#Create a logistic model with top 6 features (f766,f404,f629,f630,f281 and f322)

glm_model <- glm(default ~ f766 + f404 + f629 + f630 + f281 + f322,data=data,family=binomial(link="logit"));
  
predicted_values <- predict.glm(glm_model,newdata=data,type="response");
      
Gini_value <- Gini(predicted_values,data$default);

summary(glm_model)

Gini_value

## ----ch05_feature_ranking_03---------------------------------------------
#Create the correlation matrix for 6 features (f766,f404,f629,f630,f281 and f322)

top_6_feature <- data.frame(data$f766,data$f404,data$f629,data$f630,data$f281,data$f322)

cor(top_6_feature, use="complete")

## ----ch05_filter_method_01, warning=FALSE--------------------------------
#Calculate the variance of each individual variable and standardize the variance by dividing with mean()

coefficient_of_variance <- data.frame(feature = character(), cov = numeric())

#Write a loop to go over all features and calculate variance
for (feature in names(data))
{
  if(feature %in% c("id","loss","default"))
  {
    next;
  }
  else if(feature %in% continuos)
  {
      tryCatch(
      {cov  <- abs(sd(data[[feature]], na.rm = TRUE)/mean(data[[feature]],na.rm = TRUE));
      if(cov != Inf){
      coefficient_of_variance <- rbind(coefficient_of_variance,cbind(feature,cov));} else {next;}},error=function(e){})
      
  }
  else
  {
    next;
  }
}

coefficient_of_variance$cov <- as.numeric(as.character(coefficient_of_variance$cov))

#Order the list by highest to lowest coefficient of variation

Ranked_Features_cov <- coefficient_of_variance[order(-coefficient_of_variance$cov),]

print("Top 5 Features by Coefficient of Variance\n")

head(Ranked_Features_cov)


## ----ch05_filter_method_02-----------------------------------------------
#Create a logistic model with top 6 features (f338,f422,f724,f636,f775 and f723)

glm_model <- glm(default ~ f338 + f422 + f724 + f636 + f775 + f723,data=data,family=binomial(link="logit"));
  
predicted_values <- predict.glm(glm_model,newdata=data,type="response"); 
      
Gini_value <- Gini(predicted_values,data$default);

summary(glm_model)

cat("The Gini Coefficient for the fitted model is  ",Gini_value);

## ----ch05_filter_method_03-----------------------------------------------
#Create the correlation matrix for 6 features (f338,f422,f724,f636,f775 and f723)

top_6_feature <- data.frame(as.double(data$f338),as.double(data$f422),as.double(data$f724),as.double(data$f636),as.double(data$f775),as.double(data$f723))

cor(top_6_feature, use="complete")

## ----ch05_wrapper_method_01----------------------------------------------
#Pull 5 variables we had from highest coefficient of variation (from filter method)(f338,f422,f724,f636 and f775)

predictor_set <- c("f338","f422","f724","f636","f775")

#Randomly Pull 5 variables from categorical variable set ( Reader can apply filter method to categorical variable and can choose these 5 variables systemetically as well)
set.seed(101);
ind <- sample(1:length(categorical), 5, replace=FALSE)
p<- 1
for (i in ind)
{
  predictor_set [5+p] <- categorical[i]
  p=p+1
}

#Print the set of 10 variables we will be working with

print(predictor_set)

#Replaced f33 by f93 as f33 does not have levels

predictor_set[7] <- "f93"

#Print final list of variables

print(predictor_set)

## ----ch05_wrapper_method_02----------------------------------------------
# Create a samll modeling datset with only predictors and dependent variable
library(data.table)
data_model <- data[,.(id,f338,f422,f724,f636,f775,f222,f93,f309,f303,f113,default),]
#make sure to remove the missing cases to resolve erros regarding null values

data_model<-na.omit(data_model)
                
#Full model uses all the 10 variables
full_model <- glm(default ~ f338 + f422 + f724 + f636 + f775 + f222 + f93 + f309 + f303 + f113,data=data_model,family=binomial(link="logit"))

#Summary of the full model
summary(full_model)

## ----ch05_wrapper_method_03----------------------------------------------
#Null model uses no variables
null_model <- glm(default ~ 1 ,data=data_model,family=binomial(link="logit"))

#Summary of the full model
summary(null_model)

## ----ch05_wrapper_method_04----------------------------------------------
#Summary of backward step selection
backwards <- step(full_model)


## ----ch05_wrapper_method_05----------------------------------------------
#summary of forward selection method
forwards <- step(null_model,scope=list(lower=formula(null_model),upper=formula(full_model)), direction="forward")


## ----ch05_wrapper_method_06----------------------------------------------
#Summary of final model with backward selection process
formula(backwards)

#Summary of final model with forward selection process
formula(forwards)

## ----ch05_embeded_method_01, warning=FALSE,message=FALSE-----------------

#Create data farme with dependent and independent variables (Remove NA)

data_model <- na.omit(data)

y <- as.matrix(data_model$default)

x <- as.matrix(subset(data_model, select=continuos[250:260]))
  
library("glmnet")

## ----ch05_embeded_method_02, warning=FALSE,message=FALSE-----------------

#Fit a model with dependent variable of binomial family
fit = glmnet(x,y, family= "binomial")

#Summary of fit model 
summary(fit)

#Plot the output of glmnet fit model
plot (fit, xvar= "dev", label=TRUE)

## ----ch05_embeded_method_03, warning=FALSE,message=FALSE-----------------

#Fit a cross validated binomial model
fit_logistic = cv.glmnet(x,y, family= "binomial", type.measure = "class")

#Summary of fitted Cross Validated Linear Model

summary(fit_logistic)

#Plot the results
plot (fit_logistic)

## ----ch05_embeded_method_04----------------------------------------------
#Print the minimum lambda - regularization factor
print(fit_logistic$lambda.min)

print(fit_logistic$lambda.1se)

#Against the lamda minimum value we can get the coefficients
param <- coef(fit_logistic, s="lambda.min")

param <- as.data.frame(as.matrix(param))

param$feature<-rownames(param)

#The list of variables suggested by the embeded method

param_embeded <- param[param$`1` > 0,]

print(param_embeded)

## ----ch05_principle_component_01-----------------------------------------
#Take a subset of 10 features
pca_data <- data[,.(f381,f408,f495,f529,f549,f539,f579,f634,f706,f743)]

pca_data <- na.omit(pca_data)

head(pca_data)

#Normalise the data before applying PCA analysis mean=0, and sd=1
scaled_pca_data <- scale(pca_data)

head(scaled_pca_data)

#Do the decomposition on the scaled series
pca_results <- prcomp(scaled_pca_data)

print(pca_results)

summary(pca_results)

plot(pca_results)

#Create the biplot with principle components
biplot(pca_results, col = c("red", "blue"))


