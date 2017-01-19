## ----Ch03_3.2.1, echo=FALSE, cache=TRUE, message=FALSE, warning=FALSE----

library(data.table)

data <- fread("Dataset/ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE)

US_state <- fread("Dataset/US_State_Code_Mapping.csv",header=T, showProgress = FALSE )
data<-merge(data, US_state, by = 'state')

Gender_map<-fread("Dataset/Gender Map.csv",header=T)
data<-merge(data, Gender_map, by = 'gender')

Credit_line<-fread("Dataset/credit line map.csv",header=T)
data<-merge(data, Credit_line, by = 'creditLine')

setnames(data,"custID","CustomerID")
setnames(data,"code","Gender")
setnames(data,"numTrans","DomesTransc")
setnames(data,"numIntlTrans","IntTransc")
setnames(data,"fraudRisk","FraudFlag")
setnames(data,"cardholder","NumOfCards")
setnames(data,"balance","OutsBal")
setnames(data,"StateName","State")

data$creditLine <- NULL
data$gender <- NULL
data$state <- NULL
data$PostalCode <- NULL

#Run Below code if you want to store the transformed data.
#write.csv(data,"Dataset/Credit Card Fraud Dataset.csv",row.names = FALSE)

#Describe the data 
str(data)

## ----3_2_Population_Mean-------------------------------------------------
Population_Mean_P <- mean(data$OutsBal)
cat("The average outstanding balance on cards is ",Population_Mean_P)

## ----3_3_Population_Variance---------------------------------------------
Population_Variance_P <- var(data$OutsBal)
cat("The variance in the average outstanding balance is ",Population_Variance_P)

cat("Standard deviation of outstanding balance is", sqrt(Population_Variance_P))

## ----3_4_Pooled_Mean_Variance--------------------------------------------
set.seed(937)
i<-1
n<-rbind(10000,20000,40000,80000,100000)
Sampling_Fraction<-nrow(data)/n
sample_mean<-numeric()
sample_variance<-numeric()
for(i in 1:5)
{
    sample_100K <- data[sample(nrow(data),size=n[i], replace = FALSE, prob = NULL),]
    sample_mean[i]<- round(mean(sample_100K$OutsBal),2)
    sample_variance[i] <- round(var(sample_100K$OutsBal),2)
}

Sample_statistics <- cbind (1:5,c('10K','20K','40K','80K','100K'),sample_mean,sample_variance,round(sqrt(sample_variance),2),Sampling_Fraction)

knitr::kable(Sample_statistics, col.names = c("S.No.", "Size","Sample_Mean","Sample_Variance","Sample SD","Sample_Fraction"))

## ----3_5_Population_Mean_from_Sample_Mean--------------------------------
i<-1
Population_mean_Num<-0
Population_mean_Den<-0
for(i in 1:5)
{
  Population_mean_Num = Population_mean_Num + sample_mean[i]*n[i]
  Population_mean_Den = Population_mean_Den + n[i]
}

Population_Mean_S<-Population_mean_Num/Population_mean_Den

cat("The pooled mean ( estimate of population mean) is",Population_Mean_S)

## ----3_6_Population_Variance_from_Sample_Variance------------------------
i<-1
Population_variance_Num<-0
Population_variance_Den<-0
for(i in 1:5)
{
  Population_variance_Num = Population_variance_Num + (sample_variance[i])*(n[i] - 1)
  Population_variance_Den = Population_variance_Den + n[i] - 1
}

Population_Variance_S<-Population_variance_Num/Population_variance_Den

Population_SD_S<-sqrt(Population_Variance_S)

cat("The pooled variance (estimate of population variance) is", Population_Variance_S)

cat("The pooled standard deviation (estimate of population standard deviation) is", sqrt(Population_Variance_S))

## ----3_7_Mean_Variance_Comparision, results='asis'-----------------------

SamplingError_percent_mean<- round((Population_Mean_P - sample_mean)/Population_Mean_P,3)
SamplingError_percent_variance<-round((Population_Variance_P - sample_variance)/Population_Variance_P,3)

Com_Table_1<-cbind(1:5,c('10K','20K','40K','80K','100K'),Sampling_Fraction,SamplingError_percent_mean,SamplingError_percent_variance)

knitr::kable(Com_Table_1, col.names = c("S.No.","Size","Sampling_Frac","Sampling_Error_Mean(%)","Sampling_Error_Variance(%)"))


## ----3_8_Mean_Variance_Comparision_contd---------------------------------
SamplingError_percent_mean<-(Population_Mean_P - Population_Mean_S)/Population_Mean_P
SamplingError_percent_variance<-(Population_Variance_P - Population_Variance_S)/Population_Variance_P

Com_Table_2 <- cbind(Population_Mean_P,Population_Mean_S,SamplingError_percent_mean)
Com_Table_3 <- cbind(Population_Variance_P,Population_Variance_S,SamplingError_percent_variance)

knitr::kable(Com_Table_2)
knitr::kable(Com_Table_3)


## ----3_9_Law_of_Large_Numbers_S1, warning=FALSE, message=FALSE-----------
# Set parameters for a binomial distribution Binomial(n, p)
# n -> no. of toss
# p -> probability of getting a head
library(data.table)
n <- 100
p <- 0.6


## ----3_10_Law_of_Large_Numbers_S2----------------------------------------
#Create a data frame with 100 values selected samples from Binomial(1,p)

dt <- data.table(binomial = rbinom(n, 1, p)  ,count_of_heads = 0, mean = 0)

# Setting the first observation in the data frame

ifelse(dt$binomial[1] == 1, dt[1, 2:3] <- 1, 0)

## ----3_11_Law_of_Large_Numbers_S3_S4-------------------------------------
# Let's run a experiment large number of times (till n) and see how the average of heads -> probability of heads converge to a value

for (i in 2 : n)
  {
    dt$count_of_heads[i] <- ifelse(dt$binomial[i] == 1, dt$count_of_heads[i]<-dt$count_of_heads[i - 1]+1, dt$count_of_heads[i - 1])
    dt$mean[i] <- dt$count_of_heads[i] / i
}

## ----3_12_Law_of_Large_Numbers_S5,echo=TRUE------------------------------
# Plot the average no. of heads -> probability of heads at each experiment stage
plot(dt$mean, type='l', main = "Simulation of average no. of heads",
     xlab="Size of Sample", ylab="Sample mean of no. of Heads")
abline(h = p, col="red")

## ----3_13_CLT_S1---------------------------------------------------------
#Number of samples
r<-5000
#Size of each sample
n<-10000

## ----3_14_CLT_S2---------------------------------------------------------
#Produce a matrix of observations with  n columns and r rows. Each row is one sample
lambda<-0.6
Exponential_Samples = matrix(rexp(n*r,lambda),r)

## ----3_15_CLT_S3---------------------------------------------------------
  all.sample.sums <- apply(Exponential_Samples,1,sum)
  all.sample.means <- apply(Exponential_Samples,1,mean)   
  all.sample.vars <- apply(Exponential_Samples,1,var) 


## ----3_16_CLT_S4---------------------------------------------------------
  par(mfrow=c(2,2))
  hist(Exponential_Samples[1,],col="gray",main="Distribution of One Sample")
  hist(all.sample.sums,col="gray",main="Sampling Distributionn of
       the Sum")
  hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean")
  hist(all.sample.vars,col="gray",main="Sampling Distributionnof
       the Variance")
 par(mfrow=c(1,1)) 

## ----3_16_CLT_S4_OtherDistributions, eval=FALSE--------------------------
## Normal_Samples = matrix(rnorm(n*r,param1,param2),r),
## 
## Uniform_Samples = matrix(runif(n*r,param1,param2),r),
## 
## Poisson_Samples = matrix(rpois(n*r,param1),r),
## 
## Cauchy_Samples = matrix(rcauchy(n*r,param1,param2),r),
## 
## Bionomial_Samples = matrix(rbinom(n*r,param1,param2),r),
## 
## Gamma_Samples = matrix(rgamma(n*r,param1,param2),r),
## 
## ChiSqr_Samples = matrix(rchisq(n*r,param1),r),
## 
## StudentT_Samples = matrix(rt(n*r,param1),r))

## ----3_17_CLT_Normality_test---------------------------------------------
#Do a formal test of normality on the distribution of sample means
  
  Mean_of_sample_means <- mean (all.sample.means)
  Variance_of_sample_means <- var(all.sample.means)

  # testing normality by Shapiro  wilk test  
  shapiro.test(all.sample.means)
  
  

## ----3_18_CLT_distribution-----------------------------------------------

x <-  all.sample.means
h<-hist(x, breaks=20, col="red", xlab="Sample Means", 
    main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=Mean_of_sample_means,sd=sqrt(Variance_of_sample_means)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


## ----3_19_Population_Discription-----------------------------------------
str(data)

## ----3_20_Population_Means-----------------------------------------------
mean_outstanding_balance <- mean(data$OutsBal)
mean_outstanding_balance
mean_international_trans <- mean(data$IntTransc)
mean_international_trans
mean_domestic_trans <- mean(data$DomesTransc)
mean_domestic_trans


## ----3_21_Population_Variance--------------------------------------------
Var_outstanding_balance <- var(data$OutsBal)
Var_outstanding_balance
Var_international_trans <- var(data$IntTransc)
Var_international_trans
Var_domestic_trans <- var(data$DomesTransc)
Var_domestic_trans

## ----3_22_Population_distribution----------------------------------------
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", 
  	main="Distribution of Outstanding Balance")


hist(data$IntTransc, breaks=20, col="blue", xlab="Number of International Transactions", 
  	main="Distribution of International Transactions")

hist(data$DomesTransc, breaks=20, col="green", xlab="Number of Domestic Transactions", 
  	main="Distribution of Domestic Transactions")


## ----3_23_Simple_Random_Sampling_Population_by_cardType, , message=FALSE, warning=FALSE----
#Population Data : Distribution of Outstanding Balance across Card Type

library(dplyr)

summarise(group_by(data,CardType),Population_OutstandingBalance=mean(OutsBal))

## ----3_24_Simple_Random_Sampling_Sample_SOR_100K-------------------------
set.seed(937)
# Simple Random Sampling Without Replacment
library("base")

sample_SOR_100K <- data[sample(nrow(data),size=100000, replace = FALSE, prob = NULL),]

## ----3_25_Simple_Random_Sampling_comparison_by_card_type-----------------
#Sample Data : Distribution of Outstanding Balance across Card Type

library(dplyr)

summarise(group_by(sample_SOR_100K,CardType),Sample_OutstandingBalance=mean(OutsBal))

## ----3_26_Simple_Random_Sampling_Distribution_testing--------------------
#Testing if the sampled data comes from population or not. This makes sure that sampling does not change the original distribution
ks.test(data$OutsBal,sample_SOR_100K$OutsBal,alternative="two.sided")

## ----3_27_Simple_Random_Sampling_Distribution_testing--------------------
par(mfrow = c(1,2))
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", 
  	main="Population")

hist(sample_SOR_100K$OutsBal, breaks=20, col="green", xlab="Outstanding Balance", 
  	main="Random Sample Data")
par(mfrow = c(1,1))

## ----3_28_Simple_Random_Sampling_Mean_t_test-----------------------------
# Lets also do t.test for the mean of population and sample. 

t.test(data$OutsBal,sample_SOR_100K$OutsBal)

## ----3_29_Simple_Random_Sampling_with_replacement------------------------
set.seed(937)
# Simple Random Sampling With Replacment
library("base")

sample_SR_100K <- data[sample(nrow(data),size=100000, replace = TRUE, prob = NULL),]

## ----3_31_Simple_Random_Sampling_with_replacement_ks.test----------------
ks.test(data$OutsBal,sample_SR_100K$OutsBal,alternative="two.sided")

## ----3_32_Simple_Random_Sampling_with_replacement_distribution-----------
par(mfrow = c(1,2))
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", 
  	main="Population")

hist(sample_SR_100K$OutsBal, breaks=20, col="green", xlab="Outstanding Balance", 
  	main="Random Sample Data(WR)")
par(mfrow = c(1,1))

## ----3_33_Simple_Random_Sampling_Mean_Compare, warning=FALSE, message=FALSE----
population_summary <- summarise(group_by(data,CardType),OutstandingBalance_Population=mean(OutsBal))

random_WOR_summary<-summarise(group_by(sample_SOR_100K,CardType),OutstandingBalance_Random_WOR=mean(OutsBal))
random_WR_summary<-summarise(group_by(sample_SR_100K,CardType),OutstandingBalance_Random_WR=mean(OutsBal))


compare_population_WOR<-merge(population_summary,random_WOR_summary, by="CardType")
compare_population_WR <-merge(population_summary,random_WR_summary, by="CardType")

summary_compare<-cbind(compare_population_WOR,compare_population_WR$OutstandingBalance_Random_WR)
colnames(summary_compare)[which(names(summary_compare) == "V2")] <- "OutstandingBalance_Random_WR"

knitr::kable(summary_compare, col.names = c("C_Type", "OutBal_Population","OutBal_Random_WOR", "OutBal_Random_WR"))


## ----3_34_Systematic_Sampling_Homogenous_set-----------------------------
Data_Subset <- subset(data, IntTransc==0 & DomesTransc<=3)
summarise(group_by(Data_Subset,CardType),OutstandingBalance=mean(OutsBal))

## ----3_35_Systematic_Sampling_Size---------------------------------------
#Size of population ( here the size of card holders from Data Subset)
Population_Size_N<-length(Data_Subset$OutsBal)

# Set a the size of sample to pull (should be less than N), n. We will assume n=5000

Sample_Size_n<-5000

## ----3_36_Systematic_Sampling_Skip_factor--------------------------------
#Calculate the skip factor 

k = ceiling(Population_Size_N/Sample_Size_n)

#ceiling(x) rounds to the nearest integer that’s larger than x. 
#This means ceiling (2.3) = 3

cat("The skip factor for systematic sampling is ",k)

## ----3_37_Systematic_Sampling_sample-------------------------------------
r = sample(1:k, 1)
systematic_sample_index = seq(r, r + k*(Sample_Size_n-1), k)

## ----3_37_Systematic_Sampling_sample_draw--------------------------------
systematic_sample_5K<-Data_Subset[systematic_sample_index,]

## ----3_38_Simple_Random_Sampling_from_data_subset------------------------
set.seed(937)
# Simple Random Sampling Without Replacment
library("base")

sample_Random_5K <- Data_Subset[sample(nrow(Data_Subset),size=5000, replace = FALSE, prob = NULL),]

## ----3_39_Systematic_Random_Sampling_mean_compare------------------------
sys_summary <- summarise(group_by(systematic_sample_5K,CardType),OutstandingBalance_Sys=mean(OutsBal))
random_summary<-summarise(group_by(sample_Random_5K,CardType),OutstandingBalance_Random=mean(OutsBal))

summary_mean_compare<-merge(sys_summary,random_summary, by="CardType")

print(summary_mean_compare)

## ----3_40_Systematic_Random_Sampling_distributiontest--------------------
ks.test(Data_Subset$OutsBal,systematic_sample_5K$OutsBal,alternative="two.sided")

## ----3_41_Systematic_Random_Sampling_without_replacement_distribution----
par(mfrow = c(1,2))
hist(Data_Subset$OutsBal, breaks=50, col="red", xlab="Outstanding Balance", 
  	main="Homogenous Subset Data")

hist(systematic_sample_5K$OutsBal, breaks=50, col="green", xlab="Outstanding Balance", 
  	main="Systematic Sample")

par(mfrow = c(1,1))


## ----3_42_Stratified_Random_Sampling_Stratified--------------------------

#Frequency table for CardType in Population
table(data$CardType)

#Frequency table for State in Population
table(data$State)

#Cross table frequency for population data
table(data$State,data$CardType)


## ----3_42_Stratified_Random_Sampling_10%_Sampled-------------------------
set.seed(937)
#We want to make sure that our sampling retain the same proportion of the cardtype in the sample
#Do choose a random sample without replacement from each startum consiting of 10% of total size of stratum

library(splitstackshape)
stratified_sample_10_percent<-stratified(data, group=c("CardType","State"),size=0.1,replace=FALSE)

## ----3_42_Stratified_Random_Sampling_frequency---------------------------
#Frequency table for CardType in sample
table(stratified_sample_10_percent$CardType)

#Frequency table for State in sample
table(stratified_sample_10_percent$State)

#Cross table frequency for sample data
table(stratified_sample_10_percent$State,stratified_sample_10_percent$CardType)


## ----3_42_Stratified_Random_Sampling_Mean_compare------------------------

# Average outstanding balance by stratum variables
summary_population<-summarise(group_by(data,CardType,State),OutstandingBalance_Stratum=mean(OutsBal))


#We can see below the want to make sure that our sampling retain the same proportion of the cardtype in the sample
summary_sample<-summarise(group_by(stratified_sample_10_percent,CardType,State),OutstandingBalance_Sample=mean(OutsBal))

#Mean Comparision by stratum
summary_mean_compare<-merge(summary_population,summary_sample, by=c("CardType","State"))

## ----3_42_Stratified_Random_Sampling_KStest------------------------------

ks.test(data$OutsBal,stratified_sample_10_percent$OutsBal,alternative="two.sided")

## ----3_42_Stratified_Random_Sampling_Histogram---------------------------
par(mfrow = c(1,2))
hist(data$OutsBal, breaks=50, col="red", xlab="Outstanding Balance", 
  	main="Population")

hist(stratified_sample_10_percent$OutsBal, breaks=50, col="green", xlab="Outstanding Balance", 
  	main="Stratified Sample")

par(mfrow = c(1,1))


## ----3_42_Cluster_Sampling_Create_Clusters-------------------------------
# Before i explain cluster sampling, lets try to subset the data such that we have clear samples to explain the importance of cluster sampling
#Subset the data by outstanding balance into 5 subgroups
Data_Subset_Clusters_1 <- subset(data, IntTransc >2 &  IntTransc < 5)
Data_Subset_Clusters_2 <- subset(data, IntTransc > 10 & IntTransc < 13)
Data_Subset_Clusters_3 <- subset(data, IntTransc > 18 & IntTransc < 21)
Data_Subset_Clusters_4 <- subset(data, IntTransc > 26 & IntTransc < 29)
Data_Subset_Clusters_5 <- subset(data, IntTransc > 34)

Data_Subset_Clusters<-rbind(Data_Subset_Clusters_1,Data_Subset_Clusters_2,Data_Subset_Clusters_3,Data_Subset_Clusters_4,Data_Subset_Clusters_5)

str(Data_Subset_Clusters)

## ----3_42_Cluster_Sampling_Identify_Clusters_1---------------------------
# Now we will treat the Data_Subset_Clusters as our population 
library(stats)

kmeans_clusters <- kmeans(Data_Subset_Clusters$IntTransc, 5, nstart = 25)

cat("The cluster center are ",kmeans_clusters$centers)

## ----3_42_Cluster_Sampling_Identify_Clusters_2---------------------------
set.seed(937)
# For plotting lets use only 100000 records randomly chosen from total data.
library(splitstackshape)
PlotSample<-Data_Subset_Clusters[sample(nrow(Data_Subset_Clusters),size=100000, replace = TRUE, prob = NULL),]

plot(PlotSample$IntTransc, col = kmeans_clusters$cluster)
points(kmeans_clusters$centers, col = 1:5, pch = 8)

cluster_sample_combined<-cbind(Data_Subset_Clusters,kmeans_clusters$cluster)

setnames(cluster_sample_combined,"V2","ClusterIdentifier")

## ----3_42_Cluster_Sampling_Identify_Clusters_3---------------------------
print("Summary of no. of records per clusters")
table(cluster_sample_combined$ClusterIdentifier)

## ----3_42_Cluster_Sampling_at_Cluster_level_1----------------------------
set.seed(937)
library(splitstackshape)
cluster_sample_10_percent<-stratified(cluster_sample_combined,group=c("ClusterIdentifier"),size=0.1,replace=FALSE)

## ----3_42_Cluster_Sampling_at_Cluster_level_2----------------------------
print("Ploting the clusters for random sample from clusters")
plot(cluster_sample_10_percent$IntTransc, col = kmeans_clusters$cluster)
points(kmeans_clusters$centers, col = 1:5, pch = 8)

## ----3_42_Cluster_Sampling_at_Cluster_level_3----------------------------
print("Summary of no. of records per clusters")
table(cluster_sample_10_percent$ClusterIdentifier)

## ----3_39_Cluster_Sampling_mean_compare----------------------------------
population_summary <- summarise(group_by(data,CardType),OutstandingBalance_Population=mean(OutsBal))
cluster_summary<-summarise(group_by(cluster_sample_10_percent,CardType),OutstandingBalance_Cluster=mean(OutsBal))

summary_mean_compare<-merge(population_summary,cluster_summary, by="CardType")

print(summary_mean_compare)

## ----3_39_Cluster_Sampling_Histogram-------------------------------------
par(mfrow = c(1,2))
hist(data$OutsBal, breaks=50, col="red", xlab="Outstanding Balance", 
  	main="Cluster Population")

hist(cluster_sample_10_percent$OutsBal, breaks=50, col="green", xlab="Outstanding Balance", 
  	main="Cluster Random Sample")

par(mfrow = c(1,1))

## ----3_39_BootStrap_1----------------------------------------------------
set.seed(937)
library(boot)
# Now we need the function we would like to estimate

#First fit a linear model and know the true estimates, from population data
summary(lm(OutsBal ~ 0 + DomesTransc, data = data))

## ----3_39_BootStrap_2----------------------------------------------------
set.seed(937)
#Assume that we are only having 10000 data points and have to do the hypothesis test around significance of coeffcient domestic transactions. As the data set is small we will use the bootstarintg to create the distribution of coeffcient and then create a confidence interval to test the hypothesis.

sample_10000 <- data[sample(nrow(data),size=10000, replace = FALSE, prob = NULL),]


## ----3_39_BootStrap_3----------------------------------------------------
# Function to return Coefficient of DomesTransc
Coeff = function(data,b,formula){  
# b is the random indexes for the bootstrap sample
	d = data[b,] 
	return(lm(OutsBal ~ 0 + DomesTransc, data = d)$coefficients[1])  
# thats for the beta coefficient
}



## ----3_39_BootStrap_4----------------------------------------------------
set.seed(937)
# R is how many bootstrap samples 
bootbet = boot(data=sample_10000, statistic=Coeff, R=50) 
names(bootbet)

## ----3_39_BootStrap_5----------------------------------------------------
plot(bootbet)
hist(bootbet$t, breaks = 5)

## ----3_39_BootStrap_6----------------------------------------------------
mean(bootbet$t)
var(bootbet$t)

## ----3_39_BootStrap_7----------------------------------------------------
x <-  bootbet$t
h<-hist(x, breaks=5, col="red", xlab="Boot Strap Estimates", 
  	main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(bootbet$t),sd=sqrt(var(bootbet$t))) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

## ----3_39_BootStrap_8----------------------------------------------------
t.test(bootbet$t, mu=77.13)


## ----MonteCarlo_1--------------------------------------------------------
curve(dbeta(x, 3,10),0,1)

## ----MonteCarlo_2--------------------------------------------------------
set.seed(937)
sampled <- data.frame(proposal = runif(5000,0,1))

sampled$targetDensity <- dbeta(sampled$proposal, 3,10)

## ----MonteCarlo_3--------------------------------------------------------
maxDens = max(sampled$targetDensity, na.rm = T)
sampled$accepted = ifelse(runif(5000,0,1) < sampled$targetDensity / maxDens, TRUE, FALSE)

## ----MonteCarlo_4--------------------------------------------------------

hist(sampled$proposal[sampled$accepted], freq = F, col = "grey", breaks = 100, main = "Histogram of Accepted Sample" ,xlab = "x-value")
curve(dbeta(x, 3,10),0,1, add =T, col = "red")

## ----computational_time_1------------------------------------------------
# estimate paramters
library(MASS)

start.time <- Sys.time()

population_lm<-lm(OutsBal ~ DomesTransc + Gender, data = data)

end.time <- Sys.time()
time.taken_1 <- end.time - start.time

cat("Time taken to fit linear model on Population",time.taken_1 )

## ----computational_time_2------------------------------------------------
start.time <- Sys.time()

sample_lm<-lm(OutsBal ~ DomesTransc + Gender, data = sample_10000)

end.time <- Sys.time()
time.taken_2 <- end.time - start.time

cat("Time taken to fit linear model on Sample ",time.taken_2 )


