## ---- warning=FALSE, message=FALSE---------------------------------------

library(doParallel)

# Find out how many cores are available (if you don't already know)
c = detectCores()
c

# Find out how many cores are currently being used
getDoParWorkers()

# Create cluster with c-2 cores
cl <- makeCluster(c-2)

# Register cluster
registerDoParallel(cl)

# Find out how many cores are being used
getDoParWorkers()

## ---- warning=FALSE, message=FALSE---------------------------------------


## Problem : Identifying Risky Bank Loans

credit <- read.csv("credit.csv")
str(credit)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

## ---- warning=FALSE, message=FALSE---------------------------------------

## Training a model on the data

library(randomForest)

#Sequential Execution
system.time(rf_credit_model <- randomForest(credit_train[-17], 
                                            credit_train$default, 
                                            ntree = 1000))

## ---- warning=FALSE, message=FALSE---------------------------------------

#Parallel Execution
system.time(
  rf_credit_model_parallel <- foreach(nt = rep(250,4), 
                                    .combine = combine ,
                                    .packages = 'randomForest') 
                            %dopar%     
                            randomForest(
                              credit_train[-17], 
                              credit_train$default, 
                              ntree = nt))

## ---- warning=FALSE, message=FALSE---------------------------------------

#Shutting down cluster - when you're done, be sure to close #the parallel backend using
stopCluster(cl)


## ---- eval=FALSE---------------------------------------------------------
## 
Sys.setenv(HADOOP_CMD="/usr/lib/hadoop-2.2.0/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/usr/lib/hadoop-2.2.0/share/hadoop/tools/lib/hadoop-streaming-2.2.0.jar")

## ---- eval=FALSE---------------------------------------------------------
## 
library(rmr2)
library(rhdfs)
## 
## # Hadoop File Operations
## 
## #initialize File HDFS
 hdfs.init()

## ---- eval=FALSE---------------------------------------------------------
## 
## #Put File into HDFS
 hdfs.put("/home/sample.txt","/hadoop_practice")
## ## [1] TRUE
## 

## ---- eval=FALSE---------------------------------------------------------
## 
## # Reads a bunch of lines at a time
## #Map Phase
map <- function(k,lines) {
words.list <- strsplit(lines, '\\s+')
 words <- unlist(words.list)
 return( keyval(words, 1) )
## }
## 
## #Reduce Phase
reduce <- function(word, counts) {
  keyval(word, sum(counts))
}
## 
## #MapReduce Function
wordcount <- function (input, output=NULL) {
  mapreduce(input=input, output=output, input.format="text", map=map, reduce=reduce)
 }

## ---- eval=FALSE---------------------------------------------------------
## 
## 
## ## read text files from folder input on HDFS
## ## save result in folder output on HDFS
## ## Submit job
## 
 basedir <- '/hadoop_practice'
 infile  <- file.path(basedir, 'sample.txt')
 outfile <- file.path(basedir, 'output')
 ret <- wordcount(infile, outfile)

## ---- eval=FALSE---------------------------------------------------------
## 
## 
## ## Fetch results from HDFS
 result <- from.dfs(outfile)
 results.df <- as.data.frame(result, stringsAsFactors=F)
 colnames(results.df) <- c('word', 'count')
tail(results.df,100)
## 
## ##          word count
## ## 1           R     1
## ## 2          Hi     1
## ## 3          to     1
## ## 4         All     1
## ## 5        with     1
## ## 6       class     1
## ## 7      hadoop     3
## ## 8     Welcome     1
## ## 9 integrating     1
## 
## 
head(results.df[order(results.df$count, decreasing = TRUE),])
## 
## ##     word count
## ## 7 hadoop     3
## ## 1      R     1
## ## 2     Hi     1
## ## 3     to     1
## ## 4    All     1
## ## 5   with     1
## 

## ---- warning=FALSE,message=FALSE----------------------------------------

#Set environment variable
Sys.setenv(SPARK_HOME='C:/Spark/spark-2.0.0-bin-hadoop2.7',HADOOP_HOME='C:/Hadoop-2.3.0')
.libPaths(c(file.path(Sys.getenv('SPARK_HOME'), 'R', 'lib'),.libPaths()))
Sys.setenv('SPARKR_SUBMIT_ARGS'='"sparkr-shell"')

## ---- warning=FALSE,message=FALSE----------------------------------------


library(SparkR)
library(rJava)

#The entry point into SparkR is the SparkSession which connects your R program to a Spark cluster
sparkR.session(enableHiveSupport = FALSE, appName = "SparkR-ML",master = "local[*]", sparkConfig = list(spark.driver.memory = "1g",spark.sql.warehouse.dir="C:/Hadoop-2.3.0"))


## ---- warning=FALSE,message=FALSE----------------------------------------
library(data.table)

#Read the housing data
Data_House_Price <- fread("/Users/karthik/Dropbox/Book Writing - Drafts/Chapter Drafts/Chapter 7 - Machine Learning Model Evaluation/tosend/House Sale Price Dataset.csv",header=T, verbose = FALSE, showProgress = FALSE)

str(Data_House_Price)


#Pulling out relevant columns and assigning required fields in the dataset
Data_House_Price <- Data_House_Price[,.(HOUSE_ID,HousePrice,StoreArea,StreetHouseFront,BasementArea,LawnArea,Rating,SaleType)]

#Omit any missing value
Data_House_Price <- na.omit(Data_House_Price)

Data_House_Price$HOUSE_ID <- as.character(Data_House_Price$HOUSE_ID)


## ---- warning=FALSE,message=FALSE----------------------------------------
#Spark Data Frame - Train
gaussianDF_train <- createDataFrame(Data_House_Price[1:floor(nrow(Data_House_Price)*(2/3)),])

#Spark Data Frame - Test
gaussianDF_test <- createDataFrame(Data_House_Price[floor(nrow(Data_House_Price)*(2/3) + 1):nrow(Data_House_Price),])

class(gaussianDF_train)

class(gaussianDF_test)


## ---- warning=FALSE,message=FALSE----------------------------------------

# Fit a generalized linear model of family "gaussian" with spark.glm
gaussianGLM <- spark.glm(gaussianDF_train, HousePrice ~ StoreArea + StreetHouseFront + BasementArea + LawnArea +  Rating  +  SaleType, family = "gaussian")

# Model summary
summary(gaussianGLM)


## ---- warning=FALSE,message=FALSE----------------------------------------

#Prediction on the gaussianModel
gaussianPredictions <- predict(gaussianGLM, gaussianDF_test)
names(gaussianPredictions) <- c('HOUSE_ID','HousePrice','StoreArea','StreetHouseFront','BasementArea','LawnArea','Rating','SaleType','ActualPrice','PredictedPrice')
gaussianPredictions$PredictedPrice <- round(gaussianPredictions$PredictedPrice,2.0)
showDF(gaussianPredictions[,9:10])

## ---- warning=FALSE,message=FALSE----------------------------------------

sparkR.stop()


## ----ch09_h2o_1,eval=FALSE-----------------------------------------------
## # The following two commands remove any previously installed H2O packages for R.
## if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
## if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
## 
## # Next, we download, install and initialize the H2O package for R.
## install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/rel-kahan/5/R", getOption("repos"))))
## 
## #Alternatively you can install the package h2o from CRAN as below
## install.packages("h2o")

## ----ch09_h2o_2----------------------------------------------------------
# Load the h2o library in R
library(h2o);

#Initiate a cluster in your machine
localH2O = h2o.init()

## ----ch09_h2o_3----------------------------------------------------------
# Run Deep learning demo
demo(h2o.deeplearning)

prostate.hex

# Other demos can be called as below
#demo(h2o.glm)
#demo(h2o.gbm)

