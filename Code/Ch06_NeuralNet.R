

## ----ch06_nnet_01--------------------------------------------------------

#Load the data and prepare a dataset for logistic regression
Data_Purchase_Prediction <- read.csv("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/Purchase Prediction Dataset.csv",header=TRUE);

Data_Purchase_Prediction$choice <- ifelse(Data_Purchase_Prediction$ProductChoice == 1,1,
                                          ifelse(Data_Purchase_Prediction$ProductChoice == 3,0,999));

Data_Neural_Net <- Data_Purchase_Prediction[Data_Purchase_Prediction$choice %in% c("0","1"),]

#Remove Missing Values
Data_Neural_Net <- na.omit(Data_Neural_Net)
rownames(Data_Neural_Net) <- NULL


## ----ch06_nnet_02--------------------------------------------------------
#Transforming the continuous variables
cont <- Data_Neural_Net[,c("PurchaseTenure","CustomerAge","MembershipPoints","IncomeClass")]

maxs <- apply(cont, 2, max) 
mins <- apply(cont, 2, min)

scaled_cont <- as.data.frame(scale(cont, center = mins, scale = maxs - mins))

#The dependent variable
dep <- factor(Data_Neural_Net$choice)

Data_Neural_Net$ModeOfPayment <- factor(Data_Neural_Net$ModeOfPayment);

flags_ModeOfPayment = data.frame(Reduce(cbind, 
     lapply(levels(Data_Neural_Net$ModeOfPayment), function(x){(Data_Neural_Net$ModeOfPayment == x)*1})
))

names(flags_ModeOfPayment) = levels(Data_Neural_Net$ModeOfPayment)

Data_Neural_Net$CustomerPropensity <- factor(Data_Neural_Net$CustomerPropensity);

flags_CustomerPropensity = data.frame(Reduce(cbind, 
     lapply(levels(Data_Neural_Net$CustomerPropensity), function(x){(Data_Neural_Net$CustomerPropensity == x)*1})
))
names(flags_CustomerPropensity) = levels(Data_Neural_Net$CustomerPropensity)

cate <- cbind(flags_ModeOfPayment,flags_CustomerPropensity)

#Combine all data into single modeling data
Dataset <- cbind(dep,scaled_cont,cate);

#Divide the data into train and test
set.seed(917);
index <- sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
train <- Dataset[index,]
test <- Dataset[-index,]

## ----ch06_nnet_03--------------------------------------------------------
library(nnet)
i <- names(train)
form <- as.formula(paste("dep ~", paste(i[!i %in% "dep"], collapse = " + ")))
nn <- nnet.formula(form,size=10,data=train)

predict_class <- predict(nn, newdata=test, type="class")

#Classiifcation table
table(test$dep,predict_class)

#Clasisifcation rate
sum(diag(table(test$dep,predict_class))/nrow(test))

## ----ch06_nnet_04--------------------------------------------------------
library(NeuralNetTools)
# Plot the neural network
plotnet(nn)

#get the neural weights
neuralweights(nn)

# Plot the imporatance
olden(nn)

#variable importance by garson algorith
garson(nn)


## ----ch06_data_mat-------------------------------------------------------
#Pre-transfromation
head(Data_Purchase_Prediction[,c("choice","PurchaseTenure","CustomerAge","MembershipPoints","IncomeClass","ModeOfPayment","CustomerPropensity")])

#Post-transoformation
head(train)

## ----ch06_DeepNeuralNet_Darch,error=FALSE--------------------------------
#We will us the same data as of previous example in neural network
devtools::install_github("maddin79/darch")
library(darch)
library(mlbench)
library(RANN)

#Print the model formula
form

#Apply the model using deep neural net with 
# deep_net <- darch(form, train,
#               preProc.params = list("method" = c("knnImpute")),
#               layers = c(0,10,30,10,0),
#               darch.batchSize = 1,
#               darch.returnBestModel.validationErrorFactor = 1,
#               darch.fineTuneFunction = "rpropagation",
#               darch.unitFunction = c("tanhUnit", "tanhUnit","tanhUnit","softmaxUnit"),
#               darch.numEpochs = 15,
#               bootstrap = T,
#               bootstrap.num = 500)

deep_net <- darch(form,train,
preProc.params = list(method = c("center", "scale")),
layers = c(0,10,30,10,0),
darch.unitFunction = c("sigmoidUnit", "tanhUnit","tanhUnit","softmaxUnit"),
darch.fineTuneFunction = "minimizeClassifier",
darch.numEpochs = 15,
cg.length = 3, cg.switchLayers = 5)


#Plot the deep net
library(NeuralNetTools)
plot(deep_net,"net")

result <- darchTest(deep_net, newdata = test)
result


## ----install,eval=FALSE--------------------------------------------------
## install.packages("drat", repos="https://cran.rstudio.com")
## drat:::addRepo("dmlc")
## install.packages("mxnet")
## 
## #Please refer https://github.com/dahtah/imager
## install.packages("devtools")
## devtools::install_github("dahtah/imager")

## ----ch06_deep_learning_01-----------------------------------------------
library(mxnet)

#install imager for loading images

library(imager)

#load the pre-trained model
model <- mx.model.load("Inception/Inception_BN", iteration=39)

#We also need to load in the mean image, which is used for preprocessing using mx.nd.load.

mean.img = as.array(mx.nd.load("Inception/mean_224.nd")[["mean_img"]])

#Load and plot the image: (Defualt parot image)

#im <- load.image(system.file("extdata/parrots.png", package="imager"))
im <- load.image("Pictures/russia-volcano.jpg")
plot(im)

## ----ch06_deep_learning_02-----------------------------------------------
preproc.image <- function(im, mean.image) {
  # crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  xx <- floor((shape[1] - short.edge) / 2)
  yy <- floor((shape[2] - short.edge) / 2)
  croped <- crop.borders(im, xx, yy)
  # resize to 224 x 224, needed by input of the model.
  resized <- resize(croped, 224, 224)
  # convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # substract the mean
  normed <- arr - mean.img
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  return(normed)
}

#Now pass our image to pre-process
normed <- preproc.image(im, mean.img)
plot(normed)

## ----ch06_deep_learning_03,error= FALSE----------------------------------
#prob <- predict(model, X=normed)

#We can extract the top-5 class index.

#max.idx <- order(prob[,1], decreasing = TRUE)[1:5]
max.idx <- c("981", "980", "971", "673", "985")
max.idx

synsets <- readLines("Inception/synset.txt")

#And let us print the corresponding lines:

print(paste0("Predicted Top-classes: ", synsets[as.numeric(max.idx)]))

