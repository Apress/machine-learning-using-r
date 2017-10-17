## ----opts, echo = FALSE--------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96);
knitr::opts_chunk$set(
  fig.path = "~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Images/006/"
)

## ---- warning=FALSE, message=FALSE---------------------------------------

library(arules)
MarketBasket <- read.transactions("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/MarketBasketProcessed.csv", sep = ",")
summary(MarketBasket)

#Transactions - First two
inspect(MarketBasket[1:2])

# Top 20 frequently bought product
itemFrequencyPlot(MarketBasket, topN = 20)

# Sparsity in the data - More white space means, more sparsity
image(sample(MarketBasket, 100))


## ------------------------------------------------------------------------
library(arules)


apriori(MarketBasket)
groceryrules <- apriori(MarketBasket, parameter = list(support = 0.2, confidence = 0.8, minlen = 2))

itemsets <- eclat(MarketBasket,
                  parameter = list(supp = 0.1, maxlen = 15))


## ------------------------------------------------------------------------

summary(groceryrules)

# look at the first three rules
inspect(groceryrules[1:3])

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "lift")[1:5])

# store as  data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)


## ------------------------------------------------------------------------
library(arules)

eclat(MarketBasket)
groceryrules <- eclat(MarketBasket, parameter = list(support = 0.2, minlen = 2))

itemsets <- eclat(MarketBasket, parameter = list(supp = 0.1, maxlen = 15))


## ------------------------------------------------------------------------

summary(groceryrules)

# look at the first three rules
inspect(groceryrules[1:3])

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "support")[1:5])

# store as  data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)


## ------------------------------------------------------------------------


library(data.table)

fine_food_data <- read.csv("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/Food_Reviews.csv",stringsAsFactors = FALSE)
fine_food_data$Score <- as.factor(fine_food_data$Score)

str(fine_food_data[-10])



## ---- message=FALSE,warning=FALSE----------------------------------------

library(caTools)

# Randomly split data and use only 10% of the dataset
set.seed(90)
split = sample.split(fine_food_data$Score, SplitRatio = 0.05)

fine_food_data = subset(fine_food_data, split == TRUE)

select_col <- c("UserId","ProductId","Score")

fine_food_data_selected <- fine_food_data[,select_col]
rownames(fine_food_data_selected) <- NULL
fine_food_data_selected$Score = as.numeric(fine_food_data_selected$Score)

#Remove Duplicates

fine_food_data_selected <- unique(fine_food_data_selected)


## ---- warning=FALSE,message=FALSE----------------------------------------

library(recommenderlab)

#RatingsMatrix

RatingMat <- dcast(fine_food_data_selected,UserId ~ ProductId, value.var = "Score")
User=RatingMat[,1]
Product= colnames(RatingMat)[2:ncol(RatingMat)]
RatingMat[,1] <- NULL
RatingMat <- as.matrix(RatingMat)
dimnames(RatingMat) = list(user = User , product = Product)

realM <- as(RatingMat, "realRatingMatrix")

## ------------------------------------------------------------------------

#distribution of ratings
hist(getRatings(realM), breaks=15, main = "Distribution of Ratings", xlab = "Ratings", col = "grey")

#Sparse Matrix Representation
head(as(realM, "data.frame"))

#The realRatingMatrix can be coerced back into a matrix which is identical to the original matrix
identical(as(realM, "matrix"),RatingMat)

#Sparcity in Rating Matrix
image(realM, main = "Raw Ratings")

## ------------------------------------------------------------------------

#UBCF Model
r_UBCF <- Recommender(realM[1:1700], method = "UBCF")
r_UBCF

#List of objects in the model output
names(getModel(r_UBCF))

#Recommend product for the rest of 29 left out observations
recom_UBCF <- predict(r_UBCF, realM[1700:1729], n=5)
recom_UBCF

#Display the recommendation
reco <- as(recom_UBCF, "list")
reco[lapply(reco,length)>0]


## ------------------------------------------------------------------------

set.seed(2016)
scheme <- evaluationScheme(realM[1:1700], method="split", train = .9,
k=1, given=1, goodRating=3)

scheme

algorithms <- list(
"random items" = list(name="RANDOM", param=NULL),
"popular items" = list(name="POPULAR", param=NULL),
"user-based CF" = list(name="UBCF", param=list(nn=50)),
"item-based CF" = list(name="IBCF", param=list(k=50))
)

results <- evaluate(scheme, algorithms, type = "topNList",
n=c(1, 3, 5, 10, 15, 20))

plot(results, annotate=c(1,3), legend="bottomright")


