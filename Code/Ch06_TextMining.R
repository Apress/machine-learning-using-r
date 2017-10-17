
## ------------------------------------------------------------------------
library(data.table)

fine_food_data <- read.csv("Dataset/Food_Reviews.csv",stringsAsFactors = FALSE)
fine_food_data$Score <- as.factor(fine_food_data$Score)

str(fine_food_data[-10])

# Last column - Custmer review in free text

head(fine_food_data[,10],2)


## ---- message=FALSE,warning=FALSE----------------------------------------

library(caTools)

# Randomly split data and use only 10% of the dataset
set.seed(90)
split = sample.split(fine_food_data$Score, SplitRatio = 0.10)

fine_food_data = subset(fine_food_data, split == TRUE)

select_col <- c("Id","HelpfulnessNumerator","HelpfulnessDenominator","Score","Summary","Text")

fine_food_data_selected <- fine_food_data[,select_col]


## ------------------------------------------------------------------------
fine_food_data_selected[2,6]

## ---- warning=FALSE,message=FALSE----------------------------------------
library(LSAfun)
genericSummary(fine_food_data_selected[2,6],k=1)

## ---- warning=FALSE,message=FALSE----------------------------------------
library(LSAfun)
genericSummary(fine_food_data_selected[2,6],k=2)

## ------------------------------------------------------------------------
fine_food_data_selected[2,5]

## ----warning=FALSE,message=FALSE,eval=FALSE------------------------------
## 
## library(proxy)
## library(tm)
## 
## fine_food_data_corpus <- VCorpus(VectorSource(fine_food_data_selected$Text))
## 
## fine_food_data_text_tdm <- TermDocumentMatrix(fine_food_data_corpus, control = list(
##   tolower = TRUE,
##   removeNumbers = TRUE,
##   stopwords = TRUE,
##   removePunctuation = TRUE,
##   stemming = TRUE
## ))
## 
## matrix_c <- as.matrix(fine_food_data_text_tdm)
## 
## 
## wc_freq <- sort(rowSums(matrix_c))
## wc_tmdata <- data.frame(words=names(wc_freq), wc_freq)
## 
## wc_tmdata <- na.omit(wc_tmdata)
## 
## tail(wc_tmdata$words,100)
## 
## 
## 
## review_diss <- proxy::dist(matrix_c, method = "cosine")
## 
## docsdissim2 <- as.matrix(docsdissim)
## rownames(docsdissim2) <- titles
## colnames(docsdissim2) <- titles
## docsdissim2
## h <- hclust(docsdissim, method = "ward")
## plot(h, labels = titles, sub = "")
## 

## ------------------------------------------------------------------------
library(tm)
fine_food_data_corpus <- VCorpus(VectorSource(fine_food_data_selected$Text))

#Standardize the text - Pre-Processing

fine_food_data_text_dtm <- DocumentTermMatrix(fine_food_data_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE 
))

# save frequently-appearing terms( more than 500 times) to a character vector

fine_food_data_text_freq <- findFreqTerms(fine_food_data_text_dtm, 500)

# create DTMs with only the frequent terms
fine_food_data_text_dtm <- fine_food_data_text_dtm[ , fine_food_data_text_freq]

tm::inspect(fine_food_data_text_dtm[1:5,1:10])

#Create a tf-idf matrix
fine_food_data_tfidf <- weightTfIdf(fine_food_data_text_dtm, normalize = FALSE)

tm::inspect(fine_food_data_tfidf[1:5,1:10])


## ----message=FALSE,warning=FALSE-----------------------------------------

library(NLP)
library(tm)


fine_food_data_corpus <- Corpus(VectorSource(fine_food_data_selected$Text[1:3]))
fine_food_data_cleaned <- tm_map(fine_food_data_corpus, PlainTextDocument)

#tolwer
fine_food_data_cleaned <- tm_map(fine_food_data_cleaned, tolower) 
fine_food_data_cleaned[[1]]

fine_food_data_cleaned <-tm_map(fine_food_data_cleaned, removeWords, stopwords("english")) 
fine_food_data_cleaned[[1]]

fine_food_data_cleaned <- tm_map(fine_food_data_cleaned, removePunctuation) 
fine_food_data_cleaned[[1]]

fine_food_data_cleaned <- tm_map(fine_food_data_cleaned, removeNumbers) 
fine_food_data_cleaned[[1]]

fine_food_data_cleaned <-tm_map(fine_food_data_cleaned, stripWhitespace) 
fine_food_data_cleaned[[1]]

## ------------------------------------------------------------------------
library(openNLP)
library(NLP)

fine_food_data_string <- NLP::as.String(fine_food_data_cleaned[[1]])

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
fine_food_data_string_an <- annotate(fine_food_data_string, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
fine_food_data_string_an2 <- annotate(fine_food_data_string, pos_tag_annotator, fine_food_data_string_an)

## Variant with POS tag probabilities as (additional) features.
head(annotate(fine_food_data_string, Maxent_POS_Tag_Annotator(probs = TRUE), fine_food_data_string_an2))

## Determine the distribution of POS tags for word tokens.
fine_food_data_string_an2w <- subset(fine_food_data_string_an2, type == "word")
tags <- sapply(fine_food_data_string_an2w$features, `[[`, "POS")
table(tags)

plot(table(tags), type = "h", xlab="Part-Of_Speech", ylab = "Frequency")

## Extract token/POS pairs (all of them)
head(sprintf("%s/%s", fine_food_data_string[fine_food_data_string_an2w], tags),15)


## ---- warning=FALSE, message=FALSE---------------------------------------

library(SnowballC)
library(wordcloud)
library(tm)
library(slam)

fine_food_data_corpus <- VCorpus(VectorSource(fine_food_data_selected$Text))

fine_food_data_text_tdm <- TermDocumentMatrix(fine_food_data_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE 
))

#reducing sparsity
wc_tdm <- rollup(fine_food_data_text_tdm, 2, na.rm=TRUE, FUN = sum)
matrix_c <- as.matrix(wc_tdm)
wc_freq <- sort(rowSums(matrix_c))
wc_tmdata <- data.frame(words=names(wc_freq), wc_freq)

wc_tmdata <- na.omit(wc_tmdata)

wordcloud (tail(wc_tmdata$words,100), tail(wc_tmdata$wc_freq,100), random.order=FALSE, colors=brewer.pal(8, "Dark2"))


## ----ch06_setupTwitter_api-----------------------------------------------
library("stringr")
library("dplyr")
#install.packages("twitteR")
library("twitteR")
#getTwitterOAuth(consumer_key, consumer_secret)
consumerKey <- "YOUR KEY"
consumerSecret <- " YOUR KEY"

#Below two tokens need to be used when you want to pull tweets from your own account
accessToken <- "YOUR KEY"
accessTokenSecret <- "YOUR KEY"

setup_twitter_oauth(consumerKey, consumerSecret,accessToken,accessTokenSecret)

kIgnoreTweet <- "update:|nobot:"

GetTweets <- function(handle, n = 1000) {

    timeline <- userTimeline(handle, n = n)
    tweets <- sapply(timeline, function(x) {
        c(x$getText(), x$getCreated())
    })
    tweets <- data.frame(t(tweets))
    names(tweets) <- c("text.orig", "created.orig")

    tweets$text <- tolower(tweets$text.orig)
    tweets$created <- as.POSIXct(as.numeric(as.vector(tweets$created.orig)), origin="1970-01-01")
    
    arrange(tweets, created)
}

handle <- "@TimesNow"
tweets <- GetTweets(handle, 100)

#Store the tweets as used in the book for future reproducibility
#write.csv(tweets,"Dataset/Twitter Feed From TimesNow.csv",row.names = FALSE)
tweets <- read.csv("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/Twitter Feed From TimesNow.csv")
tweets[1:5,]

## ----ch06_microsoft_services---------------------------------------------
#install.packages("mscstexta4r")
library(mscstexta4r)

#Put the authentication APi keys you got from microsoft

Sys.setenv(MSCS_TEXTANALYTICS_URL = "https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/")
Sys.setenv(MSCS_TEXTANALYTICS_KEY = "YOUR KEY")

#Intiliatize the service
textaInit()


## ----ch06_loadADocument--------------------------------------------------
# Load Packages
require(tm)
require(NLP)
require(openNLP)

#Read the Forbes article into R environment
y <- paste(scan("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/india_after_independence.txt", what="character", sep=" "),collapse=" ")

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)

  # Convert text to class String from package NLP
  text <- as.String(text)

  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)

  # Extract sentences
  sentences <- text[sentence.boundaries]

  # return sentences
  return(sentences)
}

# Convert the text into sentances
article_text = convert_text_to_sentences(y, lang = "en")


## ----ch06_text_sentiment-------------------------------------------------

document_lang <- rep("en", length(tweets$text))

tryCatch({

  # Perform sentiment analysis
output_1 <-  textaSentiment(
    documents = tweets$text,    # Input sentences or documents
    languages = document_lang
    # "en"(English, default)|"es"(Spanish)|"fr"(French)|"pt"(Portuguese)
)

}, error = function(err) {

  # Print error
  geterrmessage()

})

merged <- output_1$results

#Order the tweets with sentment score
ordered_tweets <- merged[order(merged$score),]

#Top 5 negative tweets
ordered_tweets[1:5,]

#Top 5 Positive
ordered_tweets[95:100,]

## ----ch06_text_topic_detect----------------------------------------------

handle <- "@CNN"
topic_text <- GetTweets(handle, 150)
#write.csv(topic_text,"Dataset/Twitter Feed from CNN.csv",row.names=FALSE)
topic_text <- read.csv("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/Twitter Feed from CNN.csv")

tryCatch({

# Detect top topics in group of documents
output_2 <- textaDetectTopics(
  topic_text$text,                  # At least 100 documents (English only)
  stopWords = NULL,           # Stop word list (optional)
  topicsToExclude = NULL,     # Topics to exclude (optional)
  minDocumentsPerWord = NULL, # Threshold to exclude rare topics (optional)
  maxDocumentsPerWord = NULL, # Threshold to exclude ubiquitous topics (optional)
  resultsPollInterval = 30L,  # Poll interval (in s, default: 30s, use 0L for async)
  resultsTimeout = 1200L,     # Give up timeout (in s, default: 1200s = 20mn)
  verbose = FALSE             # If set to TRUE, print every poll status to stdout
)

}, error = function(err) {

  # Print error
  geterrmessage()

})
output_2

## ----ch06_lang_detect----------------------------------------------------
#Below i am creating 5 messgaes in 5 different langauge using google translater
#1-ARABIC, 2-POTUGESE, 3- ENGLISH , 4- CHINESE AND 5 - HINDI

lang_detect<- c("أنا عالم البيانات","Eu sou um cientista de dados","I am a data scientist","我是一个科学家的数据","
मैं एक डेटा वैज्ञानिक हूँ")

 
tryCatch({

# Detect top topics in group of documents
# Detect languages used in documents
output_3 <- textaDetectLanguages(
  lang_detect,                      # Input sentences or documents
  numberOfLanguagesToDetect = 1L    # Default: 1L
)

}, error = function(err) {

  # Print error
  geterrmessage()

})
output_3

## ----ch06_text_summarization---------------------------------------------

article_lang <- rep("en", length(article_text))

tryCatch({

  # Get key talking points in documents
 output_4 <- textaKeyPhrases(
    documents = article_text,    # Input sentences or documents
    languages = article_lang
    # "en"(English, default)|"de"(German)|"es"(Spanish)|"fr"(French)|"ja"(Japanese)
  )

}, error = function(err) {

  # Print error
  geterrmessage()

})

#Print the top 5 summary
output_4$results[1:5,1]


