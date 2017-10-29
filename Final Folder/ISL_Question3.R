

# Importing the libraries

library(caTools)
library(ggplot2)
library(MASS)
library(tm)
library(wordcloud)
library(caret)
library(e1071)
library(MLmetrics)
library(stringr)

# Importing the dataset

input_data=input_data<-read.csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/SMSSpamCollection.csv",
                                header = FALSE,
                                stringsAsFactors = FALSE)

str(input_data)

#Changing the name of the features/ columns

colnames(input_data) <- c("type", "text")

#Converting the text to utf-8 format

input_data$text <- iconv(input_data$text, to = "utf-8")

#Type as factor

input_data$type <- factor(input_data$type)

summary(input_data)

table(input_data$type)
prop.table(table(input_data$type)) * 100

set.seed(123)

# Create a training set containing 80% of the data (with stratified sampling)

trainIndex <- createDataPartition(input_data$type, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- input_data[trainIndex,]
testData <- input_data[-trainIndex,]

# proportion in train dataset

prop.table(table(trainData$type)) * 100

##      ham     spam 
## 86.58591 13.41409

# proportion in test dataset
prop.table(table(testData$type)) * 100

# Ham messages

trainData_ham <- trainData[trainData$type == "ham",]
head(trainData_ham$text)
tail(trainData_ham$text)

# spam messages

trainData_spam <- trainData[trainData$type == "spam",]
head(trainData_spam$text)

# Removing the trainData_ham and trainData_spam

trainData_spam <- NULL
trainData_ham <- NULL

# create the corpus

corpus <- Corpus(VectorSource(trainData$text))

# basic info about the corpus

print(corpus)

#1. normalize to lowercase (not a standard tm transformation)

corpus <- tm_map(corpus, content_transformer(tolower))

#2. remove numbers

corpus <- tm_map(corpus, removeNumbers)

#3. remove stopwords e.g. to, and, but, or (using predefined set of word in tm package)

corpus <- tm_map(corpus, removeWords, stopwords())

#4. remove punctuation

corpus <- tm_map(corpus, removePunctuation)

#5. normalize whitespaces

corpus <- tm_map(corpus, stripWhitespace)

# Visualizing the data

pal1 <- brewer.pal(9,"YlGn")
pal1 <- pal1[-(1:4)]

pal2 <- brewer.pal(9,"Reds")
pal2 <- pal2[-(1:4)]

#min.freq initial settings -> around 10% of the number of docs in the corpus (40 times)

par(mfrow = c(1,2))
wordcloud(corpus[trainData$type == "ham"], min.freq = 40, random.order = FALSE, colors = pal1)
wordcloud(corpus[trainData$type == "spam"], min.freq = 40, random.order = FALSE, colors = pal2)

# Creation of the DTM considering terms with at least 2 chars

sms_dtm <- DocumentTermMatrix(corpus, control = list(global = c(2, Inf)))

# Basic information about the sparse matrix

print(sms_dtm)

inspect(sms_dtm[1:10, 5:13])

sms_features <- findFreqTerms(sms_dtm, 5) #find words that appears at least 5 times
summary(sms_features)

head(sms_features)

sms_dtm_train <- DocumentTermMatrix(corpus, list(global = c(2, Inf), dictionary = sms_features))
print(sms_dtm_train)

convert_counts <- function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return (x)
}
sms_dtm_train <- apply(sms_dtm_train, MARGIN = 2, convert_counts)

head(sms_dtm_train[,1:5])

corpus <- Corpus(VectorSource(testData$text))
#1. normalize to lowercase (not a standard tm transformation)
corpus <- tm_map(corpus, content_transformer(tolower))
#2. remove numbers
corpus <- tm_map(corpus, removeNumbers)
#3. remove stopwords e.g. to, and, but, or (using predefined set of word in tm package)
corpus <- tm_map(corpus, removeWords, stopwords())
#4. remove punctuation
corpus <- tm_map(corpus, removePunctuation)
#5. normalize whitespaces
corpus <- tm_map(corpus, stripWhitespace)

sms_dtm_test <- DocumentTermMatrix(corpus, list(global = c(2, Inf), dictionary = sms_features))
#print(sms_dtm_test)

sms_dtm_test <- apply(sms_dtm_test, MARGIN = 2, convert_counts)
sms_dtm_test[1:10, 5:12]

#Evaluating the Model

#sms_classifier <- naiveBayes(sms_dtm_train, trainData$type)

sms_classifier <- train(sms_dtm_train, trainData$type, method = "nb", trControl = trainControl(method = "cv", number = 10)) #k fold cross validation

sms_classifier[[2]][1:5]
  
sms_test_pred <- predict(sms_classifier$finalModel, sms_dtm_test)$class
  
#table actual (row) vs. predicted (col): confusion matrix
  
Accuracy(sms_test_pred, testData$type)
  
F1_Score(sms_test_pred, testData$type)


