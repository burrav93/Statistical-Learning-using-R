rm(list =ls(all = TRUE))
setwd("C:/Users/bvkka/Desktop/edureka")
library(readr)
RestaurantReviews<-read_tsv(file = 'M8_Restaurant_Reviews.tsv', na=":")

#install.packages("tm")
#install.packages("SnowballC")
library(tm)
library(SnowballC)
library(dplyr)



# Creating the corpus

corpusRR<- Corpus(VectorSource(RestaurantReviews))

##Perform the following text cleaning techniques

corpusRR <- Corpus(VectorSource(RestaurantReviews))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords, stopwords("english"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)
dtmRR <- DocumentTermMatrix(corpusRR)

dtmRR
dtmRR <- removeSparseTerms(dtmRR,0.99)

dtmRR

library(wordcloud)
wordcloud(corpusRR, max.words = 40,
          random.order = FALSE,
          random.color = TRUE)



###TASK2###


word.freqRR <- sort(colSums(as.matrix(dtmRR)),decreasing = T)
head(word.freqRR)

table.RR <- data.frame(word = names(word.freqRR), 
                        absolute.frequency = word.freqRR,
                        relative.frequency = word.freqRR/length(word.freqRR))

head(table.RR)

library(ggplot2)
subset(corpusRR, frequency > 100)%>%
  ggplot(aes(word, frequency))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))



# Save the Table as
write.csv(table.RR[1:1000,],"RR.csv")

finalTable <- table.RR %>%
  merge(table.RR, by ="word") %>%
  mutate(dProp = relative.frequency.x - relative.frequency.y,dAbs = abs(dProp)) %>%
  arrange(desc(dAbs)) %>%
  rename(RR.freq = absolute.frequency.x,RR.prop = relative.frequency.x)

head(finalTable)


###splitting the data into train and test sets
                       

set.seed(12345)
di <- sample(2, nrow(RestaurantReviews), prob = c(0.7,0.3), replace = TRUE)

train <- RestaurantReviews[di==1,]
test <- RestaurantReviews[di==2,]

##converting it into data frame
dtm_matrix<-as.data.frame(as.matrix(dtmRR))
View(dtm_matrix)
class(dtm_matrix)

dtm_matrix$like<-as.factor(RestaurantReviews$Liked)
##Building a classifier


#####Naive Bayes#####



library(e1071)
NB_model <- naiveBayes(Liked~., data = train)

NB_model
summary(NB_model)

test1<-as.factor(as.integer(test$Liked))

pred_NB_model <- predict (NB_model, newdata = test)

pred_NB_model



library(caret)
confusionMatrix(table(pred_NB_model, test$Liked))

