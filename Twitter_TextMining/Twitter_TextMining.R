rm(list =ls(all = TRUE))

# install.packages("tm")
# install.packages("SnowballC")
# install.packages("dplyr")

library(tm) # Text Mining Library
library(SnowballC) # For additional Text Analysis
library(dplyr)# For manipulating and arranging the code using Pipes
library(twitteR) # For Twitter API and data

setwd("C:/Users/bvkka/Desktop/edureka/338_m8_dataset_v3.0")

# reading the Books
bookHP6 <- readLines('HP6 - The Half Blood Prince.txt')

bookHP7 <- readLines('HP7 - Deathly Hollows.txt')

# Creating the corpus
corpusHP6 <- Corpus(VectorSource(bookHP6))

# Executing Text Preprocessing tasks on the Corpus
corpusHP6 <- Corpus(VectorSource(bookHP6))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords, stopwords("english"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)

dtmHP6 <- DocumentTermMatrix(corpusHP6)

dtmHP6

dtmHP6 <- removeSparseTerms(dtmHP6,0.99)

dtmHP6

# Creating the corpus
corpusHP7 <- Corpus(VectorSource(bookHP7))

# Executing Text Preprocessing tasks on the Corpus
corpusHP7 <- Corpus(VectorSource(bookHP7))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords, stopwords("english"))%>%
  tm_map(stripWhitespace)%>%
  tm_map(stemDocument)

dtmHP7 <- DocumentTermMatrix(corpusHP7)

dtmHP7

dtmHP7 <- removeSparseTerms(dtmHP7,0.99)

dtmHP7



word.freqHP6 <- sort(colSums(as.matrix(dtmHP6)),decreasing = T)
head(word.freqHP6)

table.HP6 <- data.frame(word = names(word.freqHP6), 
                        absolute.frequency = word.freqHP6,
                        relative.frequency = word.freqHP6/length(word.freqHP6))

head(table.HP6)

word.freqHP7 <- sort(colSums(as.matrix(dtmHP7)),decreasing = T)
head(word.freqHP7)

table.HP7 <- data.frame(word = names(word.freqHP7), 
                        absolute.frequency = word.freqHP7,
                        relative.frequency = word.freqHP7/length(word.freqHP7))

head(table.HP7)



# Removing the Row Names
rownames(table.HP6) <- NULL
head(table.HP6)

rownames(table.HP7) <- NULL
head(table.HP7)


# Save the Table as
write.csv(table.HP6[1:1000,],"HP6_1000.csv")

write.csv(table.HP7[1:1000,],"HP7_1000.csv")

finalTable <- table.HP6 %>%
  merge(table.HP7, by ="word") %>%
  mutate(dProp = relative.frequency.x - relative.frequency.y,dAbs = abs(dProp)) %>%
  arrange(desc(dAbs)) %>%
  rename(HP6.freq = absolute.frequency.x,HP6.prop = relative.frequency.x,
         HP7.freq = absolute.frequency.y, HP7.prop = relative.frequency.y)

head(finalTable)

# Reading Data from Twitter

# https://rstudio-pubs-static.s3.amazonaws.com/238562_4a31ee14fbb74cb9a6a8cdbf6bde5ec7.html

# https://stackoverflow.com/questions/39962970/what-variable-should-replace-sentences

# consumer_key <- âyour_consumer_keyâ
# consumer_secret <- âyour_consumer_secretâ
# access_token <- âyour_access_tokenâ
# access_secret <- âyour_access_secretâ
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# 
# google   <- searchTwitter('#google', n = 1000, lang = 'en')
# microsft <- searchTwitteR('#microsoft', n =1000, lang = 'en')
# amazon   <- searchTwitter('#amazon', n = 1000, lang = 'en')
# apple    <- searchTwitter('#apple', n = 1000, lang = 'en')


#get text
# google_txt   <- sapply(google, function(x)x$getText())
# microsft_txt <- sapply(microsft, function(x)x$getText())
# amazon_txt   <- sapply(amazon, function(x)x$getText())
# apple_txt    <- sapply(apple, function(x)x$getText())



# 
# score.sentiment = function(sentences, final_good_text, final_bad_text, .progress='none')
# {
#   require("plyr")
#   require("stringr")
#   # we got a vector of sentences. plyr will handle a list
#   # or a vector as an "l" for us
#   # we want a simple array of scores back, so we use
#   # "l" + "a" + "ply" = "laply":
#   scores = laply(sentences, function(sentence, final_good_text, final_bad_text) {
#     
#     # clean up sentences with R's regex-driven global substitute, gsub():
#     sentence = gsub('[[:punct:]]', '', sentence) #     Removes Punctuation
#     sentence = gsub('[[:cntrl:]]', '', sentence) # Removes Control Characters
#     sentence = gsub('\\d+', '', sentence) # Removes Digits
#     #to remove emojis
#     sentence <- iconv(sentence, 'UTF-8', 'ASCII')
#     sentence = tolower(sentence)        
#     # split into words. str_split is in the stringr package
#     word.list = str_split(sentence, '\s+')
#     # sometimes a list() is one level of hierarchy too much
#     words = unlist(word.list)
#     
#     # compare our words to the dictionaries of positive & negative terms
#     pos.matches = match(words, final_good_text)
#     neg.matches = match(words, final_bad_text)
#     
#     # match() returns the position of the matched term or NA
#     # we just want a TRUE/FALSE:
#     pos.matches = !is.na(pos.matches)
#     neg.matches = !is.na(neg.matches)
#     
#     # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#     score = sum(pos.matches) - sum(neg.matches)
#     
#     return(score)
#     
#     
#   }, final_good_text, final_bad_text, .progress=.progress )
#   
#   scores.df = data.frame(score=scores, text=sentences)
#   return(scores.df)
# }
# 
