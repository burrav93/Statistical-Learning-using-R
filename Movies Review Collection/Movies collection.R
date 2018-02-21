rm(list =ls(all = TRUE))

# https://stackoverflow.com/questions/3536036/rmlist-ls-doesnt-completely-clear-the-workspace

 #install.packages("xlsx")
 #install.packages("RMySQL")
 
#install.packages("XML")
 #install.packages("RCurl")
 #install.packages("RJSONIO")
 #install.packages("twitteR")
 #install.packages("stringr")
 #install.packages("plyr")
library(xlsx)
library(XML)
library(RCurl)
library(RMySQL)
library(stringr)
library(RJSONIO)
library(twitteR)
library(plyr)


setwd("C:/Users/bvkka/Desktop/edureka/338_m3_dataset_v3.0/338_m3_dataset_v3.0")

# Reading CSV Files

movie <- read.csv("movie_metadata.csv")

# Note about assignment operator
# https://stackoverflow.com/questions/1741820/assignment-operators-in-r-and

View(movie)

download.file("https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD",
              destfile = "C:/Users/bvkka/Desktop/edureka/data/cameras.csv")

# Readind Excel Files
#install.packages("XLConnect")
library(XLConnect)
require(xlsx)
theData <- readWorksheet(loadWorkbook("C:/AB_DNA_Tag_Numbers.xlsx"),sheet=1)
#install.packages("readxl")
list.files("C:/Users/bvkka/Desktop/edureka/data")
InsuranceData <-readxl("C:/Users/bvkka/Desktop/edureka/data/InsuranceData.xlsx",sheetIndex = 1, header = TRUE)
colNumbers <- c(2:3)
rowNumbers <- c(2:10)
InsuranceData2 <-read.xlsx("./data/InsuranceData.xlsx",sheetIndex = 1,colIndex = colNumbers, 
                           rowIndex = rowNumbers, header = TRUE)
# https://stackoverflow.com/questions/11966335/how-to-import-excel-data-into-r-using-column-name-and-row-name

# Reading SQL


# hg19 <- dbConnect(MySQL(), user = "genome", db = "hg19", host = "genome-mysql.cse.usce.edu")

# allTables <- dbListTables(hg19)

# dbGetQuery(hg,"select count(*) from affyU133Plus2")


#  Webscraping
mps <- "http://news.bbc.co.uk/2/hi/uk_politics/8044207.stm"
mps.doc <- htmlParse(mps)
mps.tabs <- readHTMLTable(mps.doc)

# Reading XML Files


xmlLink <- "https://www.w3schools.com/xml/simple.xml"
xData <- getURL(xmlLink)
doc <- xmlParse(xData)

rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
# https://stackoverflow.com/questions/23584514/error-xml-content-does-not-seem-to-be-xml-r-3-1-0


# Reading JSON files

foodMarketsRaw <- fromJSON("https://data.ny.gov/api/views/9a8c-vfzj/rows.json?accessType=DOWNLOAD")
foodMarkets <-foodMarketsRaw[['data']]


foodMarkets[[1]][[14]]

foodMarkets[[2]][[14]]

# Reading Data from Twitter

# consumer_key <- “your_consumer_key”
# consumer_secret <- “your_consumer_secret”
# access_token <- “your_access_token”
# access_secret <- “your_access_secret”
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# tweets <- searchTwitter(search.string,100, lang="en")
# tweets


# df <- twListToDF(TrumpTweets)
# head(df)

#create a corpus
# myCorpus <- Corpus(VectorSource(df$text))

#cleanup the text
# myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation
# myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
# myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep words by removing it from stopwords
# myStopwords <- c(stopwords('english'), "available", "via")
# myStopwords <- stopwords('english')
# myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

## Eliminating Extra White Spaces
# myCorpus<- tm_map( myCorpus, stripWhitespace)

#remove stemdocument
# dictCorpus <- myCorpus
#remove plurals
# library(SnowballC)
# myCorpus <- tm_map( myCorpus, stemDocument)

movie$color <- str_trim(movie$color)
Colour_movie <- movie[which(movie$color == "Color"),]


movie2 <- movie[,c(2,5)]
movie_nomissing <- na.omit(movie)


movie$movie_title <- sub(pattern = "�", replacement ="", movie$movie_title)

# Summary function to find out the characteristics of the data
summary(movie)

# To filter out null values in the Data
is.na(movie)
sum(is.na(movie))
sum(complete.cases(movie))
NROW(na.omit(movie))
colnames(movie)[colSums(is.na(movie)) > 0]
which(is.na(movie))

# http://www.endmemo.com/program/R/which.php

# Removing Null Values
movies <- na.omit(movie)

# For Filtering Color Movies

Colour_movies <- subset(movies, movies$color=="Color")
dim(movie)
class(movie)
nrow(movie)
ncol(movie)
head(movie, 10)
tail(movie)
names(movie)
str(movie)
levels(movie)
levels(movie$color)
summary(movie)


# For Finding the BUdget of each movie we can subtract Budget from Gross Collection

movie1 <- movies
movie1$profit <- movie1$gross - movie1$budget

cbind(movie1, movie1$profit)
View(movie1)

movie1_profit <- movie1[order(-movie1$profit),]
cbind(movie1, movie1_profit)

head(movie1_profit$movie_title,10)
View(movie1_profit)

# We can filter out the Data

profit <- movie1_profit[1:10,c(12,29)]
View(profit)

# Since only IMDB score and movie name is required for this problem, we can filter
movie2 <- movies[,c(12,26)]
View(movie2)

# We can sort the IMDB score to get the top Ratings

movie2.1 <- movie2[order(-movies$imdb_score),]
head(movie2.1,10)

# For Finding the average

mean(movie2.1$imdb_score)

movie3 <- movies[,c(24,12)]
View(movie3)

# To change the Col Names
colnames(movie3)[1] <- "Year"
colnames(movie3)[2] <- "Title"
colnames(movie3)
View(movie3)

# Histogram to analyze the no of movies per year
par(mar = rep(2, 4))
hist(movie3$Year, breaks =24)
histinfo <- hist(movie3$Year, breaks = 24, col = "blue")

# To be more accurate we can customize using breaks
hist(movie3$Year, breaks = seq(1920, 2016, by =1), 
          main = "No of Movie Per year", xlab= "YEAR")
histinfo <- hist(movie3$Year, breaks = seq(1920, 2016, by =1), 
           main = "No of Movie Per year", xlab= "YEAR", col = "blue")
histinfo

# To find out the counts

histinfo$counts

# To find the max

which.max(histinfo$counts)

# To find the value in Row 82
histinfo$mids[82]
histinfo$mids[86]

# So we can conclude that years 2002 and 2006 had the most movies
# To list out movies in that year

movie3.1 <- movie3[movie3$Year =="2002",]
View(movie3.1)
movie3.2 <- movie3[movie3$Year =="2006",]
View(movie3.2)

# Stripchart

stripchart(profit$profit)

# Histogram

hist(movie$title_year)

# Boxplot

boxplot(movie2.1$imdb_score)

# Scattter PLots

plot(movie$duration)

plot(movie$duration,movie$gross)
