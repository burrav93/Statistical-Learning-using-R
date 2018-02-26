rm(list =ls(all = TRUE))

#install.packages("arules")
 #install.packages("arulesViz")
 #install.packages("recommenderlab")
library(arules)# for arules for Apriori
library(arulesViz) # for visualizing arules
library(recommenderlab) # for recommendation engines

setwd("C:/Users/bvkka/Desktop/edureka/338_m7_dataset_v3.1/338_m7_dataset_v3.0")

datagr_2 <- read.csv("groceries.csv", sep = ",")

datagr<- read.transactions("groceries.csv", sep =",")

summary(datagr)

inspect(datagr)

inspect(datagr[1:3])

inspect(datagr[1001:1003])

itemFrequency(datagr[,1])

itemFrequency(datagr[,1:6])

itemFrequencyPlot(datagr,support = 0.08)

itemFrequencyPlot(datagr, topN = 5)

m1 <- apriori(datagr)

summary(m1)

m2 <- apriori(datagr, parameter = list(support =0.007, confidence = 0.5))

m2

summary(m2)

inspect(m2[1:3])

inspect(sort(m2, by = "lift")[1:5])

m3 <-  apriori(datagr, parameter = list(support = 0.007, confidence = 0.2),
               appearance = list(default = "rhs", lhs = c("whole milk", "soda")))
inspect(m3[1:3])

plot(m2)

# R coded for demonstrating Recommendation Engines

data_package <- data(package = "recommenderlab")
data_package$results[,c("Item", "Title")]

data("Jester5k")
?Jester5k
nratings(Jester5k)
class(Jester5k)

# https://medium.com/@jmaxg3/101-ways-to-store-a-sparse-matrix-c7f2bf15a229

size1 <- object.size(Jester5k)
size2 <- object.size(as(Jester5k, "matrix"))
ratio <- size2/size1
ratio

head(as(Jester5k,"data.frame"))

head(as(Jester5k, "matrix"))[,1:10]
hist(getRatings(Jester5k), breaks = 100)

hist(getRatings(normalize(Jester5k)), breaks = 100)

methods(class = class(Jester5k))

?methods

recommendermodels <- recommenderRegistry$get_entries(datatype = "realRatingMatrix")
names(recommendermodels)

set.seed(3)
id <- sample(x = c(TRUE, FALSE),nrow(Jester5k), prob = c(0.8,0.2), replace = T)
jester_train <- Jester5k[id,]
jester_test <- Jester5k[!id,]

# For building a Model

recc_model <- Recommender(jester_train,method = "UBCF")

recc_model
recc_predict <- predict(recc_model,newdata = jester_test, n=10)
recc_predict

rec_list <- sapply(recc_predict@items, function(x){colnames(Jester5k)[x]})

rec_list[14:16]

number_of_items = sort(unlist(lapply(rec_list, length)), decreasing = TRUE)
table(number_of_items)

table(rowCounts(Jester5k))

model_data <- Jester5k[rowCounts(Jester5k) < 80]
dim(model_data)

boxplot(rowMeans(model_data[rowMeans(model_data)<=7 & rowMeans(model_data)>-5]))

model_data <- model_data[rowMeans(model_data)>=-5 & rowMeans(model_data)<=7]

dim(model_data)

submodeldata <- model_data[1:100]
image(submodeldata)

set.seed(3)
idd <- sample(x = c(TRUE, FALSE),nrow(model_data), prob = c(0.8,0.2), replace = T)
model_train <- model_data[idd,]
model_test <- model_data[!idd,]

ibcf_model <- Recommender(model_train, method = "IBCF", parameter = list(k=30))

ibcf_model

ibcf_details <- getModel(ibcf_model)
str(ibcf_details)

ibcf_predict <- predict(ibcf_model, model_test, n =5)
ibcf_predict


slotNames(ibcf_predict)

ibcf_predict@items[[1]]


