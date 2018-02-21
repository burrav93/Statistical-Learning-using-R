# Set the Working Directory to read the file for logistic regression
rm(list =ls(all = TRUE))
setwd("C:/Users/bvkka/Desktop/edureka/")

# reading the Data

diabet <- read.csv("Diabetes.csv", header = TRUE)

summary(diabet)
str(diabet)

#install.packages("plyr")
library(plyr)
diabet$Is_Diabetic <- revalue(diabet$Is_Diabetic,c("YES"=1))
diabet$Is_Diabetic <- revalue(diabet$Is_Diabetic,c("NO"=0))

#OR

diabet$Is_Diabetic <- factor(diabet$Is_Diabetic,levels=c('NO','YES'),c(0,1))

# Dividing the data into Test and Train
library(caTools)
set.seed(123)
di <- sample.split(diabet$Is_Diabetic, SplitRatio = 0.7)

di

training_set <- subset(diabet, di == TRUE)
testing_set <- subset(diabet, di == FALSE)

# Building the Logistic Regression Model

model2 <- glm(Is_Diabetic~.,data = training_set, family = "binomial" )

summary(model2)

predi <- predict(model2,newdata = testing_set,type = "response")

predi

# Building the Confusion Matrix

table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.5)

table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.3)

table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.7)

# Plotting the ROC Curves
#install.pacakges("ROCR")
library(ROCR)
ROCPred <- prediction(predi, testing_set$Is_Diabetic)
ROCPerf <- performance(ROCPred, "tpr", "fpr")
plot(ROCPerf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1), 
     text.adj = c(-0.2,1.7), cex = 0.7)

table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.4)

# To see data as a dataframe

newdata <- as.data.frame(predi)
condition <- ifelse(newdata>0.4, "Yes", "No")
ndata <- data.frame(testing_set$Is_Diabetic, condition)
