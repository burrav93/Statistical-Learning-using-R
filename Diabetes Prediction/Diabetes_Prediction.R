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

cm<-table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.7)

# Plotting the ROC Curves
#install.pacakges("ROCR")
library(ROCR)
ROCPred <- prediction(predi, testing_set$Is_Diabetic)
ROCPerf <- performance(ROCPred, "tpr", "fpr")
plot(ROCPerf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1), 
     text.adj = c(-0.2,1.7), cex = 0.7)

table(Actualvalues = testing_set$Is_Diabetic, Predictedvalues = predi>0.4)


accuracy_LR<-(cm[1]+cm[4])/(cm[1]+cm[2]+cm[3]+cm[4])
accuracy_LR

# To see data as a dataframe

newdata <- as.data.frame(predi)
condition <- ifelse(newdata>0.4, "Yes", "No")
ndata <- data.frame(testing_set$Is_Diabetic, condition)



#Building the decision tree
# we use the rpart library
#install.packages("rpart")
library(rpart)
colnames(diabet)
diabet_model <- rpart(Is_Diabetic~., data = training_set)


# Here we are using all the columns for the model

diabet_model
par(mar = rep(2, 4))

plot(diabet_model, margin = 0.1)
text(diabet_model, use.n = TRUE, pretty = TRUE,cex = 0.6)

temp <- subset(training_set, glucose_conc < 154.5 & BMI < 26.35)

table(temp$Is_Diabetic)

pred_diabet <- predict (diabet_model, newdata = testing_set, type = "class")
pred_diabet

#install.packages("caret")
#install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(table(pred_diabet, testing_set$Is_Diabetic))

# Building a Random Forest Model
#install.packages("randomForest")
library(randomForest)
diabet_forest <- randomForest(Is_Diabetic~., data = training_set)

diabet_forest

pred1_diabet <- predict (diabet_forest, newdata = testing_set, type = "class")

pred1_diabet

# Confusion Matrix with Random Forest

confusionMatrix(table(pred1_diabet, testing_set$Is_Diabetic))


# Building a Naive Bayes Model

diabet_naive <- naiveBayes(Is_Diabetic~., data = training_set)

diabet_naive

pred2_diabet <- predict (diabet_naive, newdata = testing_set, type = "class")

pred2_diabet

# Confusion Matrix with Naive Bayes

confusionMatrix(table(pred2_diabet, testing_set$Is_Diabetic))

# Building a SVM 


model <- svm(Is_Diabetic~.,data = training_set, kernel = "linear", cost = 0.1,
             scale = F)
summary(model)


plot(model,training_set)

# Predicting using SVM model

pred_svm <- predict(model,testing_set, type = "class")
pred_svm

# Confusion Matrix for SVM Model

confusionMatrix(table(pred_svm, testing_set$Is_Diabetic))





