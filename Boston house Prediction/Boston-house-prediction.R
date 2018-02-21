rm(list =ls(all = TRUE))
# install.packages("MASS")
# install.packages("corrplot")
# install.packages("caTools")
library(MASS) # for the dataset Boston
library(corrplot) # For Correlation plots
library(caTools) # sample.split function is present in this package
library(ROCR) # For Roc Curves

dataBoston <- Boston
?Boston

# Finding the summary and structure of the Data
summary(dataBoston)
str(dataBoston)

# Finding the relation between Prices and the crime rate
plot(dataBoston$crim,dataBoston$medv)

# Plotting Correlation Plots
cr <- cor(dataBoston)

corrplot(cr,type = "lower")

# Dividing the data into Training and Testing Data

split <- sample.split(dataBoston$medv, SplitRatio = 0.7)

split

training_data <- subset(dataBoston, split == TRUE)
testing_data <- subset(dataBoston, split == FALSE)

# Building the Linear Regression Model

model <- lm(medv~.,data = training_data)

medv~black+lstat+tax, data = training_data

summary(model)

predic <- predict(model, testing_data)

predic

plot(testing_data$medv, type = "l", lty = 1.8, col = "green")
lines(predic,type ="l", col = "blue")

