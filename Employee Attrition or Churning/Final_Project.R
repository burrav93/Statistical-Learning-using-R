####Setting the directory and loading the dataset into R, verifying that dataset is loaded correctly
library(readr)
X338_cert_proj_datasets_v3_0 <- read_csv("C:/Users/bvkka/Desktop/edureka/338_cert_proj_datasets_v3.0.csv")
View(X338_cert_proj_datasets_v3_0)
HR_Management<-X338_cert_proj_datasets_v3_0

str(HR_Management)

###Making some changes to the columns
library(plyr)
  HR_Management$salary<- revalue(HR_Management$salary,c("low"=0))
  HR_Management$salary<- revalue(HR_Management$salary,c("medium"=1))
  HR_Management$salary<- revalue(HR_Management$salary,c("high"=2))
  
  HR_Management$department<- revalue(HR_Management$department,c("hr"=0))
  HR_Management$department<- revalue(HR_Management$department,c("IT"=1))
  HR_Management$department<- revalue(HR_Management$department,c("management"=2))
  HR_Management$department<- revalue(HR_Management$department,c("marketing"=3))
  HR_Management$department<- revalue(HR_Management$department,c("product_mng"=4))
  HR_Management$department<- revalue(HR_Management$department,c("RandD"=5))
  HR_Management$department<- revalue(HR_Management$department,c("sales"=6))
  HR_Management$department<- revalue(HR_Management$department,c("support"=7))
  HR_Management$department<- revalue(HR_Management$department,c("technical"=8))
  HR_Management$department<- revalue(HR_Management$department,c("accounting"=9))
  
  str(HR_Management)
  
  
  
HR_Management$left<-as.factor(as.integer(HR_Management$left))
HR_Management$salary<-as.factor(as.integer(HR_Management$salary))
HR_Management$department<-as.factor(as.integer(HR_Management$department))

str(HR_Management)

###splitting the data into train and test sets
library(caret)
set.seed(12345)
di <- sample(2, nrow(HR_Management), prob = c(0.7,0.3), replace = TRUE)

train <- HR_Management[di==1,]
test <- HR_Management[di==2,]

###using ggplots for visualizations

library(ggplot2)
ggplot(train,aes(left,fill=left))+geom_bar()
prop.table(table(train$left)) #Percentage of left
prop.table(table(train$satisfaction_level))

#Let us look at each variable and see its influence on the churn of the organization

library(ggplot2)
library(grid)
library(gridExtra)
promotion_last_5yearsPlot <- ggplot(train,aes(promotion_last_5years,fill=left))+geom_density()+facet_grid(~left)
time_spend_companyPlot <- ggplot(train,aes(time_spend_company,fill=left))+geom_bar()
salaryPlot <- ggplot(train,aes(salary,left))+geom_point(size=4,alpha = 0.05)
depPlot <- ggplot(train,aes(department,fill = left))+geom_bar()
grid.arrange(promotion_last_5yearsPlot,time_spend_companyPlot,salaryPlot,depPlot,ncol=2,top = "Fig 1")

satisfaction_levelPlot <- ggplot(train,aes(satisfaction_level,fill=left))+geom_bar()
last_evaluationPlot <- ggplot(train,aes(last_evaluation,fill=left))+geom_bar()
number_projectPlot <- ggplot(train,aes(number_project,fill=left))+geom_bar()
average_montly_hoursPlot <- ggplot(train,aes(average_montly_hours,fill=left))+geom_bar()
Work_accidentPlot <- ggplot(train,aes(Work_accident,fill=left))+geom_bar()
grid.arrange(satisfaction_levelPlot,last_evaluationPlot,number_projectPlot,average_montly_hoursPlot,Work_accidentPlot,ncol=2,top = "Fig 2")

###Binning of Varaibles
##creating average monthly bins
max(train$average_montly_hours)
min(train$average_montly_hours)


train$average_montly_hoursGroup<-with(train,ifelse(average_montly_hours>300,7,ifelse(average_montly_hours>250,6,ifelse(average_montly_hours>200,5,ifelse(average_montly_hours>150,4,ifelse(average_montly_hours>100,3,ifelse(average_montly_hours>50,2,1)))))))
test$average_montly_hoursGroup<-with(test,ifelse(average_montly_hours>300,7,ifelse(average_montly_hours>250,6,ifelse(average_montly_hours>200,5,ifelse(average_montly_hours>150,4,ifelse(average_montly_hours>100,3,ifelse(average_montly_hours>50,2,1)))))))

##creating satisfacting level bins
max(train$satisfaction_level)
min(train$satisfaction_level)
train$satisfaction_levelGroup<-with(train,ifelse(satisfaction_level>0.9,9,ifelse(satisfaction_level>0.8,8,ifelse(satisfaction_level>0.7,7,ifelse(satisfaction_level>0.6,6,ifelse(satisfaction_level>0.5,5,ifelse(satisfaction_level>0.4,4,ifelse(satisfaction_level>0.3,3,ifelse(satisfaction_level>0.2,2,ifelse(satisfaction_level>0,1))))))))))
test$satisfaction_levelGroup<-with(test,ifelse(satisfaction_level>0.9,9,ifelse(satisfaction_level>0.8,8,ifelse(satisfaction_level>0.7,7,ifelse(satisfaction_level>0.6,6,ifelse(satisfaction_level>0.5,5,ifelse(satisfaction_level>0.4,4,ifelse(satisfaction_level>0.3,3,ifelse(satisfaction_level>0.2,2,ifelse(satisfaction_level>0,1))))))))))

##creating timespendatcompany level bins
max(train$time_spend_company)
min(train$time_spend_company)
train$time_spend_companyGroup<-with(train,ifelse(time_spend_company>9,9,ifelse(time_spend_company>8,8,ifelse(time_spend_company>7,7,ifelse(time_spend_company>6,6,ifelse(time_spend_company>5,5,ifelse(time_spend_company>4,4,ifelse(time_spend_company>3,3,ifelse(time_spend_company>2,2,ifelse(time_spend_company>0,1))))))))))
test$time_spend_companyGroup<-with(test,ifelse(time_spend_company>9,9,ifelse(time_spend_company>8,8,ifelse(time_spend_company>7,7,ifelse(time_spend_company>6,6,ifelse(time_spend_company>5,5,ifelse(time_spend_company>4,4,ifelse(time_spend_company>3,3,ifelse(time_spend_company>2,2,ifelse(time_spend_company>0,1))))))))))

###Correlation of Variables
library(corrplot)
library(psych)

#first make all the variables to numeric or integer to correlate
train$left<-as.numeric(as.factor(train$left))
train$department<-as.numeric(as.factor(train$department))
train$salary<-as.numeric(as.factor(train$salary))


train$promotion_last_5years<-as.numeric(as.integer(train$promotion_last_5years))
train$Work_accident<-as.numeric(as.integer(train$Work_accident))
train$time_spend_company<-as.numeric(as.integer(train$time_spend_company))
train$average_montly_hours<-as.numeric(as.integer(train$average_montly_hours))
train$number_project<-as.numeric(as.integer(train$number_project))


test$department<-as.numeric(as.factor(test$department))
test$salary<-as.numeric(as.factor(test$salary))


cor(train) #correlation values between variables

corrplot(cor(train),method = "circle")




##applying Logistic model to see which variables are more significant in churning out employers
model1<-glm(formula = left ~ ., binomial(link="logit"),data =train)
summary(model1)
library(MASS)
exp(cbind(OR=coef(model1),confint(model1)))

###Building models using Decision Tree, Random Forest ,NB and SVM techniques
library(caret)
library(rpart)
library(ROCR)

#####Decision Tree####



DTree_model <- rpart(left~satisfaction_levelGroup+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident, data = train)

par(mar = rep(2, 4))

plot(DTree_model, margin = 0.1)
text(DTree_model, use.n = TRUE, pretty = TRUE,cex = 0.6)
table(train$left)

pred_DTree_model <- predict (DTree_model, newdata = test)
pred_DTree_model
cm_DTree<-table(pred_DTree_model,test$left)
cm_DTree
accuracy_DTree<-(cm_DTree[1]+cm_DTree[4])/(cm_DTree[1]+cm_DTree[2]+cm_DTree[3]+cm_DTree[4])
accuracy_DTree

confusionMatrix(table(pred_tree, test$left))




####Random Forest#####
library(randomForest)
RF_model <- randomForest(left~., data = train)
RF_model


pred_RF_model <- predict (RF_model, newdata = test, type = "class")

pred_RF_model
cm_RF<-table(pred_RF_model,test$left)
cm_RF
accuracy_RF<-(cm_RF[1]+cm_RF[4])/(cm_RF[1]+cm_RF[2]+cm_RF[3]+cm_RF[4])
accuracy_RF




#####Naive Bayes#####
library(e1071)
NB_model <- naiveBayes(left~salary+time_spend_companyGroup+satisfaction_levelGroup+department+average_montly_hoursGroup, data = train,laplace = laplace)

NB_model

pred_NB_model <- predict (NB_model, newdata = test)

pred_NB_model
cm_NB<-table(pred_NB_model,test$left)
cm
accuracy_NB<-(cm[1]+cm[4])/(cm[1]+cm[2]+cm[3]+cm[4])
accuracy_NB



#####SVM######
library(e1071)

SVM_model <- svm(left~.,data = train,type="C-classification", kernel = "radial", cost = 0.1,
             gamma=c(.5,1,2))


pred_svm <- predict(SVM_model,test, type = "class")
cm_svm<-table(pred_svm,test$left)
cm
accuracy_svm<-(cm[1]+cm[4])/(cm[1]+cm[2]+cm[3]+cm[4])
accuracy_svm












