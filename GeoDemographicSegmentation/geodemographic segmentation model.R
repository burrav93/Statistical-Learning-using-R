#Geodemographic segmentation model

#Complete data set
library(readxl)
Churn_Modelling <- read_excel("C:/Users/bvkka/Desktop/Udemy/Data Science/Churn-Modelling.xlsx")
View(Churn_Modelling)




# creating dummy variables
install.packages("plyr")
library(plyr)
Churn_Modelling$Geography <- revalue(Churn_Modelling$Geography,c("France"=0))
Churn_Modelling$Geography <- revalue(Churn_Modelling$Geography,c("Spain"=1))
Churn_Modelling$Geography <- revalue(Churn_Modelling$Geography,c("Germany"=2))


Churn_Modelling$Gender <- revalue(Churn_Modelling$Gender,c("Female"=0))
Churn_Modelling$Gender <- revalue(Churn_Modelling$Gender,c("Male"=1))

Female<-as.numeric(Churn_Modelling$Gender==0)
Spain<-as.numeric(Churn_Modelling$Geography==1)
Germany<-as.numeric(Churn_Modelling$Geography==2)


#character to numeric
Churn_Modelling$Gender<-as.numeric(as.character(Churn_Modelling$Gender))
Churn_Modelling$Geography<-as.numeric(as.character(Churn_Modelling$Geography))


wealthAccumulation<-(Churn_Modelling$Balance)/(Churn_Modelling$Age)
Age1<-Churn_Modelling$Age
Balance1<-Churn_Modelling$Balance
Balance2<-log10(Balance1+1)
wealthAccumulationlog<-log10(Balance1/Age1+1)

#logestic regression
#creating a model
model1<-glm(formula = Exited ~ CreditScore+wealthAccumulationlog+Age+NumOfProducts+IsActiveMember+Female+Germany, binomial(link="logit"),data =Churn_Modelling)
summary(model1)
#library(dplyr)
prob_predict1=predict(model1,type = 'response')

summary(prob_predict1)


y_pred=ifelse(prob_predict1>0.5,1,0) #vector of predictions
y_pred
#Making the confusion Matrix
cm=table(y_pred,Churn_Modelling$Exited)       
cm
TN<-cm[1] #7681
FN<-cm[2] #282
FP<-cm[3] #1605
TP<-cm[4] #432

ActualYes<-FN+TP
ActualNo<-TN+FP
PredictedYes<-FP+TP
PredictedNo<-TN+FN
#accuracy (TP+TN)/(TP+TN+FN+FP)
accuracy1=mean(y_pred==Churn_Modelling$Exited) 
accuracy1
#misclassification rate: (FP+FN)/(TP+TN+FN+FP)
MR<-(FP+FN)/(TN+FN+FP+TP)

#TPR : When it's actually yes, how often does it predict yes?
TPR<-TP/(ActualYes)
#FPR : when it's actually no, how often does it predict yes?
FPR<-FP/(ActualNo)
#precision : when it predicts yes, how often is it correct? TP/TP+FP
precision1=precision<-diag(cm)/colSums(cm)
precision1
#recall : 
recall1=recall<-diag(cm)/rowSums(cm)
recall1
#ROC Curve
library(pROC)
myROC<-roc(response=Churn_Modelling$Exited,predictor = prob_predict1,positve='prob_predict1')
plot(myROC)
pred1<-prediction(prob_predict1,Churn_Modelling$Exited)
roc.perf=performance(pred1,measure = "tpr",x.measure = "fpr")
ggplot(mode)


#auc 0.7669
auc(roc(Churn_Modelling$Exited,prob_predict1))


#####now we add a new test data and see how classifier predicts#####

#test data set
##merged test data with train data except the lasr column, model should predict that

library(readxl)
Churn_Modelling_testt <- read_excel("C:/Users/bvkka/Desktop/Udemy/Data Science/Churn-Modelling-testt.xlsx")
View(Churn_Modelling_testt)


Churn_Modelling_testt$Gender <- revalue(Churn_Modelling_testt$Gender,c("Female"=0))
Churn_Modelling_testt$Gender <- revalue(Churn_Modelling_testt$Gender,c("Male"=1))
Churn_Modelling_testt$Geography <- revalue(Churn_Modelling_testt$Geography,c("France"=0))
Churn_Modelling_testt$Geography <- revalue(Churn_Modelling_testt$Geography,c("Spain"=1))
Churn_Modelling_testt$Geography <- revalue(Churn_Modelling_testt$Geography,c("Germany"=2))


Churn_Modelling_testt$Gender<-as.numeric(as.character(Churn_Modelling_testt$Gender))
Churn_Modelling_testt$Geography<-as.numeric(as.character(Churn_Modelling_testt$Geography))

model2<-glm(formula = Exited ~ CreditScore+wealthAccumulationlog+Age+NumOfProducts+IsActiveMember+Female+Germany, binomial(link="logit"),data =Churn_Modelling_testt)
summary(model2)
#predicting the test set results
prob_pred2=predict(model1,type='response',newdata = Churn_Modelling_testt)   #for predicitng we only need predictors , but not response
summary(prob_pred2)
y_pred2=ifelse(prob_pred2>0.5,1,0) #vector of predictions
y_pred2

#Making the confusion Matrix
cm=table(y_pred2,Churn_Modelling_testt$Exited)       
cm





