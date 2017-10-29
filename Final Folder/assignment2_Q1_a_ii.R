###only 5 variables(removed some predictors)


#import the dataset and make some changes
library(readr)
kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")

kc_weather_srt=kc_weather_srt[,5:9]

#subset that consists of only rain and snow
kc_weather_srt_without_rainthunderstorm<-kc_weather_srt[!grepl("Rain_Thunderstorm",kc_weather_srt$Events),]

dim(kc_weather_srt_without_rainthunderstorm)





#first make the response column to 0 and 1
#install.packages("plyr")
library(plyr)
kc_weather_srt_without_rainthunderstorm$Events <- revalue(kc_weather_srt_without_rainthunderstorm$Events,c("Snow"=1))
kc_weather_srt_without_rainthunderstorm$Events <- revalue(kc_weather_srt_without_rainthunderstorm$Events,c("Rain"=0))

#character to numeric 
kc_weather_srt_without_rainthunderstorm$Events<-as.numeric(as.character(kc_weather_srt_without_rainthunderstorm$Events))



# number of replications
rep=100



# newly added

#snow=1 rain=0
accuracy=dim(rep)
accuracy1=dim(rep)
accuracy2=dim(rep)
accuracy3=dim(rep)

precision_snow=dim(rep)
precision_rain=dim(rep)
recall_snow=dim(rep)
recall_rain=dim(rep)

precision_snow1=dim(rep)
precision_rain1=dim(rep)
recall_snow1=dim(rep)
recall_rain1=dim(rep)

precision_snow2=dim(rep)
precision_rain2=dim(rep)
recall_snow2=dim(rep)
recall_rain2=dim(rep)


precision_snow3=dim(rep)
precision_rain3=dim(rep)
recall_snow3=dim(rep)
recall_rain3=dim(rep)


#splitting the dataset into training and test sets, also install caTools packages
#install.packages('caTools')
library(caTools)
set.seed(123)



for(k in 1:rep)
{
  
  
  split=sample.split(kc_weather_srt_without_rainthunderstorm$Events,SplitRatio = 0.8) #80% split ratio
  training_set=subset(kc_weather_srt_without_rainthunderstorm,split==TRUE) #80% split into training set
  test_set=subset(kc_weather_srt_without_rainthunderstorm,split==FALSE) #20% split into testing set
  table(split)
  dim(training_set)
  


#****Logistic Regression***#
#Fitting Logistic Regression to the Training set
model1<-glm(formula = Events ~ ., binomial(link="logit"),data =training_set)

#predicting the test set results
prob_pred=predict(model1,type='response',newdata = test_set[-5])   #for predicitng we only need predictors , but not response
summary(prob_pred)
y_pred=ifelse(prob_pred>0.5,1,0) #vector of predictions
y_pred

#Making the confusion Matrix
cm=table(y_pred,test_set[,5])       
cm

#accuracy
accuracy[k]=mean(y_pred==test_set[,5]) 
accuracy 


#precision

precision=precision<-diag(cm)/colSums(cm)
precision_snow[k]=precision[2]
precision_rain[k]=precision[1]

#recall
recall=recall<-diag(cm)/rowSums(cm)
recall_snow[k]=recall[2]
recall_rain[k]=recall[1]







#***LDA***#
#install.packages("MASS")
library(MASS)



lda=lda(formula=Events~.,data=training_set)
y_pred1=predict(lda,test_set)$class
cm1=table(y_pred1,test_set[,5])
cm1
accuracy1[k]=mean(y_pred1==test_set[,5]) 

precision1=precison1<-diag(cm1/colSums(cm1))
precision_snow1[k]=precison1[2]
precision_rain1[k]=precision1[1]
recall1=recall1<-diag(cm1/rowSums(cm1))
recall_snow1[k]=recall1[2]
recall_rain1[k]=recall1[1]









#a=lda(formula=Events~.,data=training_set)
#training_set1=as.data.frame(predict(lda,training_set))
#training_set1=training_set1[c(4,1)]
#plot(lda)

#test_set1=as.data.frame(predict(lda,test_set))
#test_set1=test_set1[c(4,1)]

#fitting SVM to the training set
#install.packages('e1071')
#library(e1071)
#classifier=svm(formula=class~.,data=training_set1,type='C-classification',kernel='linear')
#predicting the test set results
#y_pred1=predict(classifier,newdata = test_set1[-2])
#making the confusion matrix
#cm1=table(y_pred1,test_set1[,2])
#cm1 #we see TP+TN=33+12=45 true predictions and FP+FN=0+0=0 False predictions, therefore it is 100% accurate
#accuracy1[k]=mean(y_pred1==test_set1[,2]) 
#accuracy1 

precision1=precision1<-diag(cm1)/colSums(cm1)
precision_snow1[k]=precision1[2]
precision_rain1[k]=precision1[1]

precision_snow1
precision_rain1



recall1=recall1<-diag(cm1/rowSums(cm1))
recall_snow1[k]=recall1[2]
recall_rain1[k]=recall1[1]

recall_snow1
recall_rain1



#***QDA***#
qda=qda(formula=Events~.,data=training_set)
y_pred2=predict(qda,test_set)$class
cm2=table(y_pred2,test_set[,5])
cm2
accuracy2[k]=mean(y_pred2==test_set[,5]) 

precision2=precison2<-diag(cm2/colSums(cm2))
precision_snow2[k]=precison2[2]
precision_rain2[k]=precision2[1]
recall2=recall2<-diag(cm2/rowSums(cm2))
recall_snow2[k]=recall2[2]
recall_rain2[k]=recall2[1]




#***KNN***#
#install.packages('class')
#fitting KNN to the training set and predicting the test set results
library(class)
y_pred3=knn(train=training_set[,-5],test=test_set[,-5],cl=training_set[,5],k=5)

#making the cm
cm3=table(y_pred3,test_set[,5])
cm3
accuracy3[k]=mean(y_pred3==test_set[,5])

precision3=precision3<-diag(cm3/colSums(cm3))
precision_snow3[k]=precision3[2]
precision_rain3[k]=precision3[1]

recall3=recall3<-recall3<-diag(cm3/rowSums(cm3))
recall_snow3[k]=recall3[2]
recall_rain3[k]=recall3[1]
}



###*******MEAN VALUES*****#####

#****Logestic regression***#
mean(accuracy)###0.8868889
mean(precision_snow)###0.704
mean(precision_rain)###0.9391429

mean(recall_snow) ### 0.778444
mean(recall_rain) ### 0.9190056



#****LDA***#
mean(accuracy1)###0.8604444
mean(precision_snow1)###0.599
mean(precision_rain1)###0.7571429

mean(recall_snow1) ### 0.7336911
mean(recall_rain1) ### 0.8922766








#****QDA***#
mean(accuracy2)###0.8546667
mean(precision_snow2)###0.8391429
mean(precision_rain2)###0.8391429

mean(recall_snow2) ### 0.6327154
mean(recall_rain2) ### 0.9704948



#****KNN***#
mean(accuracy3)###0.8188889
mean(precision_snow3)###0.295
mean(precision_rain3)###0.9685714

mean(recall_snow3) ### 0.NaN
mean(recall_rain3) ### 0.8287083




###****NOTES ON RESULTS****###

#As you can see accuracy of LDA>LR>QDA>KNN , we should opt for LDA

#As you can see Precision of Snow , LDA gives highest
#As you can see Precision of rain , LDA gives highest, outperforms LR,KNN and QDA
#As you can see Precision of rainthunderstorm , LDA gives highest, outperforms LR, KNN and QDA

#As you can see Recall of Snow , LDA gives highest
#As you can see Recall of rain , LDA gives highest, outperforms LR, KNN and QDA
#As you can see Recall of rainthunderstorm , LDA gives highest, outperforms LR,KNN and QDA

###***So based on results, we choose LDA model on this dataset****###
