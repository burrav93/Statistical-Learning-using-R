#import the dataset and make some changes
library(readr)
kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")

kc_weather_srt=kc_weather_srt[,2:9] #selecting all the predictors and response column

#subset that consists of only rain and snow
kc_weather_srt_without_rainthunderstorm<-kc_weather_srt[!grepl("Rain_Thunderstorm",kc_weather_srt$Events),]

dim(kc_weather_srt_without_rainthunderstorm) #dimensions 





#first make the response column to 0 and 1 (qualitative)
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
prob_pred=predict(model1,type='response',newdata = test_set[-8])   #for predicitng we only need predictors , but not response
summary(prob_pred)
y_pred=ifelse(prob_pred>0.5,1,0) #vector of predictions
y_pred

#Making the confusion Matrix
cm=table(y_pred,test_set[,8])       
cm

#accuracy
accuracy[k]=mean(y_pred==test_set[,8]) 
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
cm1=table(y_pred1,test_set[,8])
cm1
accuracy1[k]=mean(y_pred1==test_set[,8]) 

precision1=precison1<-diag(cm1/colSums(cm1))
precision_snow1[k]=precison1[2]
precision_rain1[k]=precision1[1]
recall1=recall1<-diag(cm1/rowSums(cm1))
recall_snow1[k]=recall1[2]
recall_rain1[k]=recall1[1]












1



#***QDA***#
qda=qda(formula=Events~.,data=training_set)
y_pred2=predict(qda,test_set)$class
cm2=table(y_pred2,test_set[,8])
cm2
accuracy2[k]=mean(y_pred2==test_set[,8]) 

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
y_pred3=knn(train=training_set[,-8],test=test_set[,-8],cl=training_set[,8],k=5)

#making the cm
cm3=table(y_pred3,test_set[,8])
cm3
accuracy3[k]=mean(y_pred3==test_set[,8])

precision3=precision3<-diag(cm3/colSums(cm3))
precision_snow3[k]=precision3[2]
precision_rain3[k]=precision3[1]

recall3=recall3<-recall3<-diag(cm3/rowSums(cm3))
recall_snow3[k]=recall3[2]
recall_rain3[k]=recall3[1]
}


###*******MEAN VALUES*****#####

#****Logestic regression***#
mean(accuracy)###0.9531111
mean(precision_snow)###0.889
mean(precision_rain)###0.9714286

mean(recall_snow) ### 0.9075805
mean(recall_rain) ### 0.9688803



#****LDA***#
mean(accuracy1)###0.936222
mean(precision_snow1)###0.865
mean(precision_rain1)###0.9565714

mean(recall_snow1) ### 0.8595878
mean(recall_rain1) ### 0.9618026








#****QDA***#
mean(accuracy2)###0.9346667
mean(precision_snow2)###0.952
mean(precision_rain2)###0.9297143

mean(recall_snow2) ### 0.8043408
mean(recall_rain2) ### 0.985939



#****KNN***#
mean(accuracy3)###0.9486667
mean(precision_snow3)###0.878
mean(precision_rain3)###0.9688571

mean(recall_snow3) ### 0.89958469
mean(recall_rain3) ### 0.9658621





###****NOTES ON RESULTS****###       *******change results**********

#As you can see accuracy of LR>KNN>LDA>QDA , we should opt for Logestic Regression

#As you can see Precision of Snow , QDA>LR>KNN>LDA 
#As you can see Precision of rain , LR>KNN>LDA>QDA


#As you can see Recall of Snow , LR>KNN>LDA>QDA
#As you can see Recall of rain , LR>KNN>LDA>QDA

###decision boundary is not very highly non-linear in this case, so we see Logestic Regression dominating KNN and LDA.
###QDA serves as a compromise between KNN , LDA and LR, In thus case there are more number of training observations, so QDA doesnt perform well


###***So based on results, Logestic regression performs better in accuracy, precision and recall, so we choose Logestic Regression model on this dataset****###
