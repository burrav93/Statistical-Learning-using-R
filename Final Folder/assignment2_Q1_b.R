#import the dataset and make some changes
library(readr)
kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")

kc_weather_srt=kc_weather_srt[,2:9]






#first make the response column to 0-snow, 1-rain and 2-rain_thunderstorm
install.packages("plyr")
library(plyr)
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Snow"=1))
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Rain"=0))
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Rain_Thunderstorm"=2))

#small changes to Events column , making it to numeric from character
kc_weather_srt$Events<-as.numeric(as.character(kc_weather_srt$Events))

#replications
rep=100



# newly added

#snow=1 rain=0 thunderstorm=2
accuracy1=dim(rep)
accuracy2=dim(rep)
accuracy3=dim(rep)


precision_snow1=dim(rep)
precision_rain1=dim(rep)
precision_rainThunderstorm1=dim(rep)

recall_snow1=dim(rep)
recall_rain1=dim(rep)
recall_rainThunderstorm1=dim(rep)

precision_snow2=dim(rep)
precision_rain2=dim(rep)
precision_rainThunderstorm2=dim(rep)
recall_snow2=dim(rep)
recall_rain2=dim(rep)
recall_rainThunderstorm2=dim(rep)

precision_snow3=dim(rep)
precision_rain3=dim(rep)
precision_rainThunderstorm3=dim(rep)
recall_snow3=dim(rep)
recall_rain3=dim(rep)
recall_rainThunderstorm3=dim(rep)



#splitting the dataset into training and test sets, also install caTools packages
install.packages('caTools')
library(caTools)
set.seed(123)

for(k in 1:rep)
{
  

split=sample.split(kc_weather_srt$Events,SplitRatio = 0.7923)
training_set=subset(kc_weather_srt,split==TRUE)
test_set=subset(kc_weather_srt,split==FALSE)



#***LDA***#
#install.packages("MASS")
library(MASS)
lda=lda(formula=Events~.,data=training_set)
training_set1=as.data.frame(predict(lda,training_set))
training_set1=training_set1[c(4,1)]
plot(lda)

test_set1=as.data.frame(predict(lda,test_set))
test_set1=test_set1[c(4,1)]

#fitting SVM to the training set
#install.packages('e1071')
library(e1071)
classifier=svm(formula=class~.,data=training_set1,type='C-classification',kernel='linear')
#predicting the test set results
y_pred1=predict(classifier,newdata = test_set1[-2])
#making the confusion matrix
cm1=table(y_pred1,test_set1[,2])

accuracy1[k]=mean(y_pred1==test_set1[,2]) 


precision1=precision1<-diag(cm1)/colSums(cm1)
precision_rainThunderstorm1[k]=precision1[3]
precision_snow1[k]=precision1[2]
precision_rain1[k]=precision1[1]


recall1=recall1<-diag(cm1/rowSums(cm1))
recall_rainThunderstorm1[k]=recall1[3]
recall_snow1[k]=recall1[2]
recall_rain1[k]=recall1[1]




#***QDA***#
qda=qda(formula=Events~.,data=training_set)
y_pred2=predict(qda,test_set)$class
cm2=table(y_pred2,test_set[,8])

accuracy2[k]=mean(y_pred2==test_set[,8]) 

precision2=precison2<-diag(cm2/colSums(cm2))
precision_rainThunderstorm2[k]=precison2[3]
precision_snow2[k]=precison2[2]
precision_rain2[k]=precision2[1]
recall2=recall2<-diag(cm2/rowSums(cm2))
recall_rainThunderstorm2[k]=recall2[3]
recall_snow2[k]=recall2[2]
recall_rain2[k]=recall2[1]




#***KNN***#
#install.packages('class')
#fitting KNN to the training set and predicting the test set results
library(class)
y_pred3=knn(train=training_set[,-8],test=test_set[,-8],cl=training_set[,8],k=5)

#making the cm
cm3=table(y_pred3,test_set[,8])

accuracy3[k]=mean(y_pred3==test_set[,8])

precision3=precision3<-diag(cm3/colSums(cm3))
precision_rainThunderstorm3[k]=precision3[3]
precision_snow3[k]=precision3[2]
precision_rain3[k]=precision3[1]

recall3=recall3<-recall3<-diag(cm3/rowSums(cm3))
recall_rainThunderstorm3[k]=recall3[3]
recall_snow3[k]=recall3[2]
recall_rain3[k]=recall3[1]
}

###*******MEAN VALUES*****#####


#****LDA***#
mean(accuracy1)###0.9026316
mean(precision_snow1)###0.6407459
mean(precision_rain1)###0.9115871
mean(precision_rainThunderstorm1)###0.9906168
mean(recall_snow1) ### 0.911675
mean(recall_rain1) ### 0.902705
mean(recall_rainThunderstorm1) ### 0.9906152







#****QDA***#
mean(accuracy2)###0.7389474
mean(precision_snow2)###0.945
mean(precision_rain2)###0.697027
mean(precision_rainThunderstorm2)###0.7213793
mean(recall_snow2) ### 0.7950919
mean(recall_rain2) ### 0.7514844
mean(recall_rainThunderstorm2) ### 0.7115056


#****KNN***#
mean(accuracy3)###0.745
mean(precision_snow3)###0.895
mean(precision_rain3)###0.7305405
mean(precision_rainThunderstorm3)###0.7117241
mean(recall_snow3) ### 0.9098042
mean(recall_rain3) ### 0.7444542
mean(recall_rainThunderstorm3) ### 0.701065



###****NOTES ON RESULTS****###

#As you can see accuracy of LDA>KNN>QDA , we should opt for LDA

#As you can see Precision of Snow , LDA gives highest
#As you can see Precision of rain , LDA gives highest, outperforms KNN and QDA
#As you can see Precision of rainthunderstorm , LDA gives highest, outperforms KNN and QDA

#As you can see Recall of Snow , LDA gives highest
#As you can see Recall of rain , LDA gives highest, outperforms KNN and QDA
#As you can see Recall of rainthunderstorm , LDA gives highest, outperforms KNN and QDA

###***So based on results, we choose LDA model on this dataset****###