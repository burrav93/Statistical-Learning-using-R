#import the dataset and make some changes
library(readr)
kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")

kc_weather_srt=kc_weather_srt[,2:9]






#first make the response column to 0-snow, 1-rain and 2-rain_thunderstorm
#install.packages("plyr")
library(plyr)
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Snow"=1))
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Rain"=0))
kc_weather_srt$Events <- revalue(kc_weather_srt$Events,c("Rain_Thunderstorm"=2))

#small changes to Events column , making it to numeric from character
kc_weather_srt$Events<-as.numeric(as.character(kc_weather_srt$Events))

#replications
rep=100



# newly added


accuracy1=dim(rep)



precision_snow1=dim(rep)
precision_rain1=dim(rep)
precision_rainThunderstorm1=dim(rep)

recall_snow1=dim(rep)
recall_rain1=dim(rep)
recall_rainThunderstorm1=dim(rep)





#splitting the dataset into training and test sets, also install caTools packages
#install.packages('caTools')
library(caTools)
set.seed(123)

for(k in 1:rep)
{
  
  
  split=sample.split(kc_weather_srt$Events,SplitRatio = 0.7923)
  training_set=subset(kc_weather_srt,split==TRUE)
  test_set=subset(kc_weather_srt,split==FALSE)
  
  
  
  
#*****SVM*****#
  
  #fitting SVM to the training set
  #install.packages('e1071')
  library(e1071)
  classifier=svm(Events~.,data=training_set,type='C-classification',kernel="kernel")
  

  y_pred1=predict(classifier,newdata = test_set[-8])
 # y_pred1<-as.numeric(y_pred1)
  #making the confusion matrix
  
  cm1=table(test_set$Events,y_pred1)
  
  accuracy1[k]=mean(y_pred1==test_set$Events) 
  
  
  precision1=precision1<-diag(cm1)/colSums(cm1)
  precision_rainThunderstorm1[k]=precision1[3]
  precision_snow1[k]=precision1[2]
  precision_rain1[k]=precision1[1]
  
  
  recall1=recall1<-diag(cm1/rowSums(cm1))
  recall_rainThunderstorm1[k]=recall1[3]
  recall_snow1[k]=recall1[2]
  recall_rain1[k]=recall1[1]
  

}




