
#consider all predictors as quantitative and use Naive Bayes Classifier and install package (e2071)

#import the dataset and make some changes
library(readr)
library(naivebayes)
library(e2071)

kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")
#consider only 3 predictors - Temp.F,Humidity.%,Precip.in.
kc_weather_srt=kc_weather_srt[,c(2,4,8,9)]

head(kc_weather_srt) # to check the new data consisting of only three predictors



head(kc_weather_srt)



#replications
rep=100



# newly added

#snow=1 rain=0 thunderstorm=2
accuracy=dim(rep)


precision_snow=dim(rep)
precision_rain=dim(rep)
precision_rainThunderstorm=dim(rep)

recall_snow=dim(rep)
recall_rain=dim(rep)
recall_rainThunderstorm=dim(rep)


#splitting the dataset into training and test sets, also install caTools packages
#install.packages('caTools')
library(caTools)
set.seed(123)

for(k in 1:rep)
{
  
  
  split=sample.split(kc_weather_srt$Events,SplitRatio = 0.7923)
  training_set=subset(kc_weather_srt,split==TRUE)
  test_set=subset(kc_weather_srt,split==FALSE)
  
  
  
  
  
  #install.packages('e1071')
  library(e1071)
  
  
  
  Nb=naiveBayes(formula=Events~.,data=training_set)
  summary(Nb)
  #predicting the test set results
  y_pred=predict(Nb,newdata=test_set[-4])
  #making the confusion matrix
  cm=table(y_pred,test_set[,4])
  
  accuracy[k]=mean(y_pred==test_set[,4]) 
  
  
  precision=precision<-diag(cm)/colSums(cm)
  precision_rainThunderstorm[k]=precision[3]
  precision_snow[k]=precision[2]
  precision_rain[k]=precision[1]
  
  
  recall=recall<-diag(cm/rowSums(cm))
  recall_rainThunderstorm[k]=recall[3]
  recall_snow[k]=recall[2]
  recall_rain[k]=recall[1]
  
}


mean(accuracy) ##0.7507895

mean(precision_snow)##0.73620689
mean(precision_rain)##0.7064865
mean(precision_rainThunderstorm)##0.957

mean(recall_snow)##0.7498165
mean(recall_rain)##0.766421
mean(recall_rainThunderstorm)##0.7367071

