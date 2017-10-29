
#consider Temp.F as quantitative , use package e2071 and use Naive Bayes as classifier

#import the dataset and make some changes
library(readr)
library(naivebayes)
library(e2071)

kc_weather_srt <- read_csv("C:/Users/bvkka/Desktop/ISL-Deep Medhi/kc_weather_srt.csv")
#consider only 3 predictors - Temp.F,Humidity.%,Precip.in.
kc_weather_srt=kc_weather_srt[,c(2,4,8,9)]

head(kc_weather_srt) # to check the new data consisting of only three predictors


###*****categorization of three predictors****###

mean(kc_weather_srt$Temp.F) ## we see 58.74044
mean(kc_weather_srt$Humidity.percentage)## we see 69.85246
mean(kc_weather_srt$Precip.in)##we see 0.1728415

##Now, categorize the temp.F based on mean of all the values that is values >58.74 as 1 and <58.74 as 0


kc_weather_srt$Humidity.percentage=ifelse(kc_weather_srt$Humidity.percentage>69.85246,1,0)
kc_weather_srt$Precip.in=ifelse(kc_weather_srt$Precip.in>0.1728415,1,0)


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


mean(accuracy) ##0.6923684

mean(precision_snow)##0.7682759
mean(precision_rain)##0.5497292
mean(precision_rainThunderstorm)##1

mean(recall_snow)##0.7787254
mean(recall_rain)##0.7723869
mean(recall_rainThunderstorm)##0.4878522

