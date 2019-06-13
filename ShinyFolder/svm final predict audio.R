library(tuneR)
library(audio)
library(seewave)
#install.packages("wrassp")
library(wrassp)
#install.packages("phonTools")
library(phonTools)
library(stringr)

vec1 <- NULL





mfcc.test1 <- read.csv("F:\\mid eval minor 2018\\testing_recorded_audio.csv")

mfcc.test1 <- mfcc.test1[,1:24]
mfcc.test1[,1:24] = (mfcc.test1[,1:24]- min(mfcc.test1[,1:24]))/(max(mfcc.test1[,1:24])-min(mfcc.test1[,1:24]))


library(e1071)

y_pred_linear_p = predict(classifier_linear, newdata = mfcc.test1)


# Predicting the Test set results
y_pred_polynomial_p = predict(classifier_polynomial, newdata = mfcc.test1)

y_pred_sigmoid_p = predict(classifier_sigmoid, newdata = mfcc.test1)

# Predicting the Test set results
y_pred_radial_p = predict(classifier_radial, newdata = mfcc.test1)






labelss =c('belly_pain', 'burping', 'cant_say','cold/hot','discomfort','hungry','lonely','scared','tired')

print(paste("predicted class  for linear is : ",labelss[y_pred_linear_p]))

print(paste("predicted class  for sigmoid is : ",labelss[y_pred_sigmoid_p]))



print(paste("predicted class  for radial is : ",labelss[y_pred_radial_p]))


print(paste("predicted class  for polynomial is : ",labelss[y_pred_polynomial_p]))



