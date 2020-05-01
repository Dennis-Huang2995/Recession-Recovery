library(quantmod)
library(stargazer)
library(fredr)
library(tidyr)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(haven)
library(margins)
library(caTools)
library(SDMTools)
library(randomForest)
library(forecast)
library(depmixS4)
fredr_set_key('7970af4f8e0f166e092c58667a2408c3') 

getwd() # Identify the working directory.
setwd("/Schack Spring Real Estate Data Science")


data = read.csv('RPDATA.csv')
names = c("date", "VIX", "yieldcurve", "recession", "consumer", "unemployment", "Tedrate")
colnames(data) = names

cor(data$yieldcurve, data$unemployment)
cor(data$yieldcurve, data$VIX)


data$yieldcurve = data$yieldcurve * 100
data$Tedrate = data$Tedrate * 100
data$unemployment = data$unemployment * 100
summary(data)

data$yieldcurve.l1 = lag(data$yieldcurve, 1)


RH1 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX, data=data)
stargazer(RH1, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability1 = predict(RH1, newdata = data, type = "response")


RH2 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment , data=data)
stargazer(RH2, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability2 = predict(RH2, newdata = data, type = "response")


RH3 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate, data=data )
stargazer(RH3, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability3 = predict(RH3, newdata = data, type = "response")


RH4 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + consumer, data=data )
stargazer(RH4, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability4 = predict(RH4, newdata = data, type = "response")


RH5 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate + consumer, data=data )
stargazer(RH5, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability5 = predict(RH5, newdata = data, type = "response")


RH6 = glm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate + consumer, data=data )
stargazer(RH6, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability6 = predict(RH6, newdata = data, type = "response")





# reread the dataset

data = read.csv('RPDATA.csv')
names = c("date", "VIX", "yieldcurve", "recession", "consumer", "unemployment", "Tedrate")
colnames(data) = names


data$yieldcurve = data$yieldcurve * 100
data$Tedrate = data$Tedrate * 100
data$unemployment = data$unemployment * 100
summary(data)

data$yieldcurve.l1 = lag(data$yieldcurve, 1)



set.seed(05012020)
sample = sample.split(data, SplitRatio = 0.80)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)



logit = glm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate + consumer, data=train)
stargazer(logit, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# Fit the logit on the training set.  Apply the fitted logit to the test set.
# Evaluation.
test$probability7 = predict(logit, newdata = test, type = "response")



# The Confusion Matrix and the Accuracy of Prediction
cm = confusion.matrix(test$recession, test$probability7, threshold = 0.4)
cm
tpr = cm[4] / (cm[4] + cm[3])
fpr = cm[2] / (cm[2] + cm[1])
acc = (cm[1] + cm[4]) / (cm[1] + cm[2] + cm[3] + cm[4])



tpr
fpr
acc
