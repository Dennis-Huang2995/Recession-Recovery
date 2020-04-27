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
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # Copy and paste your FREDR key.

getwd() # Identify the working directory.
setwd("/Schack Spring Real Estate Data Science")


data = read.csv('VIXCLS.csv')
names = c("date", "VIX", "yieldcurve", "recession", "consumer", "unemployment", "Tedrate")
colnames(data) = names

cor(data$yieldcurve, data$unemployment)
cor(data$yieldcurve, data$VIX)


data$yieldcurve = data$yieldcurve * 100
data$Tedrate = data$Tedrate * 100
data$unemployment = data$unemployment * 10
summary(data)

data$yieldcurve.l1 = lag(data$yieldcurve, 1)


camharvey1 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX, data=data)
stargazer(camharvey1, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability1 = predict(camharvey1, newdata = data, type = "response")


camharvey2 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment , data=data)
stargazer(camharvey2, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability2 = predict(camharvey2, newdata = data, type = "response")


camharvey3 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate, data=data )
stargazer(camharvey3, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability3 = predict(camharvey3, newdata = data, type = "response")


camharvey4 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + consumer, data=data )
stargazer(camharvey4, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability4 = predict(camharvey4, newdata = data, type = "response")


camharvey5 = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate + consumer, data=data )
stargazer(camharvey5, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability5 = predict(camharvey5, newdata = data, type = "response")


camharvey6 = glm(recession ~ yieldcurve + yieldcurve.l1 + VIX + unemployment + Tedrate + consumer, data=data )
stargazer(camharvey6, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
data$probability6 = predict(camharvey6, newdata = data, type = "response")


# The Confusion Matrix and the Accuracy of Prediction
cm = confusion.matrix(data$recession, data$probability6, threshold = 0.4)
cm
tpr = cm[4] / (cm[4] + cm[3])
fpr = cm[2] / (cm[2] + cm[1])
acc = (cm[1] + cm[4]) / (cm[1] + cm[2] + cm[3] + cm[4])



tpr
fpr
acc



# For ease there is syntax to calculate directly.
accuracy(data$recession, data$probability6, threshold = 0.4)
accuracy(data$recession, data$probability6, threshold = 0.5)





data = read.csv('VIXCLS.csv')
names = c("date", "VIX", "yieldcurve", "recession", "consumer", "unemployment", "Tedrate")
colnames(data) = names


data$yieldcurve = data$yieldcurve * 100
data$Tedrate = data$Tedrate * 100
data$unemployment = data$unemployment * 10
summary(data)

data$yieldcurve.l1 = lag(data$yieldcurve, 1)



set.seed(04272020)
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


# Time to unify this framework in the ARIMA structure.
# AR: Autoregressive
# I: Integrated (Random Walks)
# MA: Moving Averages
# The ARIMA Structure.

# White noise: ARIMA(0, 0, 0)
# Autoregression: ARIMA(1, 0, 0) 
# Random walk: ARIMA(0, 1, 0) 
# Moving Average: ARIMA(0, 0, 1) 


arima = Arima(data$VIX, order=c(1, 0, 1))
summary(arima)
plot(forecast(arima, h=8), include=360, col="blue")
grid(lw=2)
abline(h=19, lw=2)
abline(h=mean(data$VIX), lw=2)

arima = Arima(data$yieldcurve, order=c(1, 0, 1))
summary(arima)
plot(forecast(arima, h=8), include=360, col="blue")
grid(lw=2)
abline(h=156, lw=2)
abline(h=mean(data$yieldcurve), lw=2)






volatility = drop_na(fredr(series_id = "VIXCLS", observation_start = as.Date("1990-01-01")))

plot(volatility$date, volatility$value, col = 'blue', pch=16, 
     ylab = "Index", xlab = "Date", main="VIX")
grid(lw=2)
lines(volatility$date, volatility$value, col='blue')
grid(lw=2)
abline(h=0, col="black")
abline(h=26, col="red")
abline(h=14, col="green")

set.seed(12345611)
nstates = 2
mod = depmix(response = value ~ 1, data = volatility, nstates = nstates, trstart = runif(nstates^2))
fm = fit(mod, emc=em.control(random.start = TRUE))
summary(fm)

post_probs = posterior(fm)
matplot(post_probs$S1, type='l', col='red', main='Transition Probabilities', ylab='Probability')

