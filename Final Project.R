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
library(forecast)
library(depmixS4)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # Copy and paste your FREDR key.

getwd() # Identify the working directory.
setwd("/Schack Spring Real Estate Data Science")


VIX = read.csv('VIXCLS.csv')
names = c("date", "VIX", "yieldcurve", "recession")
colnames(VIX) = names


recession = fredr('USREC')
yieldcurve = fredr('T10Y3MM')

data = merge(recession, yieldcurve, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve")
colnames(data) = names


VIX$yieldcurve = VIX$yieldcurve * 100
summary(VIX)

VIX$yieldcurve.l1 = lag(VIX$yieldcurve, 1)

camharvey = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX, data=VIX)
stargazer(camharvey, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

camharvey = lm(recession ~ yieldcurve + yieldcurve.l1, data=VIX)
stargazer(camharvey, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
