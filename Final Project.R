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



VIX = drop_na(fredr("VIXCLS", observation_start = as.Date("1982-01-01")))
recession = fredr('USREC')
yieldcurve = fredr('T10Y3MM')

data = merge(recession, yieldcurve, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve")
colnames(data) = names
data1 = merge(data, VIX, by.x='date', by.y='date')
data1 = subset(data1, select = c(date,recession, yieldcurve, value))
names = c("date", "recession", "yieldcurve","VIX")
colnames(data1) = names

data1$yieldcurve = data1$yieldcurve * 100
summary(data1)

data1$yieldcurve.l1 = lag(data1$yieldcurve, 1)

plot(data1$date, data1$yieldcurve, col="darkblue", main="U.S. Yield Curve (10-Year Minus 3-Month)", pch=16, 
     xlab="Date", ylab="Basis Points", ylim=c(-100, 450))
lines(data1$date, data1$yieldcurve, col="darkblue")
grid(lw=2)
abline(h=0, col="red")
abline(h=mean(data1$yieldcurve), col='darkgreen')

camharvey = lm(recession ~ yieldcurve + yieldcurve.l1 + VIX, data=data1)
stargazer(camharvey, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

