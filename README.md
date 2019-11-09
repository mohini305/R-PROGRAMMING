# R-PROGRAMMING
---
title: "FARE PREDICTION"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
setwd("C:/Users/mohin/OneDrive/Desktop/MSIS Spring 2019/DM & PA/Excels")
df<-read.csv("C:/Users/mohin/OneDrive/Desktop/MSIS Spring 2019/DM & PA/Excels/Individual Assignment 1 Airline Data v2.csv")
df
fit1 <- lm(FARE~COUPON+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX+SLOT+VACATION+SW+HI, data=df)
fit1
summary(fit1)
fit1 <- lm(FARE~COUPON+NEW+VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX+SLOT+GATE, data=df)
fit1
summary(fit1)
newdata<-data.frame(COUPON = 1, NEW = 3, VACATION = c("No"), SW = c("No"), HI =6000, S_INCOME = 2000, E_INCOME = 2000, S_POP = 4000000, E_POP=7150000, SLOT=c("Free"),GATE =c("Constrained"), DISTANCE = 1000,PAX = 6000)
 p<-predict.lm(fit1,newdata)
 p
 pairs((df[,sapply(df,is.numeric)]),pch=20)
      plot(df$DISTANCE,df$FARE, col=df$SW, pch=21)
 round(cor(df[,unlist(lapply(df, is.numeric))],use="complete.ob?s"),2)
 fit2<-lm(FARE~SW,data=df)
 summary(fit2)
 fit3<-lm(FARE~COUPON+NEW+VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX+SLOT+GATE+VACATION*SW,data = df)
 summary(fit3)
 fit5<-lm(FARE~VACATION+SW+HI+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX,data=df)
 summary(fit5)
 fit6<-lm(FARE~VACATION+SW+HI+S_POP+E_POP+DISTANCE+PAX,data=df)
summary(fit6)
install.packages("ggplot2");
install.packages("lattice")
library(ggplot2)
df<-read.csv("C:/Users/mohin/OneDrive/Desktop/MSIS Spring 2019/DM & PA/Excels/Individual Assignment 1 Airline Data v2.csv")
fit1 <- lm(FARE~COUPON+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX+SLOT+VACATION+SW+HI, data=df)
ggplot(fit1,aes(x=FARE,y=DISTANCE),stat=stat)+geom_point(color=red)
df2<-read.csv("C:/Users/mohin/OneDrive/Desktop/MSIS Spring 2019/DM & PA/Excels/Framingham_Full_Data.csv")
df2
bx<-boxplot(df2$sysBP,df2$diaBP,df2$totChol,levels(df2$TenYearCHD))
bx<-boxplot(df2$sysBP,df2$diaBP,df2$totChol,subset=df2$TenYearCHD)
library(caTools)
set.seed(123)
split<-sample.split(df2,SplitRatio = 0.7)
train<-subset(df2,split=="TRUE")
test<-subset(df2,split=="FALSE")
mymodel<-glm(df2)
pred<-predict(mymodel,test,type="response")
pred
prop.table(table(train$male))
table(train$age)
typeof(df2$currentSmoker)
lpm<-lm(df2$TenYearCHD~df2$male+df2$age+df2$education+df2$currentSmoker+df2$cigsPerDay+df2$BPMeds+df2$prevalentStroke+df2$prevalentHyp+df2$diabetes+df2$totChol+df2$sysBP+df2$diaBP+df2$BMI+df2$heartRate+df2$glucose,data=df2)
summary(lpm)
hist(df2$age)
summary(df2$age)
summary(df2)
df2$TenYearCHD[df2$TenYearCHD == 1] <- "Disease"
df2$TenYearCHD[df2$TenYearCHD == 0] <- "No Disease"
library(magrittr)
library(Metrics)
install.packages("plotly")
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(knitr)
library(mctest)

plot_ly(df2, y = ~sysBP, color = ~TenYearCHD, type = "box")
plot_ly(df2, y = ~diaBP, color = ~TenYearCHD, type = "box")
plot_ly(df2, y = ~totChol, color = ~TenYearCHD, type = "box")


```
