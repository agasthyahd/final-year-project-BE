library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
library(e1071)
library(tm)
library(dplyr)


X<-read.csv("C:\\Users\\SIDDHARTH\\Desktop\\abhirk.csv")
X
tm <- vector(mode="numeric", length=10)
acu <- vector(mode="numeric", length=10)
num<-nrow(X)
y<-vector(mode="numeric", length=10)
for(i in 1:10){
  y[i]=i;
}


  df<-X[1:1000,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  df.train
  df.test
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low)+month+day+year, data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  conf.mat
  acu[1]<-conf.mat$overall['Accuracy']
  
  b<-Sys.time();
  tm[1]<-b-a  
  
  
  
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  conf.mat
  acu[2]<-conf.mat$overall['Accuracy']
  
  b<-Sys.time();
  tm[2]<-b-a    
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  conf.mat
  acu[3]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[3]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  conf.mat
  acu[4]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[4]<-b-a   
  
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  acu[5]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[5]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  acu[6]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[6]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  #df.test$class
  #df.train$class
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  acu[7]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[7]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  

  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
 conf.mat
  acu[8]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[8]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
 
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  
  conf.mat <- confusionMatrix(pred, df.test$class)
  acu[9]<-conf.mat$overall['Accuracy']
  b<-Sys.time();
  tm[9]<-b-a   
  df<-X[1:1500,]
  
  
  a<-Sys.time();
  df$class <- as.factor(df$class)
  
  intrain<-createDataPartition(df$class,p=0.8,list=FALSE)
  
  df.train<-df[intrain,]
  df.test<-df[-intrain,]
  
  model <- naiveBayes(class ~low+high+close+open+(high-low)+(high-open)+(close-low), data = df.train)
  pred<-predict(model, df.test)
  
  
  table("Predictions"= pred,  "Actual" = df.test$class )
  b<-Sys.time();
  tm[10]<-b-a  
  conf.mat <- confusionMatrix(pred, df.test$class)
  conf.mat
  acu[10]<-conf.mat$overall['Accuracy']
  
tm
acu


library(googleVis)

df1=data.frame(y,tm)
names(df1)=c("Number","Time")
mychart1<-gvisLineChart(df1,options=list(title="TimeSeries",width=1000,height=600,vAxis="{title:'Time'}",hAxis="{title:'Number of iterations'}"))
plot(mychart1)

df2=data.frame(y,acu)
names(df2)=c("Number","Accuracy")
mychart2<-gvisLineChart(df2,options=list(title="Accuracy",width=1000,height=600,vAxis="{title:'Accuracy'}",hAxis="{title:'Number of iterations'}")) 
plot(mychart2)
