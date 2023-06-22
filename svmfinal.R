library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
library(e1071)
library(tm)
#library(RTextTools)
library(dplyr)


X<-read.csv("C:\\Users\\SIDDHARTH\\Desktop\\abhirk.csv")
X
tm<- vector(mode="numeric", length=10)
acu <- vector(mode="numeric", length=10)
num<-nrow(X)
y<-vector(mode="numeric", length=10)
for(i in 1:10){
  y[i]=i;
}

 dat<-X[1:1000,]
 dat
  a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close)+day+month+year,data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[1]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[1]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close)+day+month,data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)

acu[2]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[2]<-b-a

dat<-X[1:800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[3]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[3]<-b-a

dat<-X[1:2100,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[4]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[4]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[5]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[5]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)

acu[6]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[6]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[7]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[7]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[8]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[8]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[9]<-conf.mat$overall['Accuracy']      
b<-Sys.time();

tm[9]<-b-a

dat<-X[1:1800,]
a<-Sys.time();

table(dat$class)

id<- sample(2,nrow(dat),prob=c(0.8,0.2),replace=T)

traindat<-dat[id==1,]

testat<-dat[id==2,]


xxx<-svm(class~volume+high+low+close+open+(high-low)+(open-low)+(high-close),data=traindat)


pre3<-predict(xxx,newdata = testat)

conf.mat<-confusionMatrix(pre3,testat$class)
conf.mat
acu[10]<-conf.mat$overall['Accuracy']  
b<-Sys.time();

tm[10]<-b-a
tm
acu


library(googleVis)

df1=data.frame(y,tm)
df1<-df1[1,]
names(df1)=c("Number","Time")
mychart1<-gvisLineChart(df1,options=list(title="TimeSeries",width=1000,height=600,vAxis="{title:'Time'}",hAxis="{title:'Number of iterations'}"))
plot(mychart1)

df2=data.frame(y,acu)
names(df2)=c("Number","Accuracy")
mychart2<-gvisLineChart(df2,options=list(title="Accuracy",width=1000,height=600,vAxis="{title:'Accuracy'}",hAxis="{title:'Number of iterations'}")) 
plot(mychart2)
