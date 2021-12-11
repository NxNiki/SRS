rm(list=ls())
setwd("~/Dropbox/Nancy_Zoe")

#library(glmnet)
library(gglasso)
library(grplasso)
#library(xlsx)
library(foreign)
data=read.spss("MultiSyndrome_Zoes_Dataset_Pruned_UpdatedWSIQ_Aug2018_DEID.sav", to.data.frame=TRUE)

names(data)
dim(data)
head(data)
#DV:  DX_NUM, IV:SRS_Adj01,..., SRS_Adj65, Covariates: Sex, SRSAge
ind<-c(2,3,6,14:78,9)
data2 <- data[, ind]

ind_asd <- which(data2$DX_NUM == "ASD")
ind_ds <- which(data2$DX_NUM == "DS")
ind_sms <- which(data2$DX_NUM == "SMS")
ind_ws <- which(data2$DX_NUM == "WS")
ind_1x <- which(data2$DX_NUM == "+1X")
ind_23x <- which(data2$DX_NUM == "+2/3X")
ind_1x1y <- which(data2$DX_NUM == "+1X+1Y")
ind_1y <- which(data2$DX_NUM == "+1Y")

length(ind_asd)
length(ind_ds)
length(ind_sms)
length(ind_ws)
length(ind_1x)
length(ind_23x)
length(ind_1x1y)
length(ind_1y)

data3 <- data2[c(ind_asd, ind_ds, ind_sms, ind_ws, ind_1x, ind_23x, ind_1x1y, ind_1y), ]
nsize <- dim(data3)[1]
y <- matrix(0, nsize, 1)

######   define different groups  #####
y[which(data3$DX_NUM == "+1Y")]=1

data4 <- cbind(data3, y)

library(caret)
set.seed(31)
ind <- createDataPartition(data4$y, p=0.75)
data.train <- data4[ind$Resample1, ]
data.test <- data4[-ind$Resample1, ]
table(data.train$y)
table(data.test$y)

#install.packages("ROSE")
library(ROSE)
# #Use function ovun.sample() to generate balanced data
data1.imp.bal<-ovun.sample(y ~ ., data=data.train, method="both",p=0.5, seed=1342)$data

y.train<-data1.imp.bal$y
y.train<-as.matrix(y.train)
x.train<-data1.imp.bal[, 1:67]
x.train = x.train[, -3]
x.train$Sex <- as.numeric(x.train$Sex)
x.train<-as.matrix(x.train)

y.test<-data.test$y
y.test<-as.matrix(y.test)
x.test<-data.test[, 1:67]
x.test = x.test[, -3]
x.test$Sex <- as.numeric(x.test$Sex)
x.test<-as.matrix(x.test)
	
# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)

# group lasso:

column.idx = 1:ncol(x.train)+2
group = rep(6, 1, ncol(x.train))

awareness.idx = c(2, 7, 25, 32, 45, 52, 54, 56)
cognition.idx = c(5, 10, 15, 17, 30, 40, 42, 44, 48, 58, 59, 62)
communication.idx = c(12, 13, 16, 18, 19, 21, 22, 26, 33, 35:38, 41, 46, 47, 51, 53, 55, 57, 60, 61)
motivation.idx = c(1, 3, 6, 9, 11, 23, 27, 34, 43, 64, 65)
Restricted.idx = c(4, 8, 14, 20, 24, 28, 29, 31, 39, 49, 50, 63)
group[column.idx %in% awareness.idx] = 1
group[column.idx %in% cognition.idx] = 2
group[column.idx %in% communication.idx] = 3
group[column.idx %in% motivation.idx] = 4
group[column.idx %in% Restricted.idx] = 5

print(x.train)
cv.fit =  cv.gglasso(x.train, y.train, nfolds=5, group = group)
#cv.fit =  grplasso(x.train, y.train, index = group, lambda = 1e-2)
print(cv.fit)
yhat = predict(cv.fit, s=cv.fit$lambda.min, newx=x.test,type="class")

myacc<-function(ypred,ytrue,thr1){
  ypred0=rep(1, length(ypred))
  ypred0[which(ypred >= thr1 )]=2
  temp<-table(ypred0,ytrue)
  speci<-temp[1,1]/(temp[1,1]+temp[2,1])
  sensi<-temp[2,2]/(temp[1,2]+temp[2,2])
  acc<-(temp[1,1]+temp[2,2])/length(ypred)
  return(c(acc,sensi,speci))
}
# acc         sensi           speci 
# 0.5 for SMS, 0.5 for ASD,  0.5for +1X, 0.5 for WS , 0.5 for +1x+1Y, 0.5 for +1Y with p =0.75
#, 0.5 for DS,0.5 for +2/3X ,  with p=0.8
thr1=0.5
myacc(yhat, y.test,  thr1)


ally<-data4$y
ally<-as.matrix(ally)
allx<-data4[, 1:67]
allx$Sex <- as.numeric(allx$Sex)
allx<-as.matrix(allx)

# ind=7 for ASD,  ind=10 for +1X, ind=5 for WS
# ind=0 for SMS, ind=9 for DS, ind=0 for +2/3X, ind=5 for +1X+1Y, ind=8 for +1Y
ind=8
set.seed(1)
cv.out<-cv.glmnet(allx, ally, type.measure="class", nfolds=4, alpha=ind/10, family="binomial")
plot(cv.out)
coefs<-coef(cv.out, s="lambda.min")
coefs<-as.matrix(coefs)
round(coefs,3)

ind=8
cv.out <- cv.glmnet(x.train, y.train, type.measure="class", nfolds=4, alpha=ind/10, family="binomial")
coefs<-coef(cv.out, s="lambda.min")
coefs<-as.matrix(coefs)
round(coefs,3)




