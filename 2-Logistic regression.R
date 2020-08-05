rm(list=ls())
setwd("/Users/shelleyxiang/Documents/MBA last spring term 第三学期/Marketing Big Data")
ccard=read.csv("https://www.dropbox.com/s/4epwkmp9855w6e7/creditcard.csv?dl=1")
head(ccard)
str(ccard)
#create train and test dataset  
set.seed(321)
rv=sample(1:nrow(ccard),round(nrow(ccard)*.7,0))
trainData=ccard[rv,]#create training dataset 
testData=ccard[-rv,]#create training dataset 

#random guess

p=sum(trainData$Class=="Fraud")/nrow(trainData)#a naive guess Fraud probability based on percentage
p
rg.pred <- sample(c("NonFraud", "Fraud"), nrow(trainData), prob=c(1-p,p),replace = TRUE)#random guess prediction
str(rg.pred)
rg.pred =as.factor(rg.pred)
summary(rg.pred)
mean(rg.pred==trainData$Class)# random guess around 


rg.pred2 <- as.factor(sample(c("NonFraud", "Fraud"), nrow(trainData), prob=c(0.9,0.1),replace = TRUE))

mean(rg.pred2==trainData$Class)

### Part 2: Logistic regression model -------

names(trainData)

logit.fit <- glm(Class ~.,trainData, family = binomial())
summary(logit.fit)
# significant variables: V3, V4, V7, V8, V9, V10, V11, V12, V14, V16, V17, V18, V19, V21 V2,V5,V6,V15,V22,V27,V28)

logit.fit<-glm(Class~V3+V4+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21+V2+V5+V6+V15+V22+V27+V28,family='binomial',trainData)
summary(logit.fit)

# Predict fraud for the test set based on the trained model
logit.predProb <- predict.glm(logit.fit, testData, type = "response") # probability values of Credit Fraud

threshold=0.5
logit.pred <- as.factor(ifelse(logit.predProb>=threshold,"NonFraud","Fraud"))
mean(logit.pred ==testData$Class)#the result 
table(logit.pred,testData$Class)

#threshold=0.4
logit.pred <- as.factor(ifelse(logit.predProb >=threshold,"NonFraud","Fraud"))
mean(logit.pred ==testData$Class)#the result is lower
table(logit.pred,testData$Class)

str(testData$Class)
#threshold=0.9
logit.pred <- as.factor(ifelse(logit.predProb >=threshold,"NonFraud","Fraud"))
mean(logit.pred ==testData$Class)#the result is lower
table(logit.pred,testData$Class)


#for confusion matrix
library(caret)
confusionMatrix(logit.pred,testData$Class,positive ="Fraud")
levels(testData$Class)
# F1 Score
install.packages("MLmetrics")
library(MLmetrics)
F1_Score(y_pred=logit.pred,y_true=testData$Class,positive="Fraud")

# Plot ROC curve using ROCR package
install.packages("ROCR")
library(ROCR)
predLogit.roc <-ROCR::prediction(logit.predProb, testData$Class)
perfLogit.roc <- performance(predLogit.roc, "tpr", "fpr")
plot(perfLogit.roc,col="green")

#compute auc 
logit.auc<- performance(predLogit.roc,  c("auc"))
logit.auc@y.values[1]