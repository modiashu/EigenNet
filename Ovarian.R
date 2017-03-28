library(glmnet)
library(pROC)
library("ROCR")

rm(list=ls())
Leukemia <- read.csv("C:/Users/Ashutosh/Dropbox/1/Ovarian.csv")

X = as.matrix(Leukemia[,1:15154])
Y = Leukemia[,15155]
Ycategorical = as.numeric(Y=='Cancer')
Ycategorical = t(Ycategorical)
Ycategorical = t(Ycategorical)
##Split into train and test
set.seed(2)
split <- sample(nrow(X), floor(0.5*nrow(X)))

Train <- X[split,]
TrainLabel <- Ycategorical[split,]

Test <- X[-split,]
TestLabel <- Ycategorical[-split,]


##Use CV
CV = cv.glmnet(x=Train, y=TrainLabel, family='binomial', type.measure = "class", alpha=0.5, nlambda=100)
plot(CV)
#CV$lambda.1se

## Create a Model
fit = glmnet(x=Train, y=TrainLabel, family='binomial', alpha=0.5, lambda=CV$lambda.1se)
plot(fit,xvar="lambda",label=TRUE)
#fit$beta[,1]   Beta values

##Predict unseen data
predictions<-predict.glmnet(fit,Test,s=CV$lambda.1se)
TestLabel = t(TestLabel)
TestLabel = t(TestLabel)
auc = roc(TestLabel,predictions)
auc

##Find AUC value

#plot(fit,xvar="lambda",label=TRUE)






cl <- rainbow(11)

j=0
for(i in seq(0,1,0.1)){
  ##Use CV
  CV = cv.glmnet(x=Train, y=TrainLabel, family='binomial', type.measure = "class", alpha=i, nlambda=100)
  #plot(CV)
  #CV$lambda.1se
  
  ## Create a Model
  fit = glmnet(x=Train, y=TrainLabel, family='binomial', alpha=i, lambda=CV$lambda.1se)
  #plot(fit,xvar="lambda",label=TRUE)
  #fit$beta[,1]   Beta values
  
  ##Predict unseen data
  predictions<-predict.glmnet(fit,Test,s=CV$lambda.1se)
  TestLabel = t(TestLabel)
  TestLabel = t(TestLabel)
  auc = roc(TestLabel,predictions)
  
  #plot(roc(TestLabel,predictions), legacy.axes = TRUE, add=TRUE)
  auc
  
  ##Find AUC value
  
  j = j+1
  pred <- prediction(predictions, TestLabel)
  perf <- performance(pred,"tpr","fpr")
  if(i==0)
  {plot(perf,lty=3, lwd=3,col=cl[j])}
  else{plot(perf,lty=3, lwd=3,add=TRUE, col=cl[j])}
  
}

