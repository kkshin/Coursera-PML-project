PML project
========================================================


setwd('/Users/kkshin/Documents/classes/Coursera/Practical Machine Learning/project')

library(gdata)
library(caret)
library(e1071)
library(randomForest)

<!-- read data -->
fitData<-read.csv('pml-training.csv')
testing<-read.csv('pml-testing.csv')

<!-- selecting columns with 'accel' but without 'var' -->
myvar<-matchcols(fitData,with='accel',without='var')
mydata<-fitData[c(myvar,'classe')]
testdata<-testing[myvar]

<!-- Tuning the random forest to find the best parameter mtry with the smallest OOB error -->
fitRF<-tuneRF(mydata[myvar],mydata$classe,stepFactor=1.5,ntreeTry=500,doBest=TRUE)

<!-- Fit the best random forest model and decide the variable importance -->
bestfit<-randomForest(classe~.,data=mydata,mtry=4,ntree=500,keep.forest=TRUE,importance=TRUE)
print(bestfit)
importance(bestfit,type=1)
varImpPlot(bestfit,type=1)

<!-- Using random forest cross validation to see if we could possibly reduce the number of predictors -->
featurefit<-rfcv(mydata[myvar],mydata$classe,ntree=500,cv.fold=5)
with(featurefit,plot(n.var,error.cv,log='x',type='o',lwd=2))

<!-- predict the testing set -->
pred<-predict(bestfit, testdata)

<!-- write the answer in txt -->
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred)
