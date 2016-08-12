letters=read.csv("data/letter-recognition.data")
colnames(letters)=c("letter","x-box","y-box","width","high","onpix","x-bar","y-bar","x2bar","y2bar","xybar",
                    "x2ybr","xy2br","x-ege","xegvy","y-ege","yegvx")
str(letters)
dim(letters)
require(caret)
set.seed(2014)
inTrain=createDataPartition(y=letters$letter,p=0.8,list=FALSE)
letters_train=letters[inTrain,]
letters_test=letters[-inTrain,]
#install.packages("kernlab")
require(kernlab)
letter_classifier=ksvm(letter~.,data=letters_train,kernel="vanilladot")
letter_classifier
letter_predicitions =predict(letter_classifier,letters_test)
table(letter_predicitions,letters_test$letter)
agreement=letter_predicitions==letters_test$letter
table(agreement)
prop.table(table(agreement))
letter_classifier_rbf=ksvm(letter~.,data=letters_train,kernel="rbfdot")
letter_predicitions_rbf =predict(letter_classifier_rbf,letters_test)
agreement_rbf=letter_predicitions_rbf==letters_test$letter
prop.table(table(agreement_rbf))

set.seed(1)
x=matrix(rnorm(20*2),ncol = 2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))
dat=data.frame(x=x,y=as.factor(y))
install.packages("e1071")
library(e1071)
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index
summary(svmfit)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges = 
                list(cost=c(0.001,0.001,0.1,1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

set.seed(1)
x=matrix(rnorm(200*2),ncol = 2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)
train=sample(200,100)
svmfit=svm(y~.,data = dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newx=dat[-train,]))
#install.packages("ROCR")
#install.packages("gplots")
require(gplots)
require(ROCR)
rocplot=function (pred,truth , ...){
  predob = prediction (pred,truth)
  perf = performance (predob,"tpr","fpr")
  plot(perf , ...)
}
svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",
               gamma=2,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
