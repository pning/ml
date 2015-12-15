wdbc <- read.csv("data/wdbc.data",header = F)
wdbc.names <- c("radius","texture","perimeter","area","smoothness","compactness","concavity","concave points","symmetry","fractal dimension")
wdbc.names <- c(wdbc.names,paste(wdbc.names,"_mean",sep = ""),paste(wdbc.names,"_worst",sep = "")) 
names(wdbc) <- c("id","diagnosis",wdbc.names)

wdbc$diagnosis <- factor(wdbc$diagnosis,levels = c("B","M"),labels = c("Bengin","Malignant"))
round(prop.table(table(wdbc$diagnosis))*100,digits = 1)
wdbc=wdbc[-1]
summary(wdbc[c("radius_mean","texture_mean","perimeter_mean")])
# 0-1 区间
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

wdbc_n <- as.data.frame(lapply(wdbc[2:31], normalize))
summary(wdbc_n[c("radius_mean","texture_mean","perimeter_mean")])

wdbc_train <- wdbc_n[1:469,]
wdbc_test <- wdbc_n[470:569,]
wdbc_train_label <- wdbc[1:469,1]
wdbc_test_label <- wdbc[470:569,1]
mal_rate <- table(wdbc_train_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)

set.seed(2014)
inTranin <- sample(1:dim(wdbc_n)[1],469,replace = F)
wdbc_train <- wdbc_n[inTranin,]
wdbc_test <- wdbc_n[-inTranin,]
wdbc_train_label <- wdbc[inTranin,1]
wdbc_test_label <- wdbc[-inTranin,1]
mal_rate <- table(wdbc_train_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)

#install.packages("caret")
require(caret)
set.seed(2014)
inTranin <- createDataPartition(y=wdbc$diagnosis,p=0.8,list = FALSE)
wdbc_train <- wdbc_n[inTranin,]
wdbc_test <- wdbc_n[-inTranin,]
wdbc_train_label <- wdbc[inTranin,1]
wdbc_test_label <- wdbc[-inTranin,1]
mal_rate <- table(wdbc_train_label)
round(mal_rate[2]/sum(mal_rate),digits = 2)


sqrt(length(wdbc_train_label))
require(class)
wdbc_test_pred <- knn(train = wdbc_train,test =wdbc_test,cl=wdbc_train_label,k=22)

install.packages("gmodels")
require(gmodels)
CrossTable (x=wdbc_test_label,y=wdbc_test_pred,prop.chisq=FALSE)

#理解 
TN <- 71
TP <- 39
FN <- 3
FP <- 0
accuracy <- (TN+TP)/113
sensitivity <- TP/(TP+FN)
specificity <- TN/(TN+FP)
accuracy
sensitivity
specificity

# http://archive.ics.uci.edu/ml/


