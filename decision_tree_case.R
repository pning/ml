#install.packages("rpart")
#install.packages("partykit")
#install.packages("rpart.plot")

require(rpart)
require(partykit)
require(rpart.plot)

require(caret)
set.seed(2014)
inTrain=createDataPartition(y=iris$Species,p=0.8,list=FALSE)
iristrain=iris[inTrain,]
iritest=iris[-inTrain,]

treemodel <- rpart(Species~.,data=iristrain)
summary(treemodel)
plot(treemodel)
text(treemodel)

prp(treemodel)
prp(treemodel,varlen = 5)

prediction <- predict(treemodel,newdata = iritest,type='class')
table(prediction,iritest$Species)

credit=read.table("data/german.data",header=F,sep=" ",stringsAsFactors = F)
str(credit)
dim(credit)
colnames(credit)=c("Status of existing checking account",
                   "Duration in month",
                   "Credit history",
                   "Purpose",
                   "Credit amount",
                   "Savings account/bonds",
                   "Present employment since",
                   "Installment rate in percentage of disposable income",
                   "Personal status and sex",
                   "Other debtors / guarantors",
                   "Present residence since",
                   "Property",
                   "Age in years",
                   "Other installment plans",
                   "Housing",
                   "Number of existing credits at this bank",
                   "Job",
                   "Number of people being liable to provide maintenance for",
                   "Telephone",
                   "foreign worker",
                   "Good.Loan")
mapping = list("A11"=" ... <    0 DM",
               "A12"="0 <= ... <  200 DM",
               "A13"="... >= 200 DM",
               "A30"="no credits taken/all credits paid back duly",
               "A31"="all credits at this bank paid back duly",
               "A32"="all credits at this bank paid back duly",
               "A33"="all credits at this bank paid back duly",
               "A34"="critical account/other credits existing (not at this bank)",
               "A40"= "car (new)",
               "A41"= "car (used)",
               "A42"=  "furniture/equipment",
               "A43"= "radio/television",
               "A44" = "domestic appliances",
               "A45" = "repairs",
               "A46"= "education",
               "A47" = "(vacation - does not exist?)",
               "A48" = "retraining",
               "A49"= "business",
               "A410" = "others",
               "A61" ="... <  100 DM",
               "A62"= "100 <= ... <  500 DM",
               "A63"= "500 <= ... < 1000 DM",
               "A64"= ".. >= 1000 DM",
               "A65"= "unknown/ no savings account",
               "A71"= "unemployed",
               "A72"= "... < 1 year",
               "A73"= "1  <= ... < 4 years",
               "A74"= "4  <= ... < 7 years",
               "A75"= ".. >= 7 years",
               "A91"= "male: divorced/separated",
               "A92"= "female : divorced/separated/married",
               "A93"= "male: single",
               "A94"= "male: married/widowed",
               "A95"= "female: single",
               "A101"= "none",
               "A102"= "co-applicant",
               "A103"= "guarantor",
               "A121"= "real estate",
               "A122"= "if not A121 : building society savings agreement/life insurance",
               "A123"= "if not A121/A122 : car or other, not in attribute 6",
               "A124"= "unknown / no property",
               "A141"= "bank",
               "A142"= "stores",
               "A143"= "none",
               "A151"= "rent",
               "A152"= "own",
               "A153"= "for free",
               "A171"= "unemployed/ unskilled  - non-resident",
               "A172"= "unskilled - resident",
               "A173"= "skilled employee / official",
               "A174"= "management/ self-employed/ighly qualified employee/ officer",
               "A191"= "none",
               "A192"= "yes, registered under the customers name",
               "A201"= "yes",
               "A202"= "no")
for(i in 1:(dim(credit))[2]){
  if(class(credit[,i])=='character'){
    credit[,i]=as.factor(as.character(mapping[credit[,i]]))
  }
}
credit$Good.Loan=as.factor(ifelse(credit$Good.Loan==1,'GoodLoan','BadLoan'))

str(credit)

table(credit$`Status of existing checking account`)
summary(credit$`Duration in month`)
summary(credit$Good.Loan)

require(caret)
set.seed(2014)
inTrain=createDataPartition(y=credit$Good.Loan,p=0.8,list=FALSE)
credit_train=credit[inTrain,]
credit_test=credit[-inTrain,]
prop.table(table(credit_train$Good.Loan))
prop.table(table(credit_test$Good.Loan))

#install.packages("C50")
require(C50)
credit_model=C5.0(credit_train[-21],credit_train$Good.Loan,trials = 20)
credit_model
summary(credit_model)

credit_pred=predict(credit_model,credit_test)
require(gmodels)
CrossTable(credit_test$Good.Loan,credit_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn = c('actual loan status','predicted loan status'))

error_cost=matrix(c(0,1,5,0),nrow=2)
rownames(error_cost)=c("GoodLoan","BadLoan")
colnames(error_cost)=c("GoodLoan","BadLoan")
credit_cost=C5.0(credit_train[-21],credit_test$Good.Loan,costs = error_cost)
credit_cost_pred=predict(credit_cost,credit_test)
require(gmodels)
CrossTable(credit_test$Good.Loan,credit_cost_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn = c('actual loan status','predicted loan status'))


require(caret)
folds=createFolds(credit$Good.Loan,k=10)
str(folds)
credit_train=credit[-folds$Fold01,]
credit_test=credit[folds$Fold01,]


