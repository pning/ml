mushrooms = read.csv("data/mushrooms.data",stringsAsFactors = TRUE,head=F)
str(mushrooms)
dim(mushrooms)
names(mushrooms)=c("type",
                   "cap-shape",
                   "cap-surface",
                   "cap-color",
                   "bruises",
                   "odor",
                   "gill-attachment",
                   "gill-spacing",
                   "gill-size",
                   "gill-color",               
                   "stalk-shape",              
                   "stalk-root",            
                   "stalk-surface-above-ring",
                   "stalk-surface-below-ring",
                   "stalk-color-above-ring",
                   "stalk-color-below-ring",
                   "veil-type",
                   "veil-color",            
                   "ring-number",           
                   "ring-type",           
                   "spore-print-color",   
                   "population",
                   "habitat")
summary(mushrooms)
table(mushrooms$type)
mushrooms$`veil-type`=NULL
dim(mushrooms)

#install.packages("RWeka")
require(RWeka)

mushroom_1R=OneR(type~.,data = mushrooms)
mushroom_1R
summary(mushroom_1R)

mushroom_JRip=JRip(type~.,data=mushrooms)
mushroom_JRip
summary(mushroom_JRip)

require(caret)
set.seed(2014)
inTranin=createDataPartition(y=mushrooms$type,p=0.8,list=FALSE)
mushrooms_train=mushrooms[inTranin,]
mushrooms_test=mushrooms[-inTranin,]
mushroom_JRip=JRip(type~.,data = mushrooms_train)
mushroom_predict=predict(mushroom_JRip,mushrooms_test)
require(gmodels)
CrossTable(mushrooms_test$type,mushroom_predict)