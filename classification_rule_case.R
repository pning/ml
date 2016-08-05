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

