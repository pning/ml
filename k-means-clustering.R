teens=read.csv("data/snsdata.csv")
dim(teens)
str(teens)
table(teens$gender)
table(teens$gender,useNA = "ifany")
summary(teens$age)
teens$age=ifelse(teens$age>=13 & teens$age<20,teens$age,NA)
summary(teens$age)
teens$female=ifelse(teens$gender=="r"&is.na(teens$gender),1,0)
teens$no_gender=ifelse(is.na(teens$gender),1,0)

table(teens$gender,useNA="ifany")
prop.table(table(teens$gender,useNA = "ifany"))
table(teens$female,useNA = "ifany")
table(teens$no_gender,useNA = "ifany")

mean(teens$age,na.rm=TRUE)
aggregate(data=teens,age~gradyear,mean,na.rm=TRUE)

ave_age=ave(teens$age,teens$gradyear,FUN=function(x) mean(x,na.rm = TRUE))
teens$age=ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

interesets=teens[5:40]
interesets_z=as.data.frame(lapply(interesets, scale))
teens_clusters=kmeans(interesets_z,5)

teens_clusters$size
teens_clusters$centers

teens$cluster=teens_clusters$cluster
teens[1:5,c("cluster","gender","age","friends")]
aggregate(data=teens,age~cluster,mean)
aggregate(data=teens,female~cluster,mean)
aggregate(data=teens,friends~cluster,mean)
