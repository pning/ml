install.packages("recommenderlab")
require(recommenderlab)
data(MovieLense)
as(MovieLense[1,],"list")[[1]][1:10]
image(MovieLense[1:100,1:100])
hist(rowCounts(MovieLense))
hist(colCounts(MovieLense))
mean(rowMeans(MovieLense))
recommenderRegistry$get_entries(dataType="realRatingMatrix")
ml.recommdModel=Recommender(MovieLense[1:800],method="IBCF")
ml.predictl=predict(ml.recommdModel,MovieLense[805:807],n=5)
ml.predictl
as(ml.predictl,"list")
ml.predictl2=predict(ml.recommdModel,MovieLense[805:807],type="ratings")
ml.predictl2
as(ml.predictl2,"matrix")[1:3,1:6]

model.eval=evaluationScheme(MovieLense[1:943],method="split",train=0.9,given=15,goodRating=5)
model.random=Recommender(getData(model.eval,"train"),method="RANDOM")
model.ubcf=Recommender(getData(model.eval,"train"),method="UBCF")
model.ibcf=Recommender(getData(model.eval,"train"),method="IBCF")

predict.random=predict(model.random,getData(model.eval,"known"),type="ratings")
predict.ubcf=predict(model.ubcf,getData(model.eval,"known"),type="ratings")
predict.ibcf=predict(model.ibcf,getData(model.eval,"known"),type="ratings")

error=rbind(
  calcPredictionAccuracy(predict.random,getData(model.eval,"unknown")),
  calcPredictionAccuracy(predict.ubcf,getData(model.eval,"unknown")),
  calcPredictionAccuracy(predict.ibcf,getData(model.eval,"unknown")))
rownames(error)=c("RANDOM","UBCF","IBCF")
error