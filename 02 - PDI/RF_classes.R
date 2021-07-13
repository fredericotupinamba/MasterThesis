
#SET R-------------------------------------

library(randomForest)

data<-read.csv(".csv", sep=";")

# Random Forests para cuatro categorias---------
library(randomForest)

set.seed(1234)
RF <- randomForest(Health.status ~. ,data, importance=TRUE, na.action=na.omit,nodesize=1)
plot(RF)
print(RF)
summary(data)

varImpPlot(RF_1,sort=TRUE, n.var=min(25, nrow(RF$importance)),
           type=NULL, class=NULL, scale=TRUE,main="",cex=0.6)



