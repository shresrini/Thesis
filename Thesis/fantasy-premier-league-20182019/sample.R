data= read.csv("Train.csv")

head(data)
ncol(data)
summary(data)
data=data[4:21]

data=na.omit(data)

library(caTools)

split=sample.split(data$Points,SplitRatio = 0.80)
train=subset(data,split==TRUE)
test=subset(data,split=FALSE)
train=na.omit(train)


Model= lm(formula=Points~., data = train)
summary(Model)


pred= predict(Model,test[,18] )
