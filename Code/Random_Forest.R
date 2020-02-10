library(randomForest)
setwd("/Users/yuetongliu/Desktop")
dat.tr <- read.csv("training1.txt",header=T)
dat.test <- read.csv("test1.txt",header=T)

#===== Random forest & variable importance
set.seed(200)
rf3 <- randomForest(V14~.,data=dat.tr,ntree=1501); 
#1，4
rf3
par(mfrow=c(1,2)); 
#2
plot(rf3); 
#3
varImpPlot(rf3)

#5===== Bagging
set.seed(200)
rf13 <- randomForest(V14~.,data=dat.tr,ntree=1501,mtry=13); rf13

#7===== Predict on test set
mean(dat.test$V14!=predict(rf3,newdata=dat.test[,1:13],type="response"))

#8===== How many trees do not contain the 3rd observation?
rf3$oob.times[3]; rf3$votes[3,]
rf3$oob.times[3]*rf3$votes[3,]
