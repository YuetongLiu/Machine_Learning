library(rpart)
setwd("/Users/yuetongliu/Desktop")
dat <- read.csv("Iris.txt",header=T)
x1 <- dat[,1]; x2 <- dat[,2]; y <- dat[,3] #y=0: versicolor; y=1: virginica
#===== Plot the data
plot(x1,x2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))

#1===== Fit a classification tree (and draw it)
myc <- rpart.control(minsplit=3,cp=1e-8)
set.seed(800)
treeobj <- rpart(y~sepal+petal,data=dat,method='class',control=myc)
plot(treeobj,uniform=T,margin=0.1)
text(treeobj,use.n=T)

#3===== Two new points
predict(treeobj,newdata=list(sepal=4.9,petal=5.3),type="class")
predict(treeobj,newdata=list(sepal=5.9,petal=4.8),type="class")

#5===== Which observations are misclassified?
diffobs <- which(dat$y!=predict(treeobj,type="vector")-1) #Be careful what you're doing here...
dat[diffobs,]

#6===== Cross validation results for various complexity parameters (CP)
printcp(treeobj)
