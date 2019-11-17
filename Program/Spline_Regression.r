library(splines)
#?MASS::mcycle
setwd("/Users/yuetongliu/Desktop")

#Read training/test set
dat.tr <- read.table("training.txt",header=T)
dat.te <- read.table("test.txt",header=T)
plot(logratio~range,dat=dat.tr)

#===== Fitting polynomial spline using B-spline basis
k <- 5; kn1 <- as.numeric(quantile(dat.tr$range,(1:k)/(k+1)))
k <- 10; kn2 <- as.numeric(quantile(dat.tr$range,(1:k)/(k+1)))
k <- 20; kn3 <- as.numeric(quantile(dat.tr$range,(1:k)/(k+1)))

lmobj1 <- lm(logratio~bs(range,knots=kn1),data=dat.tr)
lmobj2 <- lm(logratio~bs(range,knots=kn2),data=dat.tr)
lmobj3 <- lm(logratio~bs(range,knots=kn3),data=dat.tr)

#===== Showing the fits on a plot
newx <- seq(min(dat.tr$range),max(dat.tr$range),length=200)
preds1 <- predict(lmobj1,newdata=list(range=newx))
preds2 <- predict(lmobj2,newdata=list(range=newx))
preds3 <- predict(lmobj3,newdata=list(range=newx))

plot(logratio~range,dat=dat.tr)
lines(newx,preds1,lwd=3,col="blue")
lines(newx,preds2,lwd=3,col="red")
lines(newx,preds3,lwd=3,col="maroon")

#===== Prediction on the test set
pr.te1 <- predict(lmobj1,newdata=dat.te)
pr.te2 <- predict(lmobj2,newdata=dat.te)
pr.te3 <- predict(lmobj3,newdata=dat.te)
#6
mean((pr.te1-dat.te$logratio)^2)
mean((pr.te2-dat.te$logratio)^2)
mean((pr.te3-dat.te$logratio)^2)

#===== "edf"
#If you don't trust the solution...
X1 <- model.matrix(lmobj1); hat1 <- X1%*%solve(t(X1)%*%X1,t(X1))
X2 <- model.matrix(lmobj2); hat2 <- X2%*%solve(t(X2)%*%X2,t(X2))
X3 <- model.matrix(lmobj3); hat3 <- X3%*%solve(t(X3)%*%X3,t(X3))
sum(diag(hat1))
sum(diag(hat2))
sum(diag(hat3))
