library(MASS)
setwd("/Users/yuetongliu/Desktop")
dat <- read.csv("Iris.txt",header=T)
x1 <- dat[,1]; x2 <- dat[,2]; y <- dat[,3] #y=0: versicolor; y=1: virginica
#===== Plot the data
plot(x1,x2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))

#===== Fit linear discriminant and make predictions
ldaobj <- lda(y~x1+x2)
# Predict at a grid of values spanning the range of covariates
xx1 <- seq(1,9,length=200)
xx2 <- seq(1,9,length=200)
dd <- expand.grid(xx1,xx2)
colnames(dd) <- c('x1','x2')
zz <- predict(ldaobj, newdata=dd)$posterior[,1] #Check the help manual (not "?predict")
                                                #for the list of returned values
#2 Plot contours
contour(xx1,xx2,matrix(zz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='maroon')

#3 ===== Two new points
predict(ldaobj, newdata=list(x1=4.9,x2=5.3))
predict(ldaobj, newdata=list(x1=5.9,x2=4.8))

#===== Let's change a few points...
newx1 <- x1; newx2 <- x2
newx1[68:69] <- c(3,3); newx2[68:69] <- c(7.5,7.7)
ldaobj2 <- lda(y~newx1+newx2)
newzz <- predict(ldaobj2, newdata=list(newx1=dd[,1],newx2=dd[,2]))$posterior[,1]
par(mfrow=c(1,2))
plot(x1,x2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))
contour(xx1,xx2,matrix(zz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='maroon')
plot(newx1,newx2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))
contour(xx1,xx2,matrix(newzz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='grey50')


#4===== Code to make the plots in lab worksheet
par(mfrow=c(1,1),mar=c(3,3,0.5,0.5),mgp=c(2,1,0))
plot(x1,x2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))

plot(newx1,newx2,pch=c(19,1)[y+1],cex=1.2,xlab="Sepal",ylab="Petal",xlim=c(3,8),ylim=c(3,8))
contour(xx1,xx2,matrix(newzz,200,200),levels=.5,add=TRUE,drawlabels=FALSE,lwd=4,col='grey50')
points(c(7.7,7.7),c(6.7,6.9),pch=22,bg='grey50',cex=0.5)
arrows(7.7,6.7,3.1,7.5,length=0.1)
arrows(7.7,6.9,3.1,7.7,length=0.1)

