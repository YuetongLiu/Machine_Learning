library(mclust)
setwd("/Users/yuetongliu/Desktop")
#===== K-means clustering and plot within cluster SS against K
dat <- read.csv("Iris.txt",header=T)
kmobj <- vector('list',10); ss <- rep(0,10)
for(i in 1:10){
	kmobj[[i]] <- kmeans(dat,centers=i,nstart=50)
	ss[i] <- sum(kmobj[[i]]$within)
}
#1
ss[2]
#2
plot(1:10,ss,xlab='K',ylab='W_k',type='b',lwd=2,pch=19)

#===== Plot the clusters for each K
origpar <- par() #Save original par settings
par(mfrow=c(3,3),mar=c(4,3.5,2,0.5),mgp=c(2.5,1,0))
for(i in 2:10) plot(dat,col=kmobj[[i]]$cluster,pch=19,xlab="Sepal",ylab="Petal",main=paste("K =",i))
par(origpar) #Restore original par settings (you can ignore the warnings)

#4===== Model-based clustering
mcobj <- Mclust(dat)
summary(mcobj)

#===== Are these clusters the same as for K-means with K=2?
table(abs(mcobj$classification-kmobj[[2]]$cluster))

#Alternatively...
par(mfrow=c(1,2))
plot(dat,col=kmobj[[2]]$cluster,pch=19,xlab="Sepal",ylab="Petal",main="2-means")
plot(dat,col=mcobj$classification,pch=19,xlab="Sepal",ylab="Petal",main="Model-based")
#4 points in a different cluster

