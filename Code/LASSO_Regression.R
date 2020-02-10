library(glmnet)

#===== Read data
dat <- read.table("prostate_mod.data")

#===== LASSO using glmnet
X <- as.matrix(dat[,-9]); y <- dat[,9] #Create X matrix and y vector
lassoobj <- glmnet(X,y,alpha=1)
plot(lassoobj,label=T)

#===== 5-fold CV on LASSO
set.seed(800)
cvlassoobj <- cv.glmnet(X,y,alpha=1,nfolds=5)
plot(cvlassoobj$glmnet.fit, "norm", label=TRUE)
plot(cvlassoobj$glmnet.fit, "lambda", label=TRUE)
cvlassoobj$lambda.min
coef(cvlassoobj,s=cvlassoobj$lambda.min)

#Alternatively...
cvlassoobj$nzero[which(cvlassoobj$cvm==min(cvlassoobj$cvm))]

#===== EDF of ridge regression with optimal lambda [exp(-1.2)]
xm <- scale(X,scale=F)
xm.svd <- svd(xm)
sum( xm.svd$d^2 / (xm.svd$d^2 + exp(-1.2)) )

#Alternatively...
sum(diag( xm %*% solve( t(xm)%*%xm + exp(-1.2)*diag(ncol(xm)) , t(xm) ) ))
