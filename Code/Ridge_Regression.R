install.packages('glmnet')
library(glmnet)

#===== Read data
getwd()
setwd("/Users/yuetongliu/Desktop")
dat <- read.table("ridge.data")

#1===== Linear regression
lmobj <- lm(lpsa~.,data=dat)
summary(lmobj)

#===== Ridge regression using glmnet
X <- as.matrix(dat[,-9]); y <- dat[,9] #Create X matrix and y vector
lam <- exp(seq(-3,3,by=0.2)) #Specify sequence of penalty parameters (lambda)
ridgeobj <- glmnet(X,y,alpha=0,lambda=lam)
#plot(ridgeobj,xvar="lambda") #See how the coefficients "shrink" as penalty increases
str(ridgeobj)
log(ridgeobj$lambda) #1st entry has lambda equal to exp(3), 16th entry exp(0)
#3
ridgeobj$beta[,1] #coefficients when lambda=exp(3)
ridgeobj$beta[,16] #coefficients when lambda=exp(0)

#Alternatively...
coef(ridgeobj,s=exp(3))
coef(ridgeobj,s=exp(0))
#But make sure you know what "coef" does when you give it a value of lambda not in the
#supplied list in ridgeobj

#Try redoing with centred X, y and fitting glmnet without an intercept to see if you
#get the same coefficients.

#4===== 5-fold CV on ridge regression for choosing optimal penalty
set.seed(800)
cvridgeobj <- cv.glmnet(X,y,alpha=0,lambda=lam,nfolds=5)
str(cvridgeobj)
plot(cvridgeobj)
#5
cvridgeobj$lambda.min; min(cvridgeobj$cvm) #Location of and MSE at minimum

#6====== What are the coefficients for the ridge regression with optimal penalty?
coef(cvridgeobj,s=cvridgeobj$lambda.min)

#Alternatively...
wheremin <- which(cvridgeobj$cvm==min(cvridgeobj$cvm))
ridgeobj$beta[,wheremin]
