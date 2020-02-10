getwd()
setwd("/Users/yuetongliu/Desktop")
dat = read.table("cv.data",header=T)
n <- nrow(dat)

#===== Goodness of fit: full vs reduced
#1
cor(dat) #Find most correlated variables - lcavol, svi, lcp
#2
mod1 <- lm(lpsa~.,data=dat)
mod2 <- lm(lpsa~lcavol+svi+lcp,data=dat)
mean(resid(mod1)^2)
mean(resid(mod2)^2)

#===== 3-fold CV
#3
set.seed(400)
gpind <- sample(rep(1:3,each=32))
pe1 <- pe2 <- rep(NA,3)
#Group 1
x.tr <- dat[gpind!=1,]; 
x.te <- dat[gpind==1,]
cor(x.tr) #Find most correlated variables - lcavol, lweight, svi
mod1 <- lm(lpsa~.,data=x.tr); mod2 <- lm(lpsa~lcavol+lweight+svi,data=x.tr)
pe1[1] <- sum((x.te$lpsa-predict(mod1,newdata=x.te))^2)
pe2[1] <- sum((x.te$lpsa-predict(mod2,newdata=x.te))^2)
pe1[1]/32
pe2[1]/32
#Group 2
x.tr <- dat[gpind!=2,]; 
x.te <- dat[gpind==2,]
cor(x.tr) #Find most correlated variables - lcavol, svi, lcp
mod1 <- lm(lpsa~.,data=x.tr); mod2 <- lm(lpsa~lcavol+svi+lcp,data=x.tr)
pe1[2] <- sum((x.te$lpsa-predict(mod1,newdata=x.te))^2)
pe2[2] <- sum((x.te$lpsa-predict(mod2,newdata=x.te))^2)

#Group 3
x.tr <- dat[gpind!=3,]; x.te <- dat[gpind==3,]
cor(x.tr) #Find most correlated variables - lcavol, svi, lcp
mod1 <- lm(lpsa~.,data=x.tr); mod2 <- lm(lpsa~lcavol+svi+lcp,data=x.tr)
pe1[3] <- sum((x.te$lpsa-predict(mod1,newdata=x.te))^2)
pe2[3] <- sum((x.te$lpsa-predict(mod2,newdata=x.te))^2)
#5
sum(pe1)/n
sum(pe2)/n

# Challenge: Rewrite the CV procedure using a for loop
