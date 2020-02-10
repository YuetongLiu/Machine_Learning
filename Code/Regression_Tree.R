# Loading the necessary libraries
library(rpart)
library(glmnet)

# Load the data
setwd("/Users/yuetongliu/Desktop")
load("Lab 5 data")
summary(car.data)

# ---------------------------
# Q1-Q4
# ---------------------------

#2 Grow tree 
fit <- rpart(Mileage ~ Price + Country + Reliability + Type, 
             method="anova", data=car.data, 
             control=rpart.control(minsplit=5, cp=0.01)) # Change minsplit here for Q4

# Detailed summary of splits (optional)
summary(fit)

#2 Plot of tree (optional)
plot(fit, uniform=TRUE, main="Regression Tree for Mileage")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Predictions and MSE computation
pr.fit <- predict(fit, newdata=car.data)
mse.tree <- mean((car.data$Mileage - pr.fit)^2); mse.tree

# Plot the pruned tree 
#4
control1=rpart.control(minsplit=20, cp=0.01)
data.to1 <- rpart(Mileage ~., data=car.data, method='anova',
                  control=control1)
plot(data.to1, compress=TRUE)
pr.to1 <- predict(data.to1, newdata=car.data, type='vector')
with(car.data, mean((Mileage - pr.to1)^2) )

# ---------------------------
# CV Procedure
# ---------------------------

# Setting the seed and generating the folds
set.seed(400)
gpind <- sample(rep(1:7, each=7))

# Variable to store the MSPEs
mspe.tree <- numeric(3)

# (!) Minimum number of observations for each split (change here)
minsplit.tree <- 20 # Try 5 later

# CV over the folds
for(fold in 1:7){
  
  # Splitting data into training and testing sets
  data.tr <- car.data[gpind!=fold,]
  data.te <- car.data[gpind==fold,]
  
  # Fit and pruning
  fit <- rpart(Mileage ~ Price + Country + Reliability + Type, 
               method="anova", data=data.tr,
               control=rpart.control(minsplit=minsplit.tree, cp=0.01))

  # Predictions and MSPE computation
  pr.fit <- predict(fit, newdata=data.te)
  mspe.tree[fold] <- mean((data.te$Mileage - pr.fit)^2)
}

#5 Mean, minimum and maximum of MSPEs
mean(mspe.tree)
min(mspe.tree)
max(mspe.tree)



