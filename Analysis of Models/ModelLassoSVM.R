# Onion Analytics
# This R code uses Lasso and SVM on both method 1 (28 days ahead) 
# and method 0 (1 day ahead compounded, using predictions to predict further)
# It shows how much better does SVM do vs. Lasso
# Shows how useful is Method 1 vs. Method 0


#############################method 1#############################

#########################Support Vector machines#################

#setwd("C:/Users/Hassan/Desktop/Grad School/Sem 2/Business Analytics/Project")
setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')


# import data set
data1 = read.csv("DataEngineeredFinal.csv")
data = data1[,c(-1,-23,-24)] 



#Randomly divide data into two parts
set.seed(1)
train <- sample(1:nrow(data), 0.5*nrow(data))
not_train <- -train

#take a half as train, leave other half for test and validate 
data.train <- data[train,]
data.not_train <- data[not_train,]

#divide other half into two again to get train and validate
test <- sample(1:nrow(data.not_train), as.integer(0.5*nrow(data.not_train)))
validate  <- -test
data.test <- data.not_train[test,]
data.validate <- data.not_train[validate,]

#use CV to find best kernel type
tc <- tune.control(cross = 5)

kernels <- c('linear', 'polynomial', 'radial', 'sigmoid')
cv.errors <- c()
for(t in 1:length(kernels)){
  if (t == 1){
    fit <- tune.svm (PRICE ~., data = data.train, kernel = kernels[t],tunecontrol = tc,
                      cost = 10^(-3:1),epsilon = seq(0, 0.2, 0.02))
  }else if (t == 2){
    fit <- tune.svm (PRICE ~., data = data.train, kernel = kernels[t],
                      tunecontrol = tc,cost = 10^(-3:1),degree = c(4,5,6))
  }else {fit <- tune.svm (PRICE ~., data = data.train, kernel = kernels[t],
                           tunecontrol = tc,cost = 10^(1:3),gamma = seq(0, 1, by = .1),
                           epsilon = c(0, 0.01, 0.05, 0.1))
  }
  pred <- predict(fit$best.model, data.validate )
  cv.errors[t] <- mean((data.validate$PRICE-pred)^2)
}

#find which kernel performed best
t_star = which.min(cv.errors)
cat('The optimal kernel is', kernels[t_star])

#test best model on train + validate
data_final <- rbind(data.train, data.validate) 

#fit best model with best kernel
if (t_star == 1){
  final <- tune.svm(PRICE ~., data = data_final, kernel = kernels[t_star],tunecontrol = tc,
                    cost = 10^(-3:1),epsilon = seq(0, 0.2, 0.02))
}else if (t_star == 2){ 
  final <- tune.svm (PRICE ~., data = data_final, kernel = kernels[t_star],
                     tunecontrol = tc,cost = 10^(-3:1),degree = c(4,5,6))
}else{final <- tune.svm(PRICE ~., data = data_final, kernel = kernels[t_star],
                        tunecontrol = tc,cost = 10^(1:3), gamma = seq(0, 1, by = .1),
                        epsilon = c(0, 0.01, 0.05, 0.1))
}

#create plot
plot(data1[test,1], data1[test,2], pch=16, cex = 0.8)
predictedY <- predict(final$best.model, data1[test,2:ncol(data1)])
points(data1[test,1], predictedY, col = "blue", pch= 4, cex = 0.8)

e = data.frame(cbind(data1[test,1],data1[test,2],predictedY))
colnames(e) <- c('Index', 'Actual', 'Predicted')

p <- ggplot(e, aes(e$Index))+
  theme_bw()+
  scale_shape_identity()+
  scale_color_manual(labels = c("Actual", "Predicted"), 
                      values = c("deepskyblue", "darkblue"),
                      guide = guide_legend(override.aes = list(
                      shape = c(19, 4))))+
  geom_point(aes(y = e$Predicted, color = 'blue'), size = 2.5)+
  geom_point(aes(y = e$Actual, color = 'red', shape = 4), size = 2)+
  xlab("Index")+ 
  ylab("Price") +
  ggtitle("Accuracy of Prediction")+
  theme(legend.text = element_text(colour="black", size=10, face="bold"))+
  labs(color='')+
  theme(axis.title = element_text(colour="black", face="bold"))
p
    
#final MSE on training data
pred1 <- predict(final$best.model, data_final)
MSE <- mean((data_final$PRICE-pred1)^2)

#final MSE on test data
pred2 <- predict(final$best.model, data.test)
MSE1 <- mean((data.test$PRICE-pred2)^2)
print(MSE1)







############## Lasso Regression  #####################

#setwd("C:/Users/Hassan/Desktop/Grad School/Sem 2/Business Analytics/Project")
setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')


# import data set
data <- read.csv("DataEngineered_04_23_17_Method1.csv")[,c(-1,-23,-24)]
data1 <- read.csv("DataEngineered_04_23_17_Method1.csv")

#Randomly divide data into two parts
set.seed(1)
train <- sample(1:nrow(data), 0.5*nrow(data))
not_train <- -train

#take a half as train, leave other half for test and validate 
data.train <- data[train,]
data.not_train <- data[not_train,]

#divide other half into two again to get train and validate
test <- sample(1:nrow(data.not_train), as.integer(0.5*nrow(data.not_train)))
validate  <- -test
data.test <- data.not_train[test,]
data.validate <- data.not_train[validate,]


#test best model on train + validate
data_final <- rbind(data.train, data.validate) 

x <- model.matrix(PRICE~., data_final)[,-1]
y <- data_final$PRICE

#We wish to choose the best model using CV among
# lambda = 0, 0.001, 0.01, 0.1, 1, 10, 100, 1000 
grid = c(10^(-3:3),0)  #set sequence of lambdas we want to test

cv.out = cv.glmnet(x, y, alpha=1, lambda=grid, nfolds=5) 

# optimum lambda
bestlam = cv.out$lambda.min
cat('The optimum lambda is ',bestlam)

#Train model with best value of lambda on the training set
lasso.mod = glmnet(x, y, alpha=1, lambda=bestlam)

#coefficients of Lasso
lasso.coef = predict(lasso.mod, type = "coefficients", s=bestlam )
lasso.coef

#Evaluate this model on the test set
pred = predict(lasso.mod, x)
actual = y
MSE <- mean((actual-pred)^2) 
cat('The MSE of the model using training data is', MSE)

xnew <- model.matrix(PRICE~., data.test)[,-1]
ynew <- data.test$PRICE

#Evaluate this model on the test set
pred = predict(lasso.mod, xnew)
actual = ynew
MSE <- mean((actual-pred)^2) 
cat('The MSE of model using out of sample data', MSE)

predictedY <- predict(lasso.mod, xnew)
e = data.frame(cbind(data1[test,1],data1[test,2],predictedY))
colnames(e) <- c('Index', 'Actual', 'Predicted')


p <- ggplot(e, aes(e$Index))+
  theme_bw()+
  scale_shape_identity()+
  scale_color_manual(labels = c("Actual", "Predicted"), 
                     values = c("deepskyblue", "darkblue"),
                     guide = guide_legend(override.aes = list(
                       shape = c(19, 4))))+
  geom_point(aes(y = e$Predicted, color = 'blue'), size = 2.5)+
  geom_point(aes(y = e$Actual, color = 'red', shape = 4), size = 2)+
  xlab("Index")+ 
  ylab("Price") +
  ggtitle("Accuracy of Prediction")+
  theme(legend.text = element_text(colour="black", size=10, face="bold"))+
  labs(color='')+
  theme(axis.title = element_text(colour="black", face="bold"))
p





#############################method 0#############################
#########################Support Vector machines#################
library(e1071)
library(ggplot2)
library(glmnet)

#setwd("C:/Users/Hassan/Desktop/Grad School/Sem 2/Business Analytics/Project")
setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')

# import data set
data1 <- read.csv("DataEngineered_04_23_17_Method1.csv")[,c(-1,-23,-24)]

#Randomly divide data into two parts
set.seed(4574)
train <- sample(1:nrow(data1), 0.5*nrow(data1))
not_train <- -train

#take a half as train, leave other half for test and validate 
data.train1 <- data1[train,]
data.not_train1 <- data1[not_train,]

#divide other half into two again to get train and validate
test <- sample(1:nrow(data.not_train1), as.integer(0.5*nrow(data.not_train1)))
validate  <- -test
data.test1 <- data.not_train1[test,]
data.validate1 <- data.not_train1[validate,]

#use CV to find best kernel type
tc <- tune.control(cross = 5)

kernels <- c('linear', 'polynomial', 'radial', 'sigmoid')
cv.errors1 <- c()
for(t in 1:length(kernels)){
  if (t == 1){
    fit1 <- tune.svm (PRICE ~., data = data.train1, kernel = kernels[t],tunecontrol = tc,
                      cost = 10^(-3:1),epsilon = seq(0, 0.2, 0.02))
  }else if (t == 2){
    fit1 <- tune.svm (PRICE ~., data = data.train1, kernel = kernels[t],
                      tunecontrol = tc,cost = 10^(-3:1),degree = c(4,5,6))
  }else {fit1 <- tune.svm (PRICE ~., data = data.train1, kernel = kernels[t],
                           tunecontrol = tc,cost = 10^(-3:1),gamma = seq(.5, .9, by = .1))
  }
  pred3 <- predict(fit1$best.model, data.validate1)
  cv.errors1[t] <- mean((data.validate1$PRICE-pred3)^2)
}

#find which kernel performed best
t_star1 = which.min(cv.errors1)
cat('The optimal kernel is', kernels[t_star1])

#test best model on train + validate
data_final1 <- rbind(data.train1, data.validate1) 

#fit best model with best kernel
if (t_star == 1){
  final1 <- tune.svm(PRICE ~., data = data_final1, kernel = kernels[t_star],tunecontrol = tc,
                     cost = 10^(-3:1),epsilon = seq(0, 0.2, 0.02))
}else if (t_star == 2){ 
  final1 <- tune.svm (PRICE ~., data = data_final1, kernel = kernels[t_star],
                      tunecontrol = tc,cost = 10^(-3:1),degree = c(4,5,6))
}else{final1 <- tune.svm(PRICE ~., data = data_final1, kernel = kernels[t_star],
                         tunecontrol = tc,cost = 10^(-3:1), gamma = seq(.5, .9, by = .1))
}

#final MSE on training data
pred1 <- predict(final1$best.model, data_final1)
MSE <- mean((data_final1$PRICE-pred1)^2)

#final MSE on test data
pred2 <- predict(final1$best.model, data.test1)
MSE1 <- mean((data.test1$PRICE-pred2)^2)

#try 2 period multistep-ahead predictions
data_multi <- data.test1
s = 0
for (i in 1:(nrow(data.test1)-1)){
  predd <- predict(final1$best.model, data.test1[i,])
  data_multi$PAVG1[i+1] = predd
  predd1 <- predict(final1$best.model, data_multi[i+1,])
  s <- s + (predd1 - data.test1$PRICE[i+1])^2}
MSE_multi <- s/(nrow(data.test1)-1)

############## Lasso Regression  #####################

#Prepare inputs for Lasso

x <- model.matrix(PRICE~., data_final1)[,-1]
y <- data_final1$PRICE

#We wish to choose the best model using CV among
# lambda = 0, 0.001, 0.01, 0.1, 1, 10, 100, 1000 
grid = c(10^(-3:3),0)  #set sequence of lambdas we want to test

cv.out = cv.glmnet(x, y, alpha=1, lambda=grid, nfolds=5) 

# optimum lambda
bestlam = cv.out$lambda.min
cat('The optimum lambda is ',bestlam)

#Train model with best value of lambda on the training set
lasso.mod = glmnet(x, y, alpha=1, lambda=bestlam)

#coefficients of Lasso
lasso.coef = predict(lasso.mod, type = "coefficients", s=bestlam )


#Evaluate this model on the test set
pred = predict(lasso.mod, x)
actual = y
MSE <- mean((actual-pred)^2) 
cat('The MSE of the model using training data is', MSE)

xnew <- model.matrix(PRICE~., data.test1)[,-1]
ynew <- data.test1$PRICE

#Evaluate this model on the test set
pred = predict(lasso.mod, xnew)
actual = ynew
MSE <- mean((actual-pred)^2) 
cat('The MSE of model using out of sample data', MSE)

data_multi1 <- xnew
s = 0
for (i in 1:(nrow(data.test1)-1)){
  predd <- sum(lasso.coef[2:length(lasso.coef)] * xnew[i,]) + lasso.coef[1]
  data_multi1[i+1, 10] = predd
  predd1 <- sum(lasso.coef[2:length(lasso.coef)] * data_multi1[i+1,])
  s <- s + (predd1 - ynew[i+1])^2}
MSE_multi <- s/(nrow(xnew)-1)


