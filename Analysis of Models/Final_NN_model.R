setwd("C:/Users/Ramsey/Desktop/Final dataset")
data = read.csv(file="DataEngineered_04_23_17_Method1.csv",stringsAsFactors = FALSE)
data_0=na.omit(data)

################ Linear regression
data2 <- tail(data,30)

data <- data_0[-1]
data$ORIGIN <- NULL
data$DATE <- NULL
head(data)
attach(data)

index <- sample(1:nrow(data_0),round(0.75*nrow(data_0)))
train <- data[index,]
test <- data[-index,]
testX <- data_0[-index,]

lm.fit <- glm(PRICE~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$PRICE)^2)/nrow(test)
print(MSE.lm)

################ Neural nets
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

################ Parameters
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("PRICE ~", paste(n[!n %in% "PRICE"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(13,7),linear.output=FALSE)
plot(nn)

################ Predicting medv using the neural network
pr.nn <- compute(nn,test_[2:ncol(data)])
pr.nn_ <- pr.nn$net.result*(max(data$PRICE)-min(data$PRICE))+min(data$PRICE)
test.r <- (test_$PRICE)*(max(data$PRICE)-min(data$PRICE))+min(data$PRICE)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))

## Plotting predictions of actual vs predict on test
plot(testX$X, test.r, col='blue', main="Actual (blue) vs predicted (red) price",
     xlab="Index", ylab="Price")
points(testX$X, pr.nn_, col='red')
