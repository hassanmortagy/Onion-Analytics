# Onion Analytics
# Train the model every 28 days from 2014 to 2016
# Uses data engineered (only the one available at each time) from CSV: "Data EngineeredFinal"
# Uses Zones forecasted CSVs "ZonePred201X" to create the test set as in real life you do not know the origins that will serve the market before it happens.
# Produces a CSV with prices forecasted and actual named "ResultsFinalTool"

# Code with CV takes 10h to run. 
# Remove steps 4 and 5 and uncomment lines below to reduce to 30s

library(e1071)
library(ggplot2)
library(glmnet)


setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')


########### Step 1: Read Data
original_data <- read.csv("DataEngineeredFinal.csv")
original_data$DATE <- as.Date(original_data$DATE)


#dates_months= c('2013-12-31',
#                '2014-01-31','2014-02-28','2014-03-31','2014-04-30','2014-05-31','2014-06-30',
#                '2014-07-31','2014-08-31','2014-09-30','2014-10-31','2014-11-30','2014-12-31',
#                '2015-01-31','2015-02-28','2015-03-31','2015-04-30','2015-05-31','2015-06-30',
#                '2015-07-31','2015-08-31','2015-09-30','2015-10-31','2015-11-30','2015-12-31',
#                '2016-01-31','2016-02-29','2016-03-31','2016-04-30','2016-05-31','2016-06-30',
#                '2016-07-31','2016-08-31','2016-09-30','2016-10-31','2016-11-30','2016-12-31')

dates_months = seq.Date(from=as.Date('2013-12-31',format='%Y-%m-%d'),to=as.Date('2016-12-31',format='%Y-%m-%d'),by=28)

results = data.frame(matrix(ncol = 3, nrow = 0))
colnames(results) <- c('Date', 'Actual', 'Predicted')

#We will train and predict every 28 days
for (i in 1:(length(dates_months)-1)){
  
  ########### Step 2: Create Training Data Frame with All Available Data
  set.seed(1)
  data_training_all = subset(original_data, original_data$DATE <= as.Date(dates_months[i]))[c(-1, -24, -25)]
  
  #divide train data into train and validate data set to find best kernel
  train <- sample(1:nrow(data_training_all), 0.75*nrow(data_training_all))
  data.train <- data_training_all[train,]
  data.validate <- data_training_all[-train,]
  
  
  
  ########### Step 3: Create Test Data Frame with The Zones Predicted to Output In next 28 days
  test_dataset = data.frame(matrix(ncol = ncol(original_data)-4, nrow = 0))
  test_dates = c()
  test_real_prices = c()
  colnames(test_dataset) <- colnames(original_data)[c(-1,-2,-24,-25)]
  
  zones <- read.csv(paste('ZonesPred',as.character(as.numeric(format(as.Date(dates_months[i+1]),'%Y'))),'.csv', sep = ''))
  zones$date <- as.Date(zones$date)
  zones = zones[,-1]
  colnames(zones)[2] <- "Baja California"
  zones <- subset(zones, zones$date > dates_months[i] & zones$date <= dates_months[i+1])
  for (k in 1:nrow(zones)){
    for (j in 1:ncol(zones)){
      if(zones[k,j]==1){
        data_zone_date = subset(original_data, original_data$ORIGIN == colnames(zones)[j] &
                                  original_data$DATE == zones$date[k])
        if(nrow(data_zone_date)==1){
          test_dates[length(test_dates)+1]=as.character(as.Date(zones$date[k],format='%Y-%m-%d'))
          test_real_prices[length(test_real_prices)+1]= data_zone_date$PRICE
          test_dataset = rbind(test_dataset,data_zone_date[c(-1,-2,-24,-25)])
        } else{
          data_date = subset(original_data, original_data$DATE == zones$date[k])
          if(nrow(data_date)>=1){
            test_dates[length(test_dates)+1]=as.character(as.Date(zones$date[k],format='%Y-%m-%d'))
            test_real_prices[length(test_real_prices)+1]=mean(data_date$PRICE)
            
            #create extra dataframe to add row to the test dataframe
            test_dataset2 = data.frame(matrix(ncol = ncol(original_data)-4, nrow = 1))
            colnames(test_dataset2) <- colnames(original_data)[c(-1,-2,-24,-25)]
            test_dataset = rbind(test_dataset,test_dataset2)
            
            for (m in 1:ncol(test_dataset)){
              test_dataset[nrow(test_dataset),m] = mean(data_date[,m+2])
            }
          }
        }
      }
    }
  }
  test_dates = as.Date(test_dates,format='%Y-%m-%d')
  
  
  
  
  ########### Step 4: use CV to find best kernel type for this month
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
  
  
  ########### Step 5: fit best model with best kernel on validate and train datasets combined
  if (t_star == 1){
    final <- tune.svm(PRICE ~., data = data_training_all, kernel = kernels[t_star],tunecontrol = tc,
                      cost = 10^(-3:1),epsilon = seq(0, 0.2, 0.02))
  }else if (t_star == 2){ 
    final <- tune.svm (PRICE ~., data = data_training_all, kernel = kernels[t_star],
                       tunecontrol = tc,cost = 10^(-3:1),degree = c(4,5,6))
  }else{final <- tune.svm(PRICE ~., data = data_training_all, kernel = kernels[t_star],
                          tunecontrol = tc,cost = 10^(1:3), gamma = seq(0, 1, by = .1),
                          epsilon = c(0, 0.01, 0.05, 0.1))
  }
  final1 = final$best.model
  

  #code for testing without CV. Takes 30s to run compared to 10h
  # Un comment lines below and remove steps 4 and 5
  #final1 <- svm(PRICE ~., data = data_training_all, kernel = 'radial',
  #              cost = 10, gamma = 0.2,
  #              epsilon = 0.05)
  
  
  
  ########### Step 6: compute MSEs and plot against real values
  #final MSE on training data
  pred.train <- predict(final1, data_training_all)
  MSEtrain <- mean((data_training_all$PRICE-pred.train)^2)
  print(MSEtrain)
  
  #final MSE on test data
  pred.test <- predict(final1, test_dataset)
  MSEtest <- mean((test_real_prices-pred.test)^2)
  print(MSEtest)
  
  #create plot
  iteration_results = data.frame(matrix(ncol = 3, nrow = nrow(test_dataset)))
  colnames(iteration_results) <- c('Date', 'Actual', 'Predicted')
  iteration_results$Date = test_dates
  iteration_results$Actual = test_real_prices
  iteration_results$Predicted = pred.test
  results = rbind(results,iteration_results)
}



########### Step 7: Final Overall Plot: Predicted Vs. Real

p <- ggplot(results, aes(results$Date))+
  theme_bw()+
  scale_shape_identity()+
  scale_color_manual(labels = c("Actual", "Predicted"), 
                     values = c("deepskyblue", "darkblue"),
                     guide = guide_legend(override.aes = list(
                       shape = c(19, 4))))+
  geom_point(aes(y = results$Actual, color = 'blue'), size = 2.5)+
  geom_point(aes(y = results$Predicted, color = 'red', shape = 4), size = 2)+
  xlab("Date")+ 
  ylab("Price") +
  ggtitle("Prediction vs Actual")+
  theme(legend.text = element_text(colour="black", size=10, face="bold"))+
  labs(color='')+
  theme(axis.title = element_text(colour="black", face="bold"))
print(p)


########### Step 8: Overall MSE
MSETOTAL <- mean((results$Actual-results$Predicted)^2)
print(MSETOTAL)


########### Step 9: SaveToFile

write.csv(results, "ResultsFinalTool.csv")


