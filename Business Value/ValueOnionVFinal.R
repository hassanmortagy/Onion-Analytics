# Onion Analytics
# Compute the value created from the forecasting tool
# Use CSV with results from forecasted tool and real prices
# Produces a CSV with prices forecasted, real and value generated called "ValueofPredictions"

#setwd("~/Columbia/2 - Business Analytics/Project/Data")
setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')


PriceData.df = read.csv("DataBAOnions1216.csv")
PriceData.df = subset(PriceData.df, PriceData.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData.df$Date = as.Date(as.character(PriceData.df$Date), format = "%m/%d/%Y")


Forecast.df = read.csv("ResultsFinalTool.csv")
Forecast.df$X <- NULL
Forecast.df$Date = as.Date(as.character(Forecast.df$Date))

dates = seq.Date(as.Date("2014-01-01"), as.Date("2016-12-31"),"day")
value_summary = data.frame(matrix(nrow = length(dates),ncol = 8))
colnames(value_summary) = c("Date", "Mean_daily_price", "Mean_daily_forecast","Avg_next_28", "Model_sale","Value", "Best_sale", "Best_value")
value_summary$Date = dates

# Calculate Mean_daily_price
start_date = as.Date("2013-12-31")
for (i in 1:nrow(value_summary)){
  data_day_real = subset(PriceData.df, PriceData.df$Date == start_date+i)
  data_day_forecasted = subset(Forecast.df, Forecast.df$Date == start_date+i)
  if (nrow(data_day_real)>=1){
    value_summary$Mean_daily_price[i] = mean(data_day_real$MeanPrice)
  } else if (i>2){
    value_summary$Mean_daily_price[i] = value_summary$Mean_daily_price[i-1]
  }
  if (nrow(data_day_forecasted)>=1){
    value_summary$Mean_daily_forecast[i] = mean(data_day_forecasted$Predicted)
  } else if (i>2){
    value_summary$Mean_daily_forecast[i] = value_summary$Mean_daily_forecast[i-1]
  }
}

# Calculate average next 28 days and model predicted sale
for (i in 2:nrow(value_summary)-1){
  data_28days = subset(value_summary, value_summary$Date > start_date+i & value_summary$Date <= start_date+i+28)
  value_summary$Avg_next_28[i] = mean(data_28days$Mean_daily_price)
  value_summary$Model_sale[i] = data_28days$Mean_daily_price[which.max(data_28days$Mean_daily_forecast)]
  value_summary$Best_sale[i] = data_28days$Mean_daily_price[which.max(data_28days$Mean_daily_price)]
}

# Compute value
value_summary$Value <- value_summary$Model_sale - value_summary$Avg_next_28
value_summary$Best_value <- value_summary$Best_sale - value_summary$Avg_next_28
print(mean(value_summary$Value, na.rm = TRUE))
print(mean(value_summary$Best_value, na.rm = TRUE))


#plot(value_summary$Date, value_summary$Value, pch=10, cex=.2, col='blue', xlab = "Date", ylab = "$ Value of Model")
write.csv(value_summary, "ValueofPredictions.csv")

