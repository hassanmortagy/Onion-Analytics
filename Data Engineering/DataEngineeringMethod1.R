# Onion Analytics
# This R code uses method 1 (28 days ahead)
# It takes websacrapped data "DataBAOnionsXX" and weather data "DataBAClimate" and creates a dataset with prices and features
# Outputs "DataEngineeredFinal" csv

setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/dataengineering')
PriceData1.df = read.csv("DataBAOnions0811.csv")
PriceData1.df = subset(PriceData1.df, PriceData1.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData1.df$Date = as.Date(as.character(PriceData1.df$Date), format = "%d/%m/%y")
PriceData2.df = read.csv("DataBAOnions1216.csv")
PriceData2.df = subset(PriceData2.df, PriceData2.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData2.df$Date = as.Date(as.character(PriceData2.df$Date), format = "%m/%d/%Y")
PriceData3.df = read.csv("DataBAOnions17.csv")
PriceData3.df = subset(PriceData3.df, PriceData3.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData3.df$Date = as.Date(as.character(PriceData3.df$Date), format = "%m/%d/%Y")
PriceData.df = rbind(PriceData1.df,PriceData2.df,PriceData3.df)

ClimateData.df = read.csv("DataBAClimate.csv")
ClimateData.df$DATE = as.Date(as.character(ClimateData.df$DATE), format = "%Y%m%d")

#List origins and stations
stations = levels(ClimateData.df$State)
origins = levels(PriceData.df$Origin)

#stations needed: aguascalientes, baja california sur, coahuila, distrito federal, guanajato, importacion, nuevo leon, tamaulipas
#stations not needed: colima, estado de mexico, oaxaca, tabasco, veracruz

#Create dataframe
Output.df = data.frame(matrix(ncol = 24, nrow = nrow(PriceData.df)))
column_names = c("PRICE","TAVG","TMAX","TMIN", "PRCPCUM","PRCPMAX","MAXDAYSNOPRCP","MAXDAYSHUMID10","WINTER","SPRING","SUMMER","PAVG1Y","PMAX1Y","PMIN1Y","PAVG2Y","PMAX2Y","PMIN2Y","PAVG1M","PMAX1M","PMIN1M","PCH1M1W","PCH1M1M","DATE","ORIGIN")
for(i in 1:length(column_names)){
  colnames(Output.df)[i] <- column_names[i]
}

#Column1: Ouput Price
Output.df$PRICE = PriceData.df$MeanPrice

#Column 23:
Output.df$DATE = PriceData.df$Date

#Column 24:
Output.df$ORIGIN = PriceData.df$Origin


# Columns 8 to 10: Seasons
#Function for the season
season = function(season_time, date){
  season_time = as.character(season_time)
  date = as.Date(date)
  back = 0
  month = as.character(as.numeric(format(date,"%m")))
  day = as.character(as.numeric(format(date,"%d")))
  converted_date = as.Date(paste(month,"/",day,"/2000", sep=""), format = "%m/%d/%Y")

  if (season_time == "winter"){
    if (converted_date >= as.Date("12/21/2000", format = "%m/%d/%Y") | converted_date < as.Date("3/21/2000", format = "%m/%d/%Y")){
      back = 1
    }
  }
  
  if (season_time == "spring"){
    if (converted_date >= as.Date("3/21/2000", format = "%m/%d/%Y") & converted_date < as.Date("6/21/2000", format = "%m/%d/%Y")){
      back = 1
    }
  }
  
  if (season_time == "summer"){
    if (converted_date >= as.Date("6/21/2000", format = "%m/%d/%Y") & converted_date < as.Date("9/21/2000", format = "%m/%d/%Y")){
      back = 1
    }
  }
  
  return(back)
}

#Loop for seasons
for (i in 1:nrow(PriceData.df)){
  Output.df$WINTER[i] = season("winter",PriceData.df$Date[i])
  Output.df$SPRING[i] = season("spring",PriceData.df$Date[i])
  Output.df$SUMMER[i] = season("summer",PriceData.df$Date[i])
}



# Columns 12 to 22:
# Fill in prices of previous 4 days. If possible, take the ones from the same origin
# If not, take the average of the day from other zones
FirstUsefulDate = 760

for (i in FirstUsefulDate:nrow(PriceData.df)){
  
  # Data from 28 days ago
  data_subset_4weeks = subset(PriceData.df, Date == PriceData.df$Date[i]-28)
  data_subset_4weeks_origin = subset(data_subset_4weeks, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_4weeks_origin) >= 1){
    Output.df$PAVG1M[i] = data_subset_4weeks_origin$MeanPrice
    Output.df$PMAX1M[i] = data_subset_4weeks_origin$MaxPrice
    Output.df$PMIN1M[i] = data_subset_4weeks_origin$MinPrice
  } else if (nrow(data_subset_4weeks) >= 1){
    Output.df$PAVG1M[i] = mean(data_subset_4weeks$MeanPrice)
    Output.df$PMAX1M[i]= mean(data_subset_4weeks$MaxPrice)
    Output.df$PMIN1M[i] = mean(data_subset_4weeks$MinPrice)
  } else {
    data_subset_4weeks = subset(PriceData.df, Date < PriceData.df$Date[i]-26 & Date > PriceData.df$Date[i]-30)
    data_subset_4weeks = na.omit(data_subset_4weeks)
    Output.df$PAVG1M[i] = mean(data_subset_4weeks$MeanPrice)
    Output.df$PMAX1M[i]= mean(data_subset_4weeks$MaxPrice)
    Output.df$PMIN1M[i] = mean(data_subset_4weeks$MinPrice)
  }

  # Change in prices: -28 to -35 days
  data_subset_5weeks = subset(PriceData.df, Date == PriceData.df$Date[i]-35)
  data_subset_5weeks_origin = subset(data_subset_5weeks, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_5weeks_origin) >= 1 & nrow(data_subset_4weeks_origin) >= 1){
    Output.df$PCH1M1W[i] = data_subset_4weeks_origin$MeanPrice - data_subset_5weeks_origin$MeanPrice
  } else if (nrow(data_subset_5weeks) >= 1 & nrow(data_subset_4weeks) >= 1){
    Output.df$PCH1M1W[i] = mean(data_subset_4weeks$MeanPrice) - mean(data_subset_5weeks$MeanPrice)
  } else {
    data_subset_4weeks = subset(PriceData.df, Date < PriceData.df$Date[i]-26 & Date > PriceData.df$Date[i]-30)
    data_subset_4weeks = na.omit(data_subset_4weeks)
    data_subset_5weeks = subset(PriceData.df, Date < PriceData.df$Date[i]-33 & Date > PriceData.df$Date[i]-37)
    data_subset_5weeks = na.omit(data_subset_5weeks)
    Output.df$PCH1M1W[i] = mean(data_subset_4weeks$MeanPrice) - mean(data_subset_5weeks$MeanPrice)
  }
  
  # Change in prices: -28 to -56 days
  data_subset_8weeks = subset(PriceData.df, Date == PriceData.df$Date[i]-56)
  data_subset_8weeks_origin = subset(data_subset_8weeks, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_8weeks_origin) >= 1 & nrow(data_subset_4weeks_origin) >= 1){
    Output.df$PCH1M1M[i] = data_subset_4weeks_origin$MeanPrice - data_subset_8weeks_origin$MeanPrice
  } else if (nrow(data_subset_8weeks) >= 1 & nrow(data_subset_4weeks) >= 1){
    Output.df$PCH1M1M[i] = mean(data_subset_4weeks$MeanPrice) - mean(data_subset_8weeks$MeanPrice)
  } else {
    data_subset_4weeks = subset(PriceData.df, Date < PriceData.df$Date[i]-26 & Date > PriceData.df$Date[i]-30)
    data_subset_4weeks = na.omit(data_subset_4weeks)
    data_subset_8weeks = subset(PriceData.df, Date < PriceData.df$Date[i]-54 & Date > PriceData.df$Date[i]-58)
    data_subset_8weeks = na.omit(data_subset_8weeks)
    Output.df$PCH1M1M[i] = mean(data_subset_4weeks$MeanPrice) - mean(data_subset_8weeks$MeanPrice)
  }
  
  #data from 1 year ago
  data_subset_1year = subset(PriceData.df, Date == PriceData.df$Date[i]-364)
  data_subset_1year_origin = subset(data_subset_1year, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_1year_origin) >= 1){
    Output.df$PAVG1Y[i] = data_subset_1year_origin$MeanPrice
    Output.df$PMAX1Y[i] = data_subset_1year_origin$MaxPrice
    Output.df$PMIN1Y[i] = data_subset_1year_origin$MinPrice
  } else if (nrow(data_subset_1year) >= 1){
    Output.df$PAVG1Y[i] = mean(data_subset_1year$MeanPrice)
    Output.df$PMAX1Y[i]= mean(data_subset_1year$MaxPrice)
    Output.df$PMIN1Y[i] = mean(data_subset_1year$MinPrice)
  } else {
    data_subset_1year = subset(PriceData.df, Date < PriceData.df$Date[i]-362 & Date > PriceData.df$Date[i]-366)
    data_subset_1year = na.omit(data_subset_1year)
    Output.df$PAVG1Y[i] = mean(data_subset_1year$MeanPrice)
    Output.df$PMAX1Y[i]= mean(data_subset_1year$MaxPrice)
    Output.df$PMIN1Y[i] = mean(data_subset_1year$MinPrice)
  }
  
  #data from 2 year ago
  data_subset_2year = subset(PriceData.df, Date == PriceData.df$Date[i]-728)
  data_subset_2year_origin = subset(data_subset_2year, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_2year_origin) >= 1){
    Output.df$PAVG2Y[i] = data_subset_2year_origin$MeanPrice
    Output.df$PMAX2Y[i] = data_subset_2year_origin$MaxPrice
    Output.df$PMIN2Y[i] = data_subset_2year_origin$MinPrice
  } else if (nrow(data_subset_2year) >= 1){
    Output.df$PAVG2Y[i] = mean(data_subset_2year$MeanPrice)
    Output.df$PMAX2Y[i]= mean(data_subset_2year$MaxPrice)
    Output.df$PMIN2Y[i] = mean(data_subset_2year$MinPrice)
  } else {
    data_subset_2year = subset(PriceData.df, Date < PriceData.df$Date[i]-726 & Date > PriceData.df$Date[i]-730)
    data_subset_2year = na.omit(data_subset_2year)
    Output.df$PAVG2Y[i] = mean(data_subset_2year$MeanPrice)
    Output.df$PMAX2Y[i]= mean(data_subset_2year$MaxPrice)
    Output.df$PMIN2Y[i] = mean(data_subset_2year$MinPrice)
  }
}

# The previous algorithm works well for all dates but for holidays
# We will eliminate those rows at the end.




# Columns 2 to 8: Weather data


#Temperature functions. Period must be in days

max_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date-28 & DATE > date - as.numeric(period))
  return(max(na.omit(data_subset$TMAX)))
}
min_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date-28 & DATE > date - as.numeric(period))
  return(min(na.omit(data_subset$TMIN)))
}
avg_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date-28 & DATE > date - as.numeric(period))
  return(mean(na.omit(data_subset$TAVG)))
}


#Precipitation functions. Period must be in days

#cumulative precep in the range -28 until period
cumulative_prcp = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date-28 & DATE > date - as.numeric(period))
  return(sum(data_subset$PRCP))
}

#max precep in the range -28 until period in a x day period
max_prcp_days = function(state,period,date,days){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date-28 & DATE > date - as.numeric(period))
  max = 0
  for (i in days:nrow(data_subset)){
    data_x_days = subset(data_subset, DATE >= data_subset$DATE[i]-days & DATE < data_subset$DATE[i])
    x_day_prec = sum(data_x_days$PRCP)
    if (max < x_day_prec){
      max = x_day_prec
    }
  }
  return (max)
}

#max #days without precipitation in the range -28 until period
max_days_no_precep = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - as.numeric(period))
  max = 0
  start_date = date - period
  num_consec = 0
  if (nrow(data_subset) >= 1){
    for (i in 1:nrow(data_subset)){
      if (data_subset$PRCP[i] == 0){
        num_consec = as.numeric(data_subset$DATE[i] - start_date)
      } else {
        start_date = data_subset$DATE[i]
      }
      if (max < num_consec){
        max = num_consec
      }
    }
  }
  return (max)
}

# Gives a 1 if there has been a humid period int the range -28 - period, 0 if not
max_days_humid_10 = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date - 28 & DATE > date - as.numeric(period))
  past_days_rain = c()
  humid = 0
  if (nrow(data_subset) >= humid_days_total){
    for (i in humid_days_total:nrow(data_subset)){
      past_days_rain = data_subset$PRCP[(i-humid_days_total+1):i]
      count = 0
      for (j in 1:humid_days_total){
        if (past_days_rain[j] != 0){count = count + 1}
      }
      if (count >= humid_days_rain){humid = 1}
    }
  }
  return (humid)
}

#Loop for climate data
#Period 20 weeks for onions. We use days -28 to -140
#3 days with a lot of rain may be bad as well
#7 days raining in 10 is bad: humid soil leads to deseases
period = 140
days = 3
humid_days_total = 10
humid_days_rain = 6
for (i in FirstUsefulDate:nrow(Output.df)){
  Output.df$TAVG[i] =  round(avg_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i]), digits = 2)
  Output.df$TMAX[i] =  max_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$TMIN[i] =  min_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$PRCPCUM[i] = cumulative_prcp(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$PRCPMAX[i] = max_prcp_days(PriceData.df$Origin[i],period,PriceData.df$Date[i],days)
  Output.df$MAXDAYSNOPRCP[i] = max_days_no_precep(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$MAXDAYSHUMID10[i] = max_days_humid_10(PriceData.df$Origin[i],period,PriceData.df$Date[i])
}

Output.df = na.omit(Output.df)
write.csv(Output.df,"DataEngineeredFinal.csv")
