# Onion Analytics
# This R code uses method 0 (1 day ahead compounded, using predictions to predict further)
# It takes websacrapped data "DataBAOnionsXX" and weather data "DataBAClimate" and creates a dataset with prices and features
# Outputs "DataEngineeredMethod0" csv

setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/dataengineering')
#Read, subset and process
PriceData1.df = read.csv("DataBAOnions0811.csv")
PriceData1.df = subset(PriceData1.df, PriceData1.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData1.df$Date = as.Date(as.character(PriceData1.df$Date), format = "%d/%m/%y")
PriceData2.df = read.csv("DataBAOnions1216.csv")
PriceData2.df = subset(PriceData2.df, PriceData2.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData2.df$Date = as.Date(as.character(PriceData2.df$Date), format = "%m/%d/%Y")
PriceData.df = rbind(PriceData1.df,PriceData2.df)
ClimateData.df = read.csv("DataBAClimate.csv")
ClimateData.df$DATE = as.Date(as.character(ClimateData.df$DATE), format = "%Y%m%d")

#List origins and stations
stations = levels(ClimateData.df$State)
origins = levels(PriceData.df$Origin)

#stations needed: aguascalientes, baja california sur, coahuila, distrito federal, guanajato, importacion, nuevo leon, tamaulipas
#stations not needed: colima, estado de mexico, oaxaca, tabasco, veracruz

#Create dataframe
Output.df = data.frame(matrix(ncol = 22, nrow = nrow(PriceData.df)))
column_names = c("PRICE","TAVG","TMAX","TMIN", "PRCPCUM","PRCPMAX","MAXDAYSNOPRCP","WINTER","SPRING","SUMMER","PAVG1","PMAX1","PMIN1","PAVG2","PMAX2","PMIN2","PAVG3","PMAX3","PMIN3","PAVG4","PMAX4","PMIN4")
for(i in 1:length(column_names)){
  colnames(Output.df)[i] <- column_names[i]
}

#Column1: Ouput Price
Output.df$PRICE = PriceData.df$MeanPrice


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



# Columns 11 to 22:
# Fill in prices of previous 4 days. If possible, take the ones from the same origin
# If not, take the average of the day from other zones

for (i in 7:nrow(PriceData.df)){
  data_subset_date = subset(PriceData.df, Date < PriceData.df$Date[i] & Date > PriceData.df$Date[i]-7)
  data_subset_origin = subset(data_subset_date, Origin == PriceData.df$Origin[i])
  if (nrow(data_subset_origin) >= 1){
    Output.df$PAVG1[i] = data_subset_origin$MeanPrice[nrow(data_subset_origin)]
    Output.df$PMAX1[i]= data_subset_origin$MaxPrice[nrow(data_subset_origin)]
    Output.df$PMIN1[i] = data_subset_origin$MinPrice[nrow(data_subset_origin)]
    if (nrow(data_subset_origin) >= 2){
      Output.df$PAVG2[i] = data_subset_origin$MeanPrice[nrow(data_subset_origin)-1]
      Output.df$PMAX2[i]= data_subset_origin$MaxPrice[nrow(data_subset_origin)-1]
      Output.df$PMIN2[i] = data_subset_origin$MinPrice[nrow(data_subset_origin)-1]
      if (nrow(data_subset_origin) >= 3){
        Output.df$PAVG3[i] = data_subset_origin$MeanPrice[nrow(data_subset_origin)-2]
        Output.df$PMAX3[i]= data_subset_origin$MaxPrice[nrow(data_subset_origin)-2]
        Output.df$PMIN3[i] = data_subset_origin$MinPrice[nrow(data_subset_origin)-2]
        if (nrow(data_subset_origin) >= 4){
          Output.df$PAVG4[i] = data_subset_origin$MeanPrice[nrow(data_subset_origin)-3]
          Output.df$PMAX4[i]= data_subset_origin$MaxPrice[nrow(data_subset_origin)-3]
          Output.df$PMIN4[i] = data_subset_origin$MinPrice[nrow(data_subset_origin)-3]
        } else {
          if(weekdays(PriceData.df$Date[i]) != "viernes"){
            data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-6)
          } else {
            data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
          }
          Output.df$PAVG4[i] = mean(data_subset_4$MeanPrice)
          Output.df$PMAX4[i]= mean(data_subset_4$MaxPrice)
          Output.df$PMIN4[i] = mean(data_subset_4$MinPrice)
        }
      } else {
        if(weekdays(PriceData.df$Date[i]) != "viernes"){
          data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-6)
        } else {
          data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
        }
        Output.df$PAVG4[i] = mean(data_subset_4$MeanPrice)
        Output.df$PMAX4[i]= mean(data_subset_4$MaxPrice)
        Output.df$PMIN4[i] = mean(data_subset_4$MinPrice)
        
        if(weekdays(PriceData.df$Date[i]) != "viernes" & weekdays(PriceData.df$Date[i]) != "jueves"){
          data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-5)
        } else {
          data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-3)
        }
        Output.df$PAVG3[i] = mean(data_subset_3$MeanPrice)
        Output.df$PMAX3[i]= mean(data_subset_3$MaxPrice)
        Output.df$PMIN3[i] = mean(data_subset_3$MinPrice)
      }
    } else {
      
      if(weekdays(PriceData.df$Date[i]) != "viernes"){
        data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-6)
      } else {
        data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
      }
      Output.df$PAVG4[i] = mean(data_subset_4$MeanPrice)
      Output.df$PMAX4[i]= mean(data_subset_4$MaxPrice)
      Output.df$PMIN4[i] = mean(data_subset_4$MinPrice)
      
      if(weekdays(PriceData.df$Date[i]) != "viernes" & weekdays(PriceData.df$Date[i]) != "jueves"){
        data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-5)
      } else {
        data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-3)
      }
      Output.df$PAVG3[i] = mean(data_subset_3$MeanPrice)
      Output.df$PMAX3[i]= mean(data_subset_3$MaxPrice)
      Output.df$PMIN3[i] = mean(data_subset_3$MinPrice)
      
      if(weekdays(PriceData.df$Date[i]) == "lunes" | weekdays(PriceData.df$Date[i]) == "martes"){
        data_subset_2 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
      } else {
        data_subset_2 = subset(data_subset_date,Date == PriceData.df$Date[i]-2)
      }
      Output.df$PAVG2[i] = mean(data_subset_2$MeanPrice)
      Output.df$PMAX2[i]= mean(data_subset_2$MaxPrice)
      Output.df$PMIN2[i] = mean(data_subset_2$MinPrice)
      
    }
  } else {
    
    if(weekdays(PriceData.df$Date[i]) != "viernes"){
      data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-6)
    } else {
      data_subset_4 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
    }
    Output.df$PAVG4[i] = mean(data_subset_4$MeanPrice)
    Output.df$PMAX4[i]= mean(data_subset_4$MaxPrice)
    Output.df$PMIN4[i] = mean(data_subset_4$MinPrice)
    
    if(weekdays(PriceData.df$Date[i]) != "viernes" & weekdays(PriceData.df$Date[i]) != "jueves"){
      data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-5)
    } else {
      data_subset_3 = subset(data_subset_date,Date == PriceData.df$Date[i]-3)
    }
    Output.df$PAVG3[i] = mean(data_subset_3$MeanPrice)
    Output.df$PMAX3[i]= mean(data_subset_3$MaxPrice)
    Output.df$PMIN3[i] = mean(data_subset_3$MinPrice)
    
    if(weekdays(PriceData.df$Date[i]) == "lunes" | weekdays(PriceData.df$Date[i]) == "martes"){
      data_subset_2 = subset(data_subset_date,Date == PriceData.df$Date[i]-4)
    } else {
      data_subset_2 = subset(data_subset_date,Date == PriceData.df$Date[i]-2)
    }
    Output.df$PAVG2[i] = mean(data_subset_2$MeanPrice)
    Output.df$PMAX2[i]= mean(data_subset_2$MaxPrice)
    Output.df$PMIN2[i] = mean(data_subset_2$MinPrice)
    
    if(weekdays(PriceData.df$Date[i]) == "lunes"){
      data_subset_1 = subset(data_subset_date,Date == PriceData.df$Date[i]-3)
    } else {
      data_subset_1 = subset(data_subset_date,Date == PriceData.df$Date[i]-1)
    }
    Output.df$PAVG1[i] = mean(data_subset_1$MeanPrice)
    Output.df$PMAX1[i]= mean(data_subset_1$MaxPrice)
    Output.df$PMIN1[i] = mean(data_subset_1$MinPrice)
    
  }
}


#The previous algorithm works well for all dates but for holidays
# We aproximate the "NaN" generated by the closest datapoints
# The following code will change 131 points for the current dataset

count = 0
for (i in 7:nrow(Output.df)){
  if (is.na(Output.df$PAVG1[i])){
    if (!is.na(Output.df$PAVG2[i])){
      Output.df$PAVG1[i] = Output.df$PAVG2[i]
      Output.df$PMAX1[i] = Output.df$PMAX2[i]
      Output.df$PMIN1[i] = Output.df$PMIN2[i]
    } else if (!is.na(Output.df$PAVG3[i])){
      Output.df$PAVG1[i] = Output.df$PAVG3[i]
      Output.df$PMAX1[i] = Output.df$PMAX3[i]
      Output.df$PMIN1[i] = Output.df$PMIN3[i]
    } else if (!is.na(Output.df$PAVG4[i])){
      Output.df$PAVG1[i] = Output.df$PAVG4[i]
      Output.df$PMAX1[i] = Output.df$PMAX4[i]
      Output.df$PMIN1[i] = Output.df$PMIN4[i]
    }
  }
  if (is.na(Output.df$PAVG2[i])){
    if (!is.na(Output.df$PAVG1[i]) & !is.na(Output.df$PAVG3[i])){
      Output.df$PAVG2[i] = mean(Output.df$PAVG1[i],Output.df$PAVG3[i])
      Output.df$PMAX2[i]= mean(Output.df$PMAX1[i],Output.df$PMAX3[i])
      Output.df$PMIN2[i] = mean(Output.df$PMIN1[i],Output.df$PMIN3[i])
    } else if (!is.na(Output.df$PAVG1[i])){
      Output.df$PAVG2[i] = Output.df$PAVG1[i]
      Output.df$PMAX2[i] = Output.df$PMAX1[i]
      Output.df$PMIN2[i] = Output.df$PMIN1[i]
    } else if (!is.na(Output.df$PAVG3[i])){
      Output.df$PAVG2[i] = Output.df$PAVG3[i]
      Output.df$PMAX2[i] = Output.df$PMAX3[i]
      Output.df$PMIN2[i] = Output.df$PMIN3[i]
    } else if (!is.na(Output.df$PAVG4[i])){
      Output.df$PAVG2[i] = Output.df$PAVG4[i]
      Output.df$PMAX2[i] = Output.df$PMAX4[i]
      Output.df$PMIN2[i] = Output.df$PMIN4[i]
    }
  }
  
  if (is.na(Output.df$PAVG3[i])){
    if (!is.na(Output.df$PAVG2[i]) & !is.na(Output.df$PAVG4[i])){
      Output.df$PAVG3[i] = mean(Output.df$PAVG2[i],Output.df$PAVG4[i])
      Output.df$PMAX3[i] = mean(Output.df$PMAX2[i],Output.df$PMAX4[i])
      Output.df$PMIN3[i] = mean(Output.df$PMIN2[i],Output.df$PMIN4[i])
    } else if (!is.na(Output.df$PAVG2[i])){
      Output.df$PAVG3[i] = Output.df$PAVG2[i]
      Output.df$PMAX3[i] = Output.df$PMAX2[i]
      Output.df$PMIN3[i] = Output.df$PMIN2[i]
    } else if (!is.na(Output.df$PAVG4[i])){
      Output.df$PAVG3[i] = Output.df$PAVG4[i]
      Output.df$PMAX3[i] = Output.df$PMAX4[i]
      Output.df$PMIN3[i] = Output.df$PMIN4[i]
    } else if (!is.na(Output.df$PAVG1[i])){
      Output.df$PAVG3[i] = Output.df$PAVG1[i]
      Output.df$PMAX3[i] = Output.df$PMAX1[i]
      Output.df$PMIN3[i] = Output.df$PMIN1[i]
    }
  }
  
  if (is.na(Output.df$PAVG4[i])){
    if (!is.na(Output.df$PAVG3[i])){
      Output.df$PAVG4[i] = Output.df$PAVG3[i]
      Output.df$PMAX4[i] = Output.df$PMAX3[i]
      Output.df$PMIN4[i] = Output.df$PMIN3[i]
    } else if (!is.na(Output.df$PAVG2[i])){
      Output.df$PAVG4[i] = Output.df$PAVG2[i]
      Output.df$PMAX4[i] = Output.df$PMAX2[i]
      Output.df$PMIN4[i] = Output.df$PMIN2[i]
    } else if (!is.na(Output.df$PAVG1[i])){
      Output.df$PAVG4[i] = Output.df$PAVG1[i]
      Output.df$PMAX4[i] = Output.df$PMAX1[i]
      Output.df$PMIN4[i] = Output.df$PMIN1[i]
    }
  }
}



# Columns 2 to 7: Weather data

#Temperature functions. Period must be in days
max_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - as.numeric(period))
  return(max(na.omit(data_subset$TMAX)))
}
min_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - period)
  return(min(na.omit(data_subset$TMIN)))
}
avg_temp_period = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - period)
  return(mean(na.omit(data_subset$TAVG)))
}


#Precipitation functions. Period must be in days
cumulative_prcp = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - period)
  return(sum(data_subset$PRCP))
}

max_prcp_days = function(state,period,date,days){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - period)
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

max_days_no_precep = function(state,period,date){
  data_subset = subset(ClimateData.df, as.character(State) == as.character(state) & DATE < date & DATE > date - period)
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

#Loop for climate data
period = 140
days = 3
for (i in 7:nrow(Output.df)){
  Output.df$TAVG[i] =  round(avg_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i]), digits = 2)
  Output.df$TMAX[i] =  max_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$TMIN[i] =  min_temp_period(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$PRCPCUM[i] = cumulative_prcp(PriceData.df$Origin[i],period,PriceData.df$Date[i])
  Output.df$PRCPMAX[i] = max_prcp_days(PriceData.df$Origin[i],period,PriceData.df$Date[i],days)
  Output.df$MAXDAYSNOPRCP[i] = max_days_no_precep(PriceData.df$Origin[i],period,PriceData.df$Date[i])
}

Output.df = na.omit(Output.df)
write.csv(Output.df,"DataEngineeredMethod0.csv")


