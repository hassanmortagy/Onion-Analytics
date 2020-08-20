# Onion Analytics
# This R code uses DataEngineeredFinal.csv
# It forecasts the zones that will serve Aguacalientes market at each year (before it begins)
# Outputs "ZonePred20XX" csv
# Needs to comment and uncomment to produc each different year predictions


setwd('~/Desktop/COLUMBIA/Business Analytics/projectba/forecasting')
PriceData1.df = read.csv("DataBAOnions0811.csv")
PriceData1.df = subset(PriceData1.df, PriceData1.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData1.df$Date = as.Date(as.character(PriceData1.df$Date), format = "%d/%m/%y")
PriceData2.df = read.csv("DataBAOnions1216.csv")
PriceData2.df = subset(PriceData2.df, PriceData2.df$Destiny == "Aguascalientes: Centro Comercial Agropecuario de Aguascalientes")
PriceData2.df$Date = as.Date(as.character(PriceData2.df$Date), format = "%m/%d/%Y")
PriceData.df = rbind(PriceData1.df,PriceData2.df)


#Uncomment one of these dates to change the final available date there is available at a certain point.
# To predict year 2014
#final_available_date = "2013-12-31"
#number_year_days = 365
# To predict year 2015
final_available_date = "2014-12-31"
number_year_days = 365
# To predict year 2016
#final_available_date = "2015-12-31"
#number_year_days = 366

PriceData.df = subset(PriceData.df, PriceData.df$Date <= as.Date(final_available_date, format = "%Y-%m-%d"))

zones = array(levels(PriceData.df$Origin))

pred_zones = data.frame(matrix(ncol = length(zones)+1, nrow = number_year_days))
colnames(pred_zones)[1] = "date"
for(i in 1:length(zones)){
  colnames(pred_zones)[i+1] <- zones[i]
}

pred_zones$date = seq.Date(as.Date(final_available_date)+1, as.Date(final_available_date)+number_year_days,"day")
for(day in 1:number_year_days){
  for (zone in 1:length(zones)){
    pred_zones[day,zone+1]=0
  }
}

used_zones = pred_zones
total_dates = pred_zones

for (year in 2007:as.numeric(format(as.Date(final_available_date),'%Y'))-1){
  first_date = as.Date(paste(year,"-12-31",sep = ""))
  
  if(year == 2012){
    numdays=366
  }else{
    numdays=365
  }
 
  for (day in 1:numdays){
    if (day>=60 & numdays==365){day = day + 1}
    for (zone in 1:length(zones)){
      data_day_origin = subset(PriceData.df, PriceData.df$Date == first_date + day & PriceData.df$Origin == zones[zone])
      data_day = subset(PriceData.df, PriceData.df$Date == first_date + day)
      if (nrow(data_day) >= 1){
        total_dates[day,zone+1] = total_dates[day,zone+1]+1
      }
      if (nrow(data_day_origin) >= 1){
        used_zones[day,zone+1] = used_zones[day,zone+1]+1
      }      
      
    } 
  }
}


pred_zones[,2:ncol(pred_zones)] = ifelse(used_zones[,2:ncol(pred_zones)]/total_dates[,2:ncol(pred_zones)]>=0.5,1,0)

for (i in 1:nrow(pred_zones)){
  for (j in 1:ncol(pred_zones)){
    if (is.na(pred_zones[i,j])){
      pred_zones[i,j] = 0
    }
  }
}

write.csv(pred_zones,"ZonesPred2015.csv")

