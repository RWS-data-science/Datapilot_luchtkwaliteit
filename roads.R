

library(data.table)
library(ggplot2)
library(GGally)
library(reshape2)
library(chron)
Sys.setenv(TZ = "UTC")

data<-read.csv("data/dataframe2_10u55.csv")


data$dateTime<-as.POSIXct(as.character(data$dateTime), format = "%Y-%m-%d %H:%M:%S")
data<-data.table(data)
data<-data[!is.na(dateTime)]
data <- within(data, {
  dateTime[stn_ID==35 & dateTime>=as.POSIXct("2016-07-21 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") & 
             dateTime<= as.POSIXct("2016-08-04 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") ] <- NA
  stn_ID <- as.character(stn_ID)
  stn_ID[stn_ID=="34" &  dateTime<=as.POSIXct("2016-08-03 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")] <- "34a"
  stn_ID[stn_ID=="34"] <- "34b"
  stn_ID <- factor(stn_ID)
  NO2minusCAMS <- NO2-cams_NO2_3x3_min
  
})

data<-subset(data,!stn_ID %in% c(5,23))




trafficAndRoads<-data[,.(dateTime,stn_ID,NO2,NO2minusCAMS,NSL_inproduct,NSL_INT_LV,NSL_INT_MV,NSL_INT_ZV,NSL_INT_BV,dist_SEGMENT_ID_nearest,FH,DD)]


trafficAndRoads <- within(trafficAndRoads, {
  DD[DD%in%c(0, 990)] <- NA
  # cut in 4 groups of wind directions
  windDirection <- cut(DD, breaks = c(-1, 90, 180, 270, 361), labels = c("NO", "SO", "SW", "NW"))
})



trafficAndRoads[,road_contr:=ifelse(NSL_inproduct>=0,yes = NSL_inproduct, no = 0)]

trafficAndRoads[,contRoadLV:=FH/10*road_contr*NSL_INT_LV/dist_SEGMENT_ID_nearest]
trafficAndRoads[,contRoadMV:=FH/10*road_contr*NSL_INT_MV/dist_SEGMENT_ID_nearest]
trafficAndRoads[,contRoadZV:=FH/10*road_contr*NSL_INT_ZV/dist_SEGMENT_ID_nearest]
trafficAndRoads[,contRoadBV:=FH/10*road_contr*NSL_INT_BV/dist_SEGMENT_ID_nearest]

trafficType <- melt(trafficAndRoads, id.vars = c("dateTime", "stn_ID", "NO2minusCAMS"), measure.vars = c("contRoadLV", "contRoadMV", "contRoadZV", "contRoadBV"))
pairs(x = trafficType[stn_ID =="8",-2], col = adjustcolor("red", alpha = .2), pch = ".")

ggplot(trafficType[variable=="contRoadLV",], aes(x = value, y = NO2minusCAMS))+geom_smooth(aes(colour = variable))+facet_wrap(~stn_ID, scales = "free_x")


ggplot(trafficAndRoads,aes(x = FH, y = NO2))+geom_smooth()+facet_wrap(~stn_ID, scales = "free_x")

ggplot(trafficAndRoads[dateTime<=as.POSIXct("2016-04-01 00:00:00", tz="UTC")],aes(x = FH, y = NO2minusCAMS))+geom_smooth()+facet_wrap(~stn_ID, scales = "free_x")

ggplot(trafficAndRoads[!is.na(DD)],aes(x = FH/10, y = NO2minusCAMS))+geom_smooth(aes(colour = windDirection), method ="loess") + facet_wrap(~stn_ID, ncol = 4, scales = "free_x")+
  labs(title="Relations between wind speed and NO2 corrected pollution by wind direction",x="wind speed (m/s)", y="corrected NO2", colour = "wind direction")
ggplot(trafficAndRoads,aes(x = FH, y = NO2minusCAMS))+geom_smooth()+facet_wrap(~stn_ID, scales = "free_x")




