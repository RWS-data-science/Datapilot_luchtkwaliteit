library(data.table)
library(ggplot2)
library(GGally)
library(reshape2)
library(leaflet)
data<-read.csv("data/dataframe_11u13.csv")
data$dateTime<-as.POSIXct(as.character(data$dateTime), format = "%Y-%m-%d %H:%M:%S")
data<-data.table(data)
avgGlobalPerHour<-data[,.(avg=mean(NO2)), by=.(YYYYMMDD,HH)]


avgGlobalPerHourPerStation<-data[,.(avg=mean(NO2)), by=.(HH,stn_ID)]
ggplot(avgGlobalPerHourPerStation, aes(dateTime,avg))+geom_line(aes(group = stn_ID))+ facet_wrap(~stn_ID)
#
#test<-split(avgGlobalPerHourPerStation,avgGlobalPerHourPerStation$stn_ID)
#tableForCorr<-do.call(cbind,test)

test<-reshape(avgGlobalPerHourPerStation[,c(-1,-2)], idvar = "dateTime", timevar = "stn_ID", direction = "wide")
ggcorr(test[,-1],midpoint = .5, low=  "#EEEEEE")
ggcorr(test[,-1],nbreaks=8, palette='RdGy')



unique(data$Street)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

