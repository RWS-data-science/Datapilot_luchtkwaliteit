#Load the Aerias data provided by Bas
NO2 <- read.csv("data/data_knmi_bas_final/aireas/aireas_lml_2016.no2.csv",header = T)

#melt to get all measurement data into one column
library(reshape)
df<- melt(NO2,id.vars = "dateTime",variable_name = "stn_ID",na.rm = T)
df$stn_ID <- as.factor(gsub("X","",df$stn_ID)) #remove X's
df$NO2<- df$value
df$value <- NULL

#add station locations 
stations<- readxl::read_xlsx("data/data_knmi_bas_final/aireas/Locatie LML en AiREAS.xlsx",col_types = "guess")

df<- merge(df,stations,by.x = "stn_ID", by.y = "LML ID")
df$Postcode<- df$X__1
df$X__1<- NULL
df$Lat <- as.numeric(df$Lat)
df$Lon <- as.numeric(df$Lon)
df$dateTime <- as.POSIXct(strptime(df$dateTime,"%Y-%m-%d %H:%M:%S")) ##aangenomen dat alle tijden UTC zijn

#add weather data (Temperature, wind, etc...)
meteo <- read.table("data/data_knmi_bas2/knmi_meteo/KNMI_Eindhoven_201501-201710_hourly.dat",sep=",",skip = 31,header=TRUE)
colnames(meteo)<- gsub(" |#","",strsplit(readLines("data/data_knmi_bas2/knmi_meteo/KNMI_Eindhoven_201501-201710_hourly.dat",n=33)[33],",")[[1]])
meteo$datetime<- as.POSIXct(strptime(paste(meteo$YYYYMMDD,meteo$HH),"%Y%m%d %H"))

df <-merge(df,meteo,by.x="dateTime",by.y="datetime")


##PM10
PM10<- read.csv("data/data_knmi_bas_final/aireas/aireas_lml_2016.pm10.csv")
PM10<- melt(PM10,id.vars = "dateTime",variable_name = "stn_ID",na.rm = T)
PM10$stn_ID <- as.factor(gsub("X","",PM10$stn_ID)) #remove X's

PM10$dateTime <- as.POSIXct(strptime(PM10$dateTime,"%Y-%m-%d %H:%M:%S"))
PM10$PM10 <- PM10$value
PM10$value<- NULL

df<- merge(df,PM10,by=c("dateTime","stn_ID") )

##Data CAMS
cams<- read.csv("data/data_knmi_bas_final/cams/cams_Eindhoven_2016_NOx.csv")
cams$dateTime <- as.POSIXct(strptime(cams$dateTime,"%Y-%m-%d %H:%M:%S"))
colnames(cams)<- paste0("cams_",colnames(cams))

df<- merge(df,cams,by.x = "dateTime",by.y = "cams_dateTime")

##Data van Sjoerd
data_sjoerd_onlyshortest<- read.csv("data/uitwisseling/combineLinePointsCleaned/koppel_INTRIX_StationsOnlyShortest.csv")
data_sjoerd_alles<- read.csv("data/uitwisseling/combineLinePointsCleaned/koppel_INTRIX_Stations.csv")

df$SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$SEGMENT_ID[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]
df$dist_SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$dist[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]
df$orient_deg_SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$orientation_deg[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]

##exploration
library(ggplot2)
#hoe zijn de metingen verdeeld per station?
ggplot(df,aes(x=stn_ID,y=NO2))+geom_boxplot()
ggplot(df,aes(x=stn_ID,y=PM10))+geom_boxplot()+ylim(0,150)
#zijn er dagelijkse patronen zichtbaar?
ggplot(df,aes(x=factor(HH),y=NO2))+geom_boxplot()+facet_wrap(~stn_ID)+ylim(0,100)
ggplot(df,aes(x=factor(HH),y=PM10))+geom_boxplot()+facet_wrap(~stn_ID)+ylim(0,70)
#df vs PM10
ggplot(df,aes(x=PM10,y=NO2))+geom_point(alpha=0.1)+facet_wrap(~stn_ID)+xlim(0,150)+ylim(0,100)



write.csv(df,file="data/dataframe_11u30.csv")
