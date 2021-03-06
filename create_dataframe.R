#Load the Aerias data provided by Bas
library(data.table)
library(dplyr)

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

df <-merge(df,meteo,by.x="dateTime",by.y="datetime",all.x = T)


##PM10
PM10<- read.csv("data/data_knmi_bas_final/aireas/aireas_lml_2016.pm10.csv")
PM10<- melt(PM10,id.vars = "dateTime",variable_name = "stn_ID",na.rm = T)
PM10$stn_ID <- as.factor(gsub("X","",PM10$stn_ID)) #remove X's

PM10$dateTime <- as.POSIXct(strptime(PM10$dateTime,"%Y-%m-%d %H:%M:%S"))
PM10$PM10 <- PM10$value
PM10$value<- NULL

df<- merge(df,PM10,by=c("dateTime","stn_ID") ,all.x = T)

##Data CAMS
cams<- read.csv("data/data_knmi_bas_final/cams/cams_Eindhoven_2016_NOx.csv")
cams$dateTime <- as.POSIXct(strptime(cams$dateTime,"%Y-%m-%d %H:%M:%S"))
colnames(cams)<- paste0("cams_",colnames(cams))

df<- merge(df,cams,by.x = "dateTime",by.y = "cams_dateTime",all.x = T)

##Data van Sjoerd
data_sjoerd_onlyshortest<- read.csv("data/uitwisseling/combineLinePointsCleaned/koppel_INTRIX_StationsOnlyShortest.csv")
data_sjoerd_alles<- read.csv("data/uitwisseling/combineLinePointsCleaned/koppel_INTRIX_Stations.csv")

df$SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$SEGMENT_ID[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]
df$dist_SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$dist[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]
df$orient_deg_SEGMENT_ID_nearest <- data_sjoerd_onlyshortest$orientation_deg[match(df$stn_ID,data_sjoerd_onlyshortest$id_Aireas)]


##data CBS

cbs50<- read.csv("data/CBSinvoer/83618NED_TypedDataSet_31102017_102723_50m_out.csv")
cbs50$stationID[(nrow(cbs50)-2):nrow(cbs50)]<- paste0("NL",cbs50$stationID[(nrow(cbs50)-2):nrow(cbs50)])
colnames(cbs50)<- paste0("cbs50_",colnames(cbs50))
df<- merge(df, cbs50, by.x="stn_ID",by.y= "cbs50_stationID",all.x = T)

cbs100<- read.csv("data/CBSinvoer/83618NED_TypedDataSet_31102017_102723_100m_out.csv")
cbs100$stationID[(nrow(cbs100)-2):nrow(cbs100)]<- paste0("NL",cbs50$stationID[(nrow(cbs100)-2):nrow(cbs100)])
colnames(cbs100)<- paste0("cbs100_",colnames(cbs100))
df<- merge(df, cbs100, by.x="stn_ID",by.y= "cbs100_stationID",all.x = T)

cbs200<- read.csv("data/CBSinvoer/83618NED_TypedDataSet_31102017_102723_200m_out.csv")
cbs200$stationID[(nrow(cbs200)-2):nrow(cbs200)]<- paste0("NL",cbs50$stationID[(nrow(cbs200)-2):nrow(cbs200)])
colnames(cbs200)<- paste0("cbs200_",colnames(cbs200))
df<- merge(df, cbs200, by.x="stn_ID",by.y= "cbs200_stationID",all.x = T)

# ##exploration
# library(ggplot2)
# #hoe zijn de metingen verdeeld per station?
# ggplot(df,aes(x=stn_ID,y=NO2))+geom_boxplot()
# ggplot(df,aes(x=stn_ID,y=PM10))+geom_boxplot()+ylim(0,150)
# #zijn er dagelijkse patronen zichtbaar?
# ggplot(df,aes(x=factor(HH),y=NO2))+geom_boxplot()+facet_wrap(~stn_ID)+ylim(0,100)
# ggplot(df,aes(x=factor(HH),y=PM10))+geom_boxplot()+facet_wrap(~stn_ID)+ylim(0,70)
# #df vs PM10
# ggplot(df,aes(x=PM10,y=NO2))+geom_point(alpha=0.1)+facet_wrap(~stn_ID)+xlim(0,150)+ylim(0,100)


##aantal wegen
library(rgdal)
library(rgeos)

nwb<- readOGR("data/Verkeer/Shapes/NWB/NWB_Eindhoven_WGS84.shp")
nwb$Lengte_m <- as.numeric(as.character(nwb$Lengte_m))

stations<- readxl::read_xlsx("data/data_knmi_bas_final/aireas/Locatie LML en AiREAS.xlsx",col_types = "guess")
stations$Lat<- as.numeric(stations$Lat)
stations$Lon<- as.numeric(stations$Lon)

stations<- stations[which(stations$Lat>0),] #remove NA's

coordinates(stations) <- ~Lon+Lat
proj4string(stations) <- proj4string(nwb)

#convert to rd (for unit meters)
rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs "

stations<- spTransform(stations, rd)
nwb<- spTransform(nwb, rd)


st<- data.frame("stn_ID" = unique(stations$`LML ID`))


for (station in unique(stations$`LML ID`)){
  buf50<- gBuffer(stations[which(stations$`LML ID` == station),],width = 50)
  buf100<- gBuffer(stations[which(stations$`LML ID` == station),],width = 100)
  buf200<- gBuffer(stations[which(stations$`LML ID` == station),],width = 200)
  
  nwb_sel50<- nwb[buf50,]
  nwb_sel100<- nwb[buf100,]
  nwb_sel200<- nwb[buf200,]
  
  st$len_wegen_50_tot[which(st$stn_ID == station)] <-  sum(nwb_sel50$Lengte_m,na.rm = T) 
  st$len_wegen_100_tot[which(st$stn_ID == station)] <-  sum(nwb_sel100$Lengte_m,na.rm = T) 
  st$len_wegen_200_tot[which(st$stn_ID == station)] <-  sum(nwb_sel200$Lengte_m,na.rm = T)
  
  st$len_wegen_50_gem[which(st$stn_ID == station)] <-  sum(nwb_sel50@data[nwb_sel50@data$WEGBEHSRT == "G","Lengte_m"],na.rm = T) 
  st$len_wegen_100_gem[which(st$stn_ID == station)] <-  sum(nwb_sel100@data[nwb_sel100@data$WEGBEHSRT == "G","Lengte_m"],na.rm = T) 
  st$len_wegen_200_gem[which(st$stn_ID == station)] <-  sum(nwb_sel200@data[nwb_sel200@data$WEGBEHSRT == "G","Lengte_m"],na.rm = T) 
  
  st$len_wegen_50_prov[which(st$stn_ID == station)] <-  sum(nwb_sel50@data[nwb_sel50@data$WEGBEHSRT == "P","Lengte_m"],na.rm = T) 
  st$len_wegen_100_prov[which(st$stn_ID == station)] <-  sum(nwb_sel100@data[nwb_sel100@data$WEGBEHSRT == "P","Lengte_m"],na.rm = T) 
  st$len_wegen_200_prv[which(st$stn_ID == station)] <-  sum(nwb_sel200@data[nwb_sel200@data$WEGBEHSRT == "P","Lengte_m"],na.rm = T) 
  
  st$len_wegen_50_rijk[which(st$stn_ID == station)] <-  sum(nwb_sel50@data[nwb_sel50@data$WEGBEHSRT == "R","Lengte_m"],na.rm = T) 
  st$len_wegen_100_rijk[which(st$stn_ID == station)] <-  sum(nwb_sel100@data[nwb_sel100@data$WEGBEHSRT == "R","Lengte_m"],na.rm = T) 
  st$len_wegen_200_rijk[which(st$stn_ID == station)] <-  sum(nwb_sel200@data[nwb_sel200@data$WEGBEHSRT == "R","Lengte_m"],na.rm = T) 
  
  
}


df<- merge(df,st,by="stn_ID",all.x= T)



##Data skyview factor en gebouwen
skyview<- read.csv("data/DGMI-DATAMATCH/dataDelivered/stationsWithSkyViewFactor.csv")
df$svf <- skyview$svf[match(df$stn_ID,skyview$LML.ID)]
  
boundery_layer<- read.csv("data/DGMI-DATAMATCH/dataDelivered/stationsBoundaryLayerHeight1HourInterpol.csv")
boundery_layer$dateTime <- as.POSIXct(strptime(boundery_layer$timeStamp,"%Y-%m-%d %H:%M:%S"))

df<- merge(df,boundery_layer[,c("LML.ID","dateTime","BoundaryLayerHeight.m.")],by.x=c("stn_ID","dateTime"),by.y=c("LML.ID","dateTime"),all.x = T)

heigth_around<-  read.csv("data/DGMI-DATAMATCH/dataDelivered/stationsWithHeightAround.csv")
df$height_around_mean50 <- heigth_around$mean50[match(df$stn_ID,heigth_around$LML.ID)]
df$height_around_sd50 <- heigth_around$sd50[match(df$stn_ID,heigth_around$LML.ID)]
df$height_around_mean100 <- heigth_around$mean100[match(df$stn_ID,heigth_around$LML.ID)]
df$height_around_sd100 <- heigth_around$sd100[match(df$stn_ID,heigth_around$LML.ID)]
df$height_around_mean200 <- heigth_around$mean200[match(df$stn_ID,heigth_around$LML.ID)]
df$height_around_sd200 <- heigth_around$sd200[match(df$stn_ID,heigth_around$LML.ID)]




##convert factors to numeric for some of the cbs vars
df$cbs100_GemiddeldeWoningwaarde_35<- as.numeric(as.character(df$cbs100_GemiddeldeWoningwaarde_35))
df$cbs100_BouwjaarVoor2000_45<- as.numeric(as.character(df$cbs100_BouwjaarVoor2000_45))
df$cbs100_BouwjaarVanaf2000_46 <- as.numeric(as.character(df$cbs100_BouwjaarVanaf2000_46 ))
df$cbs200_GemiddeldeWoningwaarde_35<- as.numeric(as.character(df$cbs200_GemiddeldeWoningwaarde_35))
df$cbs200_BouwjaarVoor2000_45<- as.numeric(as.character(df$cbs200_BouwjaarVoor2000_45))
df$cbs200_BouwjaarVanaf2000_46 <- as.numeric(as.character(df$cbs200_BouwjaarVanaf2000_46 ))
df$cbs200_GemiddeldElektriciteitsverbruikTotaal_47<- as.numeric(as.character(df$cbs200_GemiddeldElektriciteitsverbruikTotaal_47))
df$cbs200_GemiddeldAardgasverbruikTotaal_55<- as.numeric(as.character(df$cbs200_GemiddeldAardgasverbruikTotaal_55))

# df$height_aorund_mean50<-NULL
# df$height_aorund_mean100<-NULL
# df$height_aorund_mean200<-NULL
# 
# height_aorund_sd50 <- NULL
# height_aorund_sd100 <- NULL
# height_aorund_sd200 <- NULL

##NSL data Sjoerd
NSL<- read.csv("data/LinkNSLtoAireasLML/relativeAngleOnlyShortest.csv")
NSL$timestamp <- as.POSIXct(strptime(NSL$timestamp,"%d-%m-%Y %H:%M"))

colnames(NSL)<- paste0("NSL_",colnames(NSL))


df<- merge(df, NSL, by.x=c("stn_ID","dateTime"),by.y = c("NSL_id_Aireas","NSL_timestamp"),all.x=T)



#####Add speed data (Marlous)
#speed<- lapply(list.files("data/Speed/"),function(x){fread(paste0("data/Speed/",x),stringsAsFactors = TRUE)}) 
#speed<- rbindlist(speed)
speed<- fread("data/20171208 Speed Inrix.csv")

# ref<- list()
# # ref_files<- c("data/LinkFCDtoAireasLML/koppel_1602_StationsOnlyShortest.csv","data/LinkFCDtoAireasLML/koppel_1502_StationsOnlyShortest.csv",
# #               "data/LinkFCDtoAireasLML/koppel_1601_StationsOnlyShortest.csv")
# #ref<- rbindlist(lapply(ref_files,function(x){cbind(x,read.csv(x))} ),fill = T)
# ref[[1]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1602_StationsOnlyShortest.csv")
# ref[[1]]$basemap <- 1602
# ref[[2]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1502_StationsOnlyShortest.csv")
# ref[[2]]$basemap <- 1502
# ref[[3]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1601_StationsOnlyShortest.csv")
# ref[[3]]$basemap <- 1601
# 
# ref<- rbindlist(ref,fill=T)
# 
# 
# ref<- merge(ref[,c("id_Aireas","XDSegID","basemap")],speed, by.x=c("XDSegID","basemap"),by.y=c("segmentid","basemapid"),all.x=TRUE)
# ref$`datetime (UTC)`<- as.POSIXct(strptime(ref$`datetime (UTC)`,"%Y-%m-%d %H:%M:%S"))
# 
# df<- merge(df,ref,by.x=c("stn_ID","dateTime"),by.y=c("id_Aireas","datetime (UTC)"),all.x=TRUE)


#reftabel FCD to aireas for all ids
ref_250<- list()

ref_250[[1]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1502_StationsWithin250Radius.csv")
ref_250[[1]]$basemap <- 1502
ref_250[[2]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1602_StationsWithin250Radius.csv")
ref_250[[2]]$basemap <- 1602
ref_250[[3]]<- read.csv("data/LinkFCDtoAireasLML/koppel_1601_StationsWithin250Radius.csv")
ref_250[[3]]$basemap <- 1601

ref_250<- as.data.frame(rbindlist(ref_250,fill=T))

ref_250$code<- paste(ref_250$basemap,ref_250$XDSegID,sep="_")
#ref_250$timestamp<- as.POSIXct(strptime(ref_250$timestamp,"%d-%m-%Y %H:%M"))

speed<- as.data.frame(speed)
speed<- speed[which(speed$code %in% ref_250$code),]

speed_merged<- merge(ref_250[,c("id_Aireas","code","dist")],speed,by="code",all=T)

calc <- function(sp,d){
  sum(sp/d)/sum(1/d) }
calc2<- function(sp,d){
  sum(sp/d)/length(d)
}
speed_agg<- speed_merged %>%
  group_by(id_Aireas,datetime) %>% 
  summarise(FCD_weighted_speedfactor=calc(speedfactor,dist),FCD_weighted_speed=calc(speed,dist),FCD_weighted_max_speed= calc(max_speed,dist),
            FCD_speedfactor_by_dist = calc2(speedfactor,dist))

speed_agg$datetime<- as.POSIXct(strptime(speed_agg$datetime,"%Y-%m-%d %H:%M:%S"))

#merge
df<- merge(df,speed_agg,by.x=c("stn_ID","dateTime"),by.y=c("id_Aireas","datetime"),all.x=TRUE)


###2017-12-08 Nieuwe verkeersdata Marlous

fcd_flow<- fread("data/20171207 Koppeling INRIX FCD-flow NDW Eindhoven.csv")

fcd_flow$datetime <- as.POSIXct(strptime(fcd_flow$datetime,"%Y-%m-%d %H:%M:%S"))

fcd_flow<- fcd_flow[which(fcd_flow$code %in% ref_250$code),]

int_merged<- merge(ref_250[,c("id_Aireas","code","dist")],fcd_flow[,c("code","datetime","cat_all","cat_1_3","cat_2_3","cat_3_3","cat_1_5","cat_2_5","cat_3_5","cat_4_5","cat_5_5","categorie" )],by="code",all=T)

int_agg<- int_merged %>%
  group_by(id_Aireas,datetime) %>%
  summarise(weighted_int_all = calc(cat_all, dist),weighted_int_cat_1_3 = calc(cat_1_3, dist),weighted_int_cat_2_3 = calc(cat_2_3, dist),weighted_int_cat_3_3 = calc(cat_3_3, dist),
            weighted_int_cat_1_5 = calc(cat_1_5, dist),weighted_int_cat_2_5 = calc(cat_2_5, dist),weighted_int_cat_3_5 = calc(cat_3_5, dist),weighted_int_cat_4_5 = calc(cat_4_5, dist),
            weighted_int_cat_5_5 = calc(cat_5_5, dist))


df<- merge(df,int_agg,by.x=c("stn_ID","dateTime"),by.y=c("id_Aireas","datetime"),all.x=TRUE)

###save
save(df,file="data/dataframe3_15u00.RData")
write.csv(df,file="data/dataframe3_15u00.csv")

