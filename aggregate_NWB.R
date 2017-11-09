library(rgdal)

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

plot(nwb)  
plot(stations,col="red",add=T,type="p",size=2)

library(rgeos)
buf100<- gBuffer(stations, byid=T,width = 100)
buf200<- gBuffer(stations, byid=T,width = 200)
buf500<- gBuffer(stations, byid=T,width = 500)

plot(buf500,col="red",add=T)



nwb_sel100<- nwb[buf100,]
nwb_sel200<- nwb[buf200,]
nwb_sel500<- nwb[buf500,]

plot(buf500,col="red")
plot(nwb_sel500,add=T)

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


df<- merge(df,st,by="stn_ID")
