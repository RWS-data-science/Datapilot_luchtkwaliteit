#Load the Aerias data provided by Bas
NO2 <- read.csv("data/data_knmi_bas2/aireas/no2/aireas_lml_2016.csv",header = T)

#melt to get all measurement data into one column
library(reshape)
NO2<- melt(NO2,id.vars = "dateTime",variable_name = "stn_ID",na.rm = T)
NO2$stn_ID <- as.factor(gsub("X","",NO2$stn_ID)) #remove X's

#add station locations 
stations<- readxl::read_xlsx("data/data_knmi_bas2/aireas/Locatie LML en AiREAS.xlsx",col_types = "guess")

NO2<- merge(NO2,stations,by.x = "stn_ID", by.y = "LML ID")
NO2$Postcode<- NO2$X__1
NO2$X__1<- NULL
NO2$Lat <- as.numeric(NO2$Lat)
NO2$Lon <- as.numeric(NO2$Lon)
NO2$dateTime <- as.POSIXct(strptime(NO2$dateTime,"%Y-%m-%d %H:%M:%S")) ##aangenomen dat alle tijden UTC zijn

#add weather data (Temperature, wind, etc...)
meteo <- read.table("data/data_knmi_bas2/knmi_meteo/KNMI_Eindhoven_201501-201710_hourly.dat",sep=",",skip = 31,header=TRUE)
colnames(meteo)<- gsub(" |#","",strsplit(readLines("data/data_knmi_bas2/knmi_meteo/KNMI_Eindhoven_201501-201710_hourly.dat",n=33)[33],",")[[1]])
meteo$datetime<- as.POSIXct(strptime(paste(meteo$YYYYMMDD,meteo$HH),"%Y%m%d %H"))

NO2 <-merge(NO2,meteo,by.x="dateTime",by.y="datetime")


library(ggplot2)
#hoe zijn de metingen verdeeld per station?
ggplot(NO2,aes(x=stn_ID,y=value))+geom_boxplot()
#zijn er dagelijkse patronen zichtbaar?
ggplot(NO2,aes(x=factor(HH),y=value))+geom_boxplot()+facet_wrap(~stn_ID)+ylim(0,100)
