#Load the Aerias data provided by Bas
NO2 <- read.csv("data/data_knmi_bas2/aireas/no2/aireas_lml_2016.csv",header = T)

#melt to get all measurement data into one column
library(reshape)
NO2<- melt(NO2,id.vars = "dateTime",variable_name = "stn_ID",na.rm = F)
NO2$stn_ID <- as.factor(gsub("X","",NO2$stn_ID)) #remove X's

#add station locations and stationary data
stations<- readxl::read_xlsx("data/data_knmi_bas2/aireas/Locatie LML en AiREAS.xlsx",col_types = "guess")

NO2<- merge(NO2,stations,by.x = "stn_ID", by.y = "LML ID")
NO2$Postcode<- NO2$X__1
NO2$X__1<- NULL
NO2$Lat <- as.numeric(NO2$Lat)
NO2$Lon <- as.numeric(NO2$Lon)


