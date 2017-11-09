##ML_routine

df<- read.csv("data/dataframe_15u42.csv")

df_prep<- df

#remove variables with no value
df_prep$X<- NULL
df_prep$City<- NULL
df_prep$Species<- NULL
df_prep$STN<- NULL





xnam <- colnames(df_prep)
xnam<- xnam[! xnam %in% c("NO2")]
fmla <- as.formula(paste("NO2 ~ ", paste(xnam, collapse= "+")))

stns<- sample(unique(df_prep$stn_ID),15) ## 15 station voor training, 7 voor testen (random geselecteerd)

train<- df_prep[df_prep$stn_ID %in% stns,]
test<- df_prep[!df_prep$stn_ID %in% stns,]
