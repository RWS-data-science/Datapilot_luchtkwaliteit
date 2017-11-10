##ML_routine
library(h2o)
library(dplyr)
library(ggplot2)



load("data/dataframe_16_32.RData")

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

n<-as.numeric(which(colnames(train)=="NO2"))


##ML met H2o
localH2O = h2o.init(max_mem_size = '12g', 
                    nthreads = -1) 


train_h2o = as.h2o(train[,3:ncol(test)])
test_h2o = as.h2o(test[,3:ncol(test)])

model =
  h2o.gbm(x = c(2:ncol(train)),  # column numbers for predictors
          y = 1,   # column number for label
          training_frame = train_h2o) # data in H2O format


h2o_y_test <- h2o.predict(model, test_h2o)


test$pred<-  as.data.frame(h2o_y_test)

#calculate rmse per station
rmse_tot <- sqrt(mean((test$NO2-test$pred)^2,na.rm = TRUE))

rmse <- test %>% group_by(stn_ID) %>%
  summarize(rmse=sqrt(mean((NO2-pred)^2,na.rm = TRUE)))

rmse

ggplot(test,aes(x=dateTime,y=NO2))+geom_line(col="blue")+geom_line(aes(y=pred),col="red")+facet_wrap(~stn_ID)

aml <- h2o.automl(x = c(2:ncol(train)), y = 1,
                  training_frame = train_h2o)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb


# The leader model is stored here
aml@leader



pred <- h2o.predict(aml@leader, test)



h2o.shutdown(prompt=F)
