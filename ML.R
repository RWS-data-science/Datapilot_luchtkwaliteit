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


##ML met H2o
localH2O = h2o.init(max_mem_size = '12g', 
                    nthreads = -1) 


train_h2o = as.h2o(train)
test_h2o = as.h2o(test)

model =
  h2o.gbm(x = c(1:(n-2),(n+1):nrow(train)),  # column numbers for predictors
          y = n,   # column number for label
          training_frame = train_h2o) # data in H2O format


h2o_y_test <- h2o.predict(model, test_h2o)


df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame(ImageId = seq(1,length(df_y_test$predict)), Label = df_y_test$predict)

