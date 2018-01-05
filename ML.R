##ML_routine
library(h2o)
library(dplyr)
library(ggplot2)



#load("data/dataframe2_14u12.RData")
load("data/dataframe19-12-2017_11u25.RData") #update van Maarten met correctie voor drifts
df<- df.corrected

df_prep<- df

#remove variables with no value
df_prep$X<- NULL
df_prep$Street<- NULL
df_prep$City<- NULL
df_prep$Species<- NULL
df_prep$STN<- NULL
df_prep$YYYYMMDD<- NULL
df_prep$basemap <- NULL

df_prep$HH<- as.factor(df_prep$HH)


stns<- sample(unique(df_prep$stn_ID),15) ## 15 station voor training, 7 voor testen (random geselecteerd)

train<- df_prep[df_prep$stn_ID %in% stns,]
test<- df_prep[!df_prep$stn_ID %in% stns,]

n<-as.numeric(which(colnames(train)=="NO2"))


##ML met H2o
localH2O = h2o.init(max_mem_size = '24g', 
                    nthreads = 60) 


train_h2o = as.h2o(train[,3:ncol(test)])
test_h2o = as.h2o(test[,3:ncol(test)])



## Hyper-Parameter Search for GBM

## Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(10000) ## early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(10,20,50)
learn_rate_opts <- seq(0.001,0.01,0.001)
sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_opts <- seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
#nbins_cats_opts = seq(100,10000,100) ## no categorical features in this dataset

hyper_params = list( ntrees = ntrees_opts,
                     max_depth = max_depth_opts,
                     min_rows = min_rows_opts,
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)


## Search a random subset of these hyper-parmameters (max runtime and max models are enforced, and the search will stop after we don't improve much over the best 5 random models)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 600, max_models = 100, stopping_metric = "RMSE", stopping_tolerance = 0.00001, stopping_rounds = 5)

gbm.grid <- h2o.grid("gbm",
                     
#                     x=c(2:ncol(train)), y = 1,            #without corrected NO2
                     x=c(2:(h2o.ncol(train_h2o)-1)),y=h2o.ncol(train_h2o), #corrected NO2 is the final column of the df
                     # faster to use a 80/20 split
                     training_frame = train_h2o,
                     validation_frame = test_h2o,
                     nfolds = 0,
                     
                     # alternatively, use N-fold cross-validation
                     #training_frame = train,
                     #nfolds = 5,
                     
                     distribution="gaussian", ## best for MSE loss, but can try other distributions ("laplace", "quantile")
                     
                     ## stop as soon as mse doesn't improve by more than 0.1% on the validation set,
                     ## for 2 consecutive scoring events
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "RMSE",
                     
                     score_tree_interval = 100, ## how often to score (affects early stopping)
                     #seed = 123456, ## seed to control the sampling of the Cartesian hyper-parameter space
                    hyper_params = hyper_params,
                     search_criteria = search_criteria
)

gbm.sorted.grid <- h2o.getGrid(grid_id = gbm.grid@grid_id, sort_by = "mse")
print(gbm.sorted.grid)

best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)
h2o.varimp_plot(best_model,num_of_features = 15)


h2o_y_test_grid<- h2o.predict(best_model, test_h2o)


test$pred_grid<-  as.data.frame(h2o_y_test_grid)

#calculate rmse per station
sqrt(mean((test$NO2_corrected-test$pred_grid)^2,na.rm = TRUE))

rmse_grid <- test %>% group_by(stn_ID) %>%
  summarize(rmse=sqrt(mean((NO2_corrected-pred_grid)^2,na.rm = TRUE)))

rmse_grid<- as.data.frame(rmse_grid)

ggplot(test,aes(x=dateTime))+geom_line(aes(y=NO2_corrected),alpha=0.7)+geom_line(aes(y=pred_grid),col="red",alpha=0.7)+facet_wrap(~stn_ID)+ ylim(0,100) 
              

  #geom_smooth(aes(y=NO2_corrected),col="black") + geom_smooth(aes(y=pred_grid),col="red")

ggplot(test[which(test$dateTime<"2016-03-01 00:00:00"& test$stn_ID!=3),],aes(x=dateTime))+geom_line(aes(y=NO2_corrected),alpha=0.7)+geom_line(aes(y=pred_grid),col="red",alpha=0.7)+
  facet_wrap(~stn_ID)+ ylim(0,100)+
  geom_text(data=data.frame(x=as.POSIXct("2016-01-01 00:00:00"), y=100, label=paste("RMSE=",round(rmse_grid$rmse,2)), stn_ID=rmse_grid$stn_ID), 
            aes(x,y,label=label), inherit.aes=FALSE,size=3,hjust=0)


