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

h2o.varimp_plot(model)

h2o_y_test <- h2o.predict(model, test_h2o)


test$pred<-  as.data.frame(h2o_y_test)

#calculate rmse per station
rmse_tot <- sqrt(mean((test$NO2-test$pred)^2,na.rm = TRUE))

rmse <- test %>% group_by(stn_ID) %>%
  summarize(rmse=sqrt(mean((NO2-pred)^2,na.rm = TRUE)))

rmse

ggplot(test,aes(x=dateTime,y=NO2))+geom_line()+geom_line(aes(y=pred),col="red")+facet_wrap(~stn_ID)



###Auto-ML
aml <- h2o.automl(x = c(2:ncol(train)), y = 1,
                  training_frame = train_h2o,max_runtime_secs=120)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb


# The leader model is stored here
aml@leader

h2o.varimp_plot(aml@leader)


h2o_y_test_aml<- h2o.predict(aml, test_h2o)


test$pred_aml<-  as.data.frame(h2o_y_test_aml)

#calculate rmse per station
rmse_tot_aml <- sqrt(mean((test$NO2-test$pred_aml)^2,na.rm = TRUE))

rmse_aml <- test %>% group_by(stn_ID) %>%
  summarize(rmse=sqrt(mean((NO2-pred_aml)^2,na.rm = TRUE)))

rmse_aml

ggplot(test[test$dateTime<"2016-04-01 00:00:00",],aes(x=dateTime,y=NO2))+geom_line()+geom_line(aes(y=pred_aml),col="red")+geom_line(aes(y=cams_NO2),col="blue",alpha=0.5)+facet_wrap(~stn_ID)





## Hyper-Parameter Search for GBM

## Construct a large Cartesian hyper-parameter space
ntrees_opts <- c(10000) ## early stopping will stop earlier
max_depth_opts <- seq(1,20)
min_rows_opts <- c(1,5,10,20,50,100)
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
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 600, max_models = 100, stopping_metric = "AUTO", stopping_tolerance = 0.00001, stopping_rounds = 5, seed = 123456)

gbm.grid <- h2o.grid("gbm",
                     grid_id = "mygrid",
                     x=c(2:ncol(train)), y = 1,
                     
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
                     stopping_metric = "MSE",
                     
                     score_tree_interval = 100, ## how often to score (affects early stopping)
                     seed = 123456, ## seed to control the sampling of the Cartesian hyper-parameter space
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm.sorted.grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "mse")
print(gbm.sorted.grid)

best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)
h2o.varimp_plot(best_model)


h2o_y_test_grid<- h2o.predict(best_model, test_h2o)


test$pred_grid<-  as.data.frame(h2o_y_test_grid)

#calculate rmse per station
sqrt(mean((test$NO2-test$pred_grid)^2,na.rm = TRUE))

rmse_grid <- test %>% group_by(stn_ID) %>%
  summarize(rmse=sqrt(mean((NO2-pred_grid)^2,na.rm = TRUE)))

rmse_grid

ggplot(test,aes(x=dateTime,y=NO2))+geom_line()+geom_line(aes(y=pred_grid),col="red",alpha=0.7)+facet_wrap(~stn_ID,scales = "free")


