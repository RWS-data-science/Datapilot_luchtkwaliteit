
# Try to detrend one of the arias stations.
library(ggplot2)
library(GGally)
library(reshape2)
library(chron)
library(lubridate)
library(splines)

Sys.setenv(TZ = "UTC")

load(file =  "data/dataframe08122017.bin")

# reference station ? NL10236
# take monthly averages and caculate ratio wrt NL10236
# see the change between jan2016 and dec2016

data <- subset(data, 
  dateTime != as.POSIXct("2017-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

# define maand
data <- within(data, maand <- month(dateTime))

# maak gemiddeldes van NO2 voor alle stations per maand
avg.mnth.stn <- aggregate(NO2~maand+stn_ID, data = data, FUN = mean)

# take NL10236 as a reference
reference <- subset(avg.mnth.stn, stn_ID == "NL10236", select = c(maand, NO2))

# bereken ratio wrt reference

avg.mnth.stn <- within(avg.mnth.stn, {
  ref <- rep(reference, 20)
  ratio <- NO2/ref
})

# point plot
ggplot(avg.mnth.stn, aes(x = maand, y = ratio)) + 
  geom_point() +
  facet_wrap(~stn_ID)

# smooth plot
ggplot(avg.mnth.stn, aes(x = maand, y = ratio)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~stn_ID)


# additief model
data_sub <- subset(data, select = c(dateTime, maand, stn_ID, NO2))

data_sub <- within(data_sub, {
  hours <- as.numeric((dateTime-as.POSIXct("2016-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC"))/3600)
})

data_sub_ref <- subset(data_sub, stn_ID == "NL10236", select = c(hours, NO2))
names(data_sub_ref)[2] <- "ref" 
  
subdata <- merge(data_sub, data_sub_ref, all = TRUE)

# regressie om straks te kunnen voorspellen:
# mod1 <- mod2 <- with(subdata, lm(log(NO2) ~ offset(log(ref)) + hours*stn_ID))
# 
# subdata <- within(subdata, {
#   p1 <- exp(predict(mod1, newdata= subdata))
#   p2 <- exp(predict(mod2, newdata= within(subdata, hours <-0)))
#   ratio <- NO2/ref
#   pred1 <- p1/ref
#   pred2 <- p2/ref
# })
# 
# 
# ggplot(subset(subdata,  stn_ID == "12"), aes(x = hours, y = NO2)) + 
#   geom_line(aes(y = p1), col = "red") +
#   geom_line(aes(y = p2), col = "green") +
#   facet_wrap(~stn_ID)
# 
# 
# avg.mnth.stn2 <- aggregate(cbind(ratio, pred1, pred2)~maand+stn_ID, data = subdata, FUN = mean)
# 
# # smooth plot
# ggplot(avg.mnth.stn2, aes(x = maand, y = ratio)) + 
#   geom_line() + geom_smooth(method = "lm") +
#   geom_smooth(aes(y = pred1), method = "lm", col = "red") +
#   geom_smooth(aes(y = pred2), method = "lm", col = "green") +
#   facet_wrap(~stn_ID)


# try the same, but now with an additive model:
# so model NO2 with time and station:
mod.add <- with(subdata, lm(NO2~ offset(ref) + hours*stn_ID))
subdata <- within(subdata, {
  pred.NO2 <- predict(mod.add, newdata = subdata)
  pred.NO2.detrend <- predict(mod.add, newdata = within(subdata, hours<-0))
  errors <- NO2 - pred.NO2 
  # do not detrend signal for LML stations
  NO2.detrend <- ifelse(test = grepl(pattern = "NL", x = stn_ID),
    yes = NO2, no = pred.NO2.detrend + errors)
  stn_ID <- factor(stn_ID, 
    levels = levels(stn_ID)[order(as.numeric(unlist(regmatches(
      x = levels(stn_ID), m = gregexpr(
        pattern = "[[:digit:]]+", 
        text = levels(stn_ID))))))])
})

avg.mnth.stn.add <- aggregate(cbind(NO2, pred.NO2, pred.NO2.detrend, NO2.detrend)~maand+stn_ID, data = subdata, FUN = mean)

# smooth plot
ggplot(avg.mnth.stn.add, aes(x = maand, y = NO2)) + 
  geom_line() + geom_smooth(method = "lm") +
  geom_smooth(aes(y = pred.NO2), method = "lm", col = "red")  +
  geom_smooth(aes(y = pred.NO2.detrend), method = "lm", col = "green")  +
  geom_smooth(aes(y = NO2.detrend), method = "lm", col = "blue")  +
  facet_wrap(~stn_ID)

# predictions are only available for hours where station NL10236 has a measurement
# so introducing 2299 extra NA's

ggplot(avg.mnth.stn.add, aes(x = maand, y = NO2)) + 
  geom_line() +
  geom_smooth(aes(y = pred.NO2), col = "red")  +
  geom_smooth(aes(y = pred.NO2.detrend), col = "green")  +
  geom_smooth(aes(y = NO2.detrend), col = "blue")  +
  facet_wrap(~stn_ID, labeller = label_both) +
  labs(title = "Original signal and (linear) detrended signal per station",  x = "month" ) +
  scale_x_continuous(breaks = 1:12, labels = 1:12)

ggplot(avg.mnth.stn.add, aes(x = maand, y = NO2-pred.NO2)) + 
  geom_smooth(col = "red")  +
  geom_smooth(aes(y = NO2 - pred.NO2.detrend), col = "green")  +
  geom_smooth(aes(y = NO2 - NO2.detrend), col = "blue")  +
  facet_wrap(~stn_ID, labeller = label_both) +
  labs(title = "Original signal and (linear) detrended signal per station",  x = "month" ) +
  scale_x_continuous(breaks = 1:12, labels = 1:12)

presetter <- function(string, prefix = "AiREAS ") paste0(prefix, string)
cols <- RColorBrewer::brewer.pal(9, "Set1")[1:2]
alfa <- .3
ggplot(subset(subdata, !grepl(pattern = "NL", x = stn_ID)), aes(x = dateTime, y = NO2)) + 
  geom_line(col = cols[1], alpha = alfa) +
  geom_line(aes(y = NO2.detrend), col = cols[2], alpha = alfa) +
  geom_smooth(aes(y = NO2), method = "lm", formula = y~ns(x, df = 2), col = cols[1])  +
  geom_smooth(aes(y = NO2.detrend), col = cols[2])  +
  coord_cartesian(ylim = c(0, 150)) +
  facet_wrap(~stn_ID, nrow = 3, labeller = as_labeller(presetter)) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b") +
  labs(x = "2016", title = "Original signal and (linear) detrended signal per AiREAS station")


newdata <- rbind(
  x1 <- subset(subdata, !grepl(pattern = "NL", x = stn_ID), 
    select = c(dateTime, stn_ID, NO2, NO2.detrend)),
  x2 <- subset(avg.mnth.stn.add, !grepl(pattern = "NL", x = stn_ID), 
    select = c(dateTime, stn_ID, NO2, NO2.detrend)))
newdata <- within(newdata, {
  soort <- factor(rep(c("hour", "month"), times = c(nrow(x1), nrow(x2))))
})

nieuwdata <- melt(newdata, measure.vars = c("NO2", "NO2.detrend"))

ggplot(nieuwdata, aes(x = dateTime, y = value, colour = variable, 
  group = interaction(soort, variable))) + 
  geom_line(aes(alpha = soort)) +
  scale_alpha_discrete(name = "", range = c(.3, 1)) +
  scale_colour_brewer(name = "", type = "qual", palette = 6) +
  coord_cartesian(ylim = c(0, 150)) +
  facet_wrap(~stn_ID, nrow = 3, labeller = as_labeller(presetter))
  


avg.mnth.stn.add <- aggregate(cbind(NO2, pred.NO2, pred.NO2.detrend, NO2.detrend)~maand+stn_ID, data = subdata, FUN = mean)
cols <- RColorBrewer::brewer.pal(9, "Paired")[c(5, 6, 1, 2)]
presetter <- function(string, prefix = "AiREAS ") paste0(prefix, string)

ggplot(subset(subdata, !grepl(pattern = "NL", x = stn_ID)), aes(x = dateTime, y = NO2)) + 
  geom_line(aes(colour = "col1", alpha = "original")) +
  geom_line(aes(y = NO2.detrend, colour = "col3", alpha = "original")) +
  geom_smooth(data = subset(avg.mnth.stn.add, !grepl(pattern = "NL", x = stn_ID)), 
    aes(colour = "col2", alpha = "smoothed"), se = FALSE)  +
  geom_smooth(data = subset(avg.mnth.stn.add, !grepl(pattern = "NL", x = stn_ID)),
    aes(y = NO2.detrend, colour = "col4", alpha = "smoothed"), se = FALSE)  +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~stn_ID, nrow = 3, labeller = as_labeller(presetter)) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b") +
  scale_colour_manual(name = "NO2 signal", values = cols[c(2, 2, 4, 4)], 
    labels = c("original", "original smoothed", "detrended","detrended smoothed")) +
  scale_alpha_discrete(name = "", range = c("original" = .2, "detrended" = 1)) +
  guides(alpha = "none") +
  guides(colour = guide_legend(override.aes = list(colour = cols))) +
  labs(x = "2016", 
    title = "Original signal and (linear) detrended signal per AiREAS station") 
#+ theme_bw()
  

ggplot(subdata, aes(x = hours, y = NO2)) + 
  geom_line() +
  geom_line(aes(y = NO2.detrend), col = "red") +
#  ylim(-5, 150) +
  facet_wrap(~stn_ID)

data.to.merge <- subset(subdata, select = c(dateTime, stn_ID, NO2.detrend))
names(data.to.merge)[3] <- "NO2_corrected"

load("data/dataframe3_15u00.RData")

df.corrected <- merge(df, data.to.merge, all.x = TRUE)
save(df.corrected,file="data/dataframe4_11u25.RData")
write.csv(df.corrected,file="data/dataframe4_11u25.csv")


# plaatje van opbouw Aireas NO2 signaal
# NO2(t) = ref(t) + beta_0 + beta_1*t + e(t)

subdata1 <- na.omit(subset(subdata, stn_ID == 4, 
  select = c("hours", "dateTime", "maand", "stn_ID", "NO2", "ref")))
mod <- with(subdata1, lm(NO2~ offset(ref)+hours))
subdata1 <- within(subdata1, {
  NO2.detrend.pred <- predict(mod, newdata = within(subdata1, hours <-0))
  errors <- residuals(mod)
  NO2.detrend <- NO2.detrend.pred + errors
  constant <- coef(mod)[1]
  linearTrend <- coef(mod)[2]*hours
  rm(NO2.detrend.pred)
  })


subdf.detrend <- within(subdata1, {
  signal <- NO2.detrend
  linearTrend <- NA
  NO2 <- "detrended"
  rm(NO2.detrend, NO2.detrend.pred)
})
subdf <- within(subdata1,{
  signal <- NO2
  NO2 <- "original"
  rm(NO2.detrend, NO2.detrend.pred)
})
df <- rbind(subdf, subdf.detrend)
df$NO2 <- factor(df$NO2, levels = c("original", "detrended"), 
  labels = c("original", "detrended"))

subdf <- melt(df,
  measure.vars = c("signal", "ref", "constant", "linearTrend", "errors"))

head(subdf)
subdf <- within(subdf, 
  variable <- factor(variable, 
    levels = c("signal", "ref", "constant", "linearTrend", "errors"),
    labels = c("signal", "ref_NL10236", "constant", "linear trend", "error")
))

ggplot(subdf, aes(x = dateTime, y = value, colour = NO2)) +
  geom_line(show.legend = FALSE) +
  facet_grid(NO2~variable, switch = "y") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "")) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b") +
  labs(y = expression(NO[2]), x = "2016",
    title = bquote(paste("Decomposition of AiREAS NO"[2], " signal in reference signal + constant + linear trend + error")),
    subtitle = "Detrended signal is linear trend removed from original signal")
