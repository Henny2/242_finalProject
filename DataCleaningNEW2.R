library(data.table) # needed for function rbindlist
library(dplyr)
library(ggplot2)
library(lubridate)
library(caTools)
library(randomForest)
library(caret)
library(viridis)
library(car)
# install.packages('vcd')
library(vcd)

# getting the data for 2019 in one df
files_2019 = list.files("242data/2019/",pattern = ".csv")
temp_2019 = lapply(paste("242data/2019/",files_2019, sep = ""), read.csv, sep=",")
data_2019 <- rbindlist(temp_2019) 
# tranform FL Date into a Date variable instead of factor variable 
data_2019 <- transform(data_2019, FL_DATE = as.Date(FL_DATE))

# getting the data for 2018 in one df
files_2018 = list.files("242data/2018/",pattern = ".csv")
temp_2018 = lapply(paste("242data/2018/",files_2018, sep = ""), read.csv, sep=",")
data_2018 <- rbindlist(temp_2018) 
# tranform FL Date into a Date variable instead of factor variable 
data_2018 <- transform(data_2018, FL_DATE = as.Date(FL_DATE))


data_2018$X = NULL
data_2019$X = NULL
df_list = list(data_2018, data_2019)
data_all = rbindlist(df_list)


##### filtering for 5 carriers ######
carrier5 <- c('AA','AS','DL','WN','B6')
data5carriers = filter(data_all, data_all$OP_UNIQUE_CARRIER %in% carrier5)

##### filtering for top 10 airports ######
top10airports <- c('JFK','SFO','LAX','ATL','ORD','DFW','DEN','SEA','LAS','MCO')

origin10 <-  data5carriers %>% filter(data5carriers$ORIGIN %in% top10airports)
OD10 <- origin10 %>% filter( origin10$DEST %in% top10airports)

#data from 1 year
date = as.Date(c("2018-09-30"))
date
OD10 <- transform(OD10, FL_DATE = as.Date(FL_DATE))
pickeddata <-  OD10 %>% filter(OD10$FL_DATE > date)



#### NA values ####

naTF <- sapply(pickeddata, anyNA)
dataNaCols <- colnames(pickeddata[,naTF])

# problem: the islands below florida are not captured by this division in regions
# I just put islands for them as the region
# Puerto Rico 13151 
# U.S. Virgin Islands 5221


##### NAs in ArrDelay #####
table(is.na(pickeddata$ARR_DELAY)) # 52082 NA's
table(pickeddata$CANCELLED[is.na(pickeddata$ARR_DELAY)])
# most of the flights that do not have a value for the arrival delay were cancelled 
## decide to delete them 

df_final = pickeddata[!is.na(pickeddata$ARR_DELAY),]


# checking again for NA's
naTF <- sapply(df_final, anyNA)
dataNaCols <- colnames(df_final[,naTF])
dataNaCols
# the remaining columns are not interesting for us atm, thus don't do something about the 
# missing values


#### Add Weather Data ####

# Read data
weather <- read.csv("242data/Weather.csv")
weather <- transform(weather, DATE = as.Date(as.character(DATE),"%m/%d/%y"))
# merge datasets
df_final_weather <- merge(x= df_final, y = weather, by.x = c("ORIGIN","FL_DATE"), by.y = c("Airport.CODE","DATE"))
df_final_weather <- merge(x= df_final_weather, y = weather, by.x = c("DEST","FL_DATE"), by.y = c("Airport.CODE","DATE"))


# temp
vec = rep(0, 180568)
vec[df_final_weather$WT01.x==1] = 1
vec[df_final_weather$WT02.x==1] = 2
vec[df_final_weather$WT03.x==1] = 3
vec[df_final_weather$WT04.x==1] = 4
vec[df_final_weather$WT05.x==1] = 5
vec[df_final_weather$WT06.x==1] = 6
vec[df_final_weather$WT07.x==1] = 7
vec[df_final_weather$WT08.x==1] = 8
vec[df_final_weather$WT09.x==1] = 9
vec[df_final_weather$WT010.x==1] = 10
vec
df_final_weather$WT_Origin = vec

vec2 = rep(0, 180568)
vec2[df_final_weather$WT01.y==1] = 1
vec2[df_final_weather$WT02.y==1] = 2
vec2[df_final_weather$WT03.y==1] = 3
vec2[df_final_weather$WT04.y==1] = 4
vec2[df_final_weather$WT05.y==1] = 5
vec2[df_final_weather$WT06.y==1] = 6
vec2[df_final_weather$WT07.y==1] = 7
vec2[df_final_weather$WT08.y==1] = 8
vec2[df_final_weather$WT09.y==1] = 9
vec2[df_final_weather$WT010.y==1] = 10
df_final_weather$WT_Destination = vec2


# handling NA's

naTF_new <- sapply(df_final_weather, anyNA)
dataNaCols_new <- colnames(dadf_final_weatherta[,naTF_new])
dataNaCols_new

# deleting the single weather type columns 
df_final_weather$WT01.x = NULL
df_final_weather$WT02.x = NULL
df_final_weather$WT03.x = NULL
df_final_weather$WT04.x = NULL
df_final_weather$WT05.x = NULL
df_final_weather$WT06.x = NULL
df_final_weather$WT07.x = NULL
df_final_weather$WT08.x = NULL
df_final_weather$WT09.x = NULL
df_final_weather$WT10.x = NULL

df_final_weather$WT01.y = NULL
df_final_weather$WT02.y = NULL
df_final_weather$WT03.y = NULL
df_final_weather$WT04.y = NULL
df_final_weather$WT05.y = NULL
df_final_weather$WT06.y = NULL
df_final_weather$WT07.y = NULL
df_final_weather$WT08.y = NULL
df_final_weather$WT09.y = NULL
df_final_weather$WT10.y = NULL

# again
naTF_new <- sapply(df_final_weather, anyNA)
dataNaCols_new <- colnames(df_final_weather[,naTF_new])
dataNaCols_new

# PGTM (hours and minutes, i.e., HHMM)
# he highest "instantaneous" wind speed recorded at a station during a specified period, 
# usually the 24-hour observational day.
table(is.na(df_final_weather$PGTM.x)) # half na's
table(is.na(df_final_weather$PGTM.y)) # half na's
head(df_final_weather$PGTM.x[!is.na(df_final_weather$PGTM.x)])
table(df_final_weather$PGTM.x)

# PRCP: all weather conditions where something falls from the sky (tenths of mm)
table(is.na(df_final_weather$PRCP.x)) 
table(is.na(df_final_weather$PRCP.y)) 
head(df_final_weather$PRCP.x[!is.na(df_final_weather$PRCP.x)])
table(df_final_weather$PRCP.x)
## my idea: set zero, or delete, only little observations wihtout!

# SNOW = Snowfall (mm)
table(is.na(df_final_weather$SNOW.x)) # 20% na 
table(is.na(df_final_weather$SNOW.y)) # 20% na 
head(df_final_weather$SNOW.x[!is.na(df_final_weather$SNOW.x)])
table(df_final_weather$SNOW.x)
## idea: maybe na when there was nothing so set to 0?!

#SNWD = Snow depth (mm)
table(is.na(df_final_weather$SNWD.x)) # 20% na 
table(is.na(df_final_weather$SNWD.y)) # 20% na 
head(df_final_weather$SNWD.x[!is.na(df_final_weather$SNWD.x)])
table(df_final_weather$SNWD.x)
## idea: maybe na when there was nothing so set to 0?!

#TAVG = Average temperature (tenths of degrees C)
table(is.na(df_final_weather$TAVG.x)) # 10% na 
table(is.na(df_final_weather$TAVG.y)) # 10% na 
head(df_final_weather$TAVG.x[!is.na(df_final_weather$TAVG.x)])
table(df_final_weather$TAVG.x)
# what to do if we dont have the average temp?

# TMAX = Maximum temperature (tenths of degrees C)

table(is.na(df_final_weather$TMAX.x)) # only a few nas
table(is.na(df_final_weather$TMAX.y)) 
head(df_final_weather$TMAX.x[!is.na(df_final_weather$TMAX.x)])
table(df_final_weather$TMAX.x)
# idea: delete them, only a few


# TMIN = Minimum temperature (tenths of degrees C)
table(is.na(df_final_weather$TMIN.x)) # only a few nas
table(is.na(df_final_weather$TMIN.y)) 
head(df_final_weather$TMIN.x[!is.na(df_final_weather$TMIN.x)])
table(df_final_weather$TMIN.x)
# idea: delete them, only a few


# AWND = Average daily wind speed (tenths of meters per second)
table(is.na(df_final_weather$AWND.x)) # only a few nas
table(is.na(df_final_weather$AWND.y)) 
head(df_final_weather$AWND.x[!is.na(df_final_weather$AWND.x)])
table(df_final_weather$AWND.x)
# idea: delete them, only a few

# WDF2 = Direction of fastest 2-minute wind (degrees)
table(is.na(df_final_weather$WDF2.x)) # only a few nas
table(is.na(df_final_weather$WDF2.y)) 
head(df_final_weather$WDF2.x[!is.na(df_final_weather$WDF2.x)])
table(df_final_weather$WDF2.x)
# idea: delete them, only a few

# WDF5 = Direction of fastest 5-second wind (degrees)
table(is.na(df_final_weather$WDF5.x)) 
table(is.na(df_final_weather$WDF5.y)) 
head(df_final_weather$WDF5.x[!is.na(df_final_weather$WDF5.x)])
table(df_final_weather$WDF5.x)
# idea1: not to use at all :D
# idea2: delete them, only a few

# WESD = Water equivalent of snow on the ground (tenths of mm)
table(is.na(df_final_weather$WESD.x)) 
table(is.na(df_final_weather$WESD.y)) 
head(df_final_weather$WESD.x[!is.na(df_final_weather$WESD.x)])
table(df_final_weather$WESD.x)
# not using, too many nas and the rest is all zero!!

# WSF2 = Fastest 2-minute wind speed (tenths of meters per second)
table(is.na(df_final_weather$WSF2.x)) 
table(is.na(df_final_weather$WSF2.y)) 
head(df_final_weather$WSF2.x[!is.na(df_final_weather$WSF2.x)])
table(df_final_weather$WSF2.x)
# idea: delete them, only a few

# WSF5 = Fastest 5-second wind speed (tenths of meters per second)
table(is.na(df_final_weather$WSF5.x)) 
table(is.na(df_final_weather$WSF5.y)) 
head(df_final_weather$WSF5.x[!is.na(df_final_weather$WSF5.x)])
table(df_final_weather$WSF5.x)
# idea1: not to use at all :D
# idea2: delete them, only a few

# Decision to go with those weather features:
# AWND, PRCP, TAVG, WDF2, SNOW and WT (all for origin and destination)
# NA Handling: deleting the rows that have NA's, except TAVG: there we take average of MIN 
# & MAX





# remove useless columns
df_final_weather$WESD.y <- NULL
df_final_weather$WESD.x <- NULL
df_final_weather$X <- NULL
df_final_weather$STATION.x <- NULL
df_final_weather$STATION.y <- NULL

df_final_weather$PGTM.x <- NULL
df_final_weather$PGTM.y <- NULL
df_final_weather$WSF2.x <- NULL
df_final_weather$WSF2.y <- NULL
df_final_weather$WSF5.x <- NULL
df_final_weather$WSF5.y <- NULL
df_final_weather$SNWD.x <- NULL
df_final_weather$SNWD.y <- NULL
df_final_weather$WDF5.x <- NULL
df_final_weather$WDF5.y <- NULL

df_final_weather$TAVG.x[which(is.na(df_final_weather$TAVG.x))] <- (df_final_weather$TMAX.x[which(is.na(df_final_weather$TAVG.x))]
                                                                   + df_final_weather$TMIN.x[which(is.na(df_final_weather$TAVG.x))])/2

df_final_weather$TAVG.y[which(is.na(df_final_weather$TAVG.y))] <- (df_final_weather$TMAX.y[which(is.na(df_final_weather$TAVG.y))]
                                                                   + df_final_weather$TMIN.y[which(is.na(df_final_weather$TAVG.y))])/2

df_final_weather <- df_final_weather[which((!is.na(df_final_weather$AWND.x))&
                                             (!is.na(df_final_weather$PRCP.x))&
                                             (!is.na(df_final_weather$SNOW.x))&
                                             (!is.na(df_final_weather$TAVG.x))&
                                             (!is.na(df_final_weather$WDF2.x))&
                                             (!is.na(df_final_weather$WT_Origin)&
                                                (!is.na(df_final_weather$AWND.y))&
                                                (!is.na(df_final_weather$PRCP.y))&
                                                (!is.na(df_final_weather$SNOW.y))&
                                                (!is.na(df_final_weather$TAVG.y))&
                                                (!is.na(df_final_weather$WDF2.y))&
                                                (!is.na(df_final_weather$WT_Destination)))),]

## All weather variables
# Average wind speed (AWND);Direction of fastest 2-minute wind (WDF2)
# Precipitation (PRCP)
# Snowfall (SNOW)
# Average Temperature. (TAVG)
# Weather Type (WT)


df_final <- df_final_weather
head(df_final)

## deleting more useless columns and columns we should not use
df_final$CANCELLED = NULL
df_final$CANCELLATION_CODE = NULL
df_final$CARRIER_DELAY = NULL
df_final$WEATHER_DELAY = NULL
df_final$LATE_AIRCRAFT_DELAY= NULL
df_final$SECURITY_DELAY= NULL
df_final$DEP_DELAY= NULL
df_final$NAS_DELAY= NULL
df_final$TMAX.x= NULL
df_final$TMIN.x= NULL
df_final$TMAX.y= NULL
df_final$TMIN.y= NULL

df_final$DEST = as.character(df_final$DEST)
df_final$DEST = as.factor(df_final$DEST)
df_final$ORIGIN = as.character(df_final$ORIGIN)
df_final$ORIGIN = as.factor(df_final$ORIGIN)
df_final$DEST_CITY_NAME = as.character(df_final$DEST_CITY_NAME)
df_final$DEST_CITY_NAME = as.factor(df_final$DEST_CITY_NAME)
df_final$DEST_STATE_NM = as.character(df_final$DEST_STATE_NM)
df_final$DEST_STATE_NM = as.factor(df_final$DEST_STATE_NM)
df_final$ORIGIN_CITY_NAME = as.character(df_final$ORIGIN_CITY_NAME)
df_final$ORIGIN_CITY_NAME = as.factor(df_final$ORIGIN_CITY_NAME)
df_final$ORIGIN_STATE_NM = as.character(df_final$ORIGIN_STATE_NM)
df_final$ORIGIN_STATE_NM = as.factor(df_final$ORIGIN_STATE_NM)
df_final$OP_UNIQUE_CARRIER = as.character(df_final$OP_UNIQUE_CARRIER)
df_final$OP_UNIQUE_CARRIER = as.factor(df_final$OP_UNIQUE_CARRIER)
df_final$NAME.x = as.character(df_final$NAME.x)
df_final$NAME.x = as.factor(df_final$NAME.x)
df_final$NAME.y = as.character(df_final$NAME.y)
df_final$NAME.y = as.factor(df_final$NAME.y)

## putting as factor
df_final$WT_Origin = as.factor(df_final$WT_Origin)
df_final$WT_Destination = as.factor(df_final$WT_Destination)
df_final$MONTH = as.factor(month(df_final$FL_DATE))
df_final$WEEKDAY = as.factor(weekdays(df_final$FL_DATE))

head(df_final)
str(df_final)
## making the classification ##

df_final$DELAYED = 0
df_final$DELAYED[df_final$ARR_DELAY >15] = 1
df_final$DELAYED = as.factor(df_final$DELAYED)


# deleting the ARR_DELAY column to not use it as a feature
df_final$ARR_DELAY = NULL


######## Visualization Via Boxplot ######

### DELAYED (works poorly)
# DISTANCE VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = DISTANCE, fill = DELAYED)) + 
            geom_boxplot() + ggtitle("Boxplot of distance VS delayed")

# flight time VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = CRS_ELAPSED_TIME, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of flight time VS delayed") + ylab("flight time")


# origin wind speed VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = AWND.x, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nORIGIN wind speed VS delayed") + ylab("Wind speed")

# destination wind speed VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = AWND.y, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nDEST wind speed VS delayed") + ylab("Wind speed")

# origin average temperature VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = TAVG.x, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nORIGIN AVG temp VS delayed") + ylab("Temperature (°F)")

# destination average temperature VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = TAVG.y, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nDEST AVG temp VS delayed") + ylab("Temperature (°F)")

# origin temperature VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = TAVG.x, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nORIGIN AVG temp VS delayed") + ylab("Temperature (°F)")

# destination temperature VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = TAVG.y, fill = DELAYED)) + 
  geom_boxplot() + ggtitle("Boxplot of \nDEST AVG temp VS delayed") + ylab("Temperature (°F)")

# origin precipitation VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = PRCP.x * 10, fill = DELAYED)) + 
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  coord_cartesian(ylim = c(0, 3)) +
  ggtitle("Boxplot of \nORIGIN precipitation VS delayed") + 
  ylab("Precipitation (mm)")

# destination precipitation VS DELAYED
df_final %>% ggplot(aes(x = DELAYED, y = PRCP.y * 10, fill = DELAYED)) + 
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  coord_cartesian(ylim = c(0, 3)) +
  ggtitle("Boxplot of \nDESTINATION precipitation VS delayed") + 
  ylab("Precipitation (mm)")

# origin snow VS DELAYED (so.... so bad)
df_final %>% ggplot(aes(x = DELAYED, y = SNOW.x, fill = DELAYED)) + 
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  coord_cartesian(ylim = c(0, 0.1)) +
  ggtitle("Boxplot of \nORIGIN snow VS delayed") + 
  ylab("Snow (mm)")


### try ARR_DELAY instead

# origin
df_final %>% ggplot(aes(x = ORIGIN, y = ARR_DELAY, fill = DELAYED)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-100, 200)) + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  ggtitle("Boxplot of arrival delay VS origin") + 
  ylab("delayed minutes")
  # facet_wrap(~DELAYED)

# destination
df_final %>% ggplot(aes(x = DEST, y = ARR_DELAY, fill = DELAYED)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-100, 200)) + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  ggtitle("Boxplot of arrival delay VS destination") + 
  ylab("delayed minutes")  

# airlines
df_final %>% ggplot(aes(x = OP_UNIQUE_CARRIER, y = ARR_DELAY, fill = DELAYED)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-100, 200)) + 
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  ggtitle("Boxplot of arrival delay VS carrier") + 
  ylab("delayed minutes")

######## Visualization Via Mosaic Plot ######

# origin
tbl <- structable(df_final$DELAYED ~ df_final$ORIGIN)
mosaic(tbl, main = "Mosaic Plot of \nORIGIN VS delay", xlab = "delayed", ylab = "origin", shade = TRUE, legend = TRUE)

# destination
tbl <- structable(df_final$DELAYED ~ df_final$DEST)
mosaic(tbl, main = "Mosaic Plot of \nDEST VS delay", xlab = "delayed", ylab = "destination", shade = TRUE, legend = TRUE)

######## Build models! ######
set.seed(1234)
split = sample.split(df_final$DELAYED, SplitRatio = 0.7) 
class.train <- filter(df_final, split == TRUE) 
class.test <- filter(df_final, split == FALSE)




######## Logistic Regression ######
log_reg <- glm(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
               +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME
               , data=class.train, family=binomial(link='logit'), maxit = 100)
#log_reg <- glm(DELAYED ~ ., data=class.train, family=binomial(link='logit'), maxit = 100)
vif(log_reg)
summary(log_reg)

###### Testing additional features #####
# tried with CRS_ELAPSED_TIME, but then Multicollinerarity; prbly between Distance and CRS ELAPSED TIME
# name.x and name.y same information as origin and dest (city/airport names - lead to singularities)
# AWND.x +AWND.y - both significant, no multicollinearity and improved accuracy ## ADDED
# PRCP.x+PRCP.y - both significant, no multicollinearity and improved accuracy ## ADDED
# SNOW.x+SNOW.y - both significant, no multicollinearity and improved accuracy ## ADDED
# TAVG.x+TAVG.y - both significant, no multicollinearity and improved accuracy ## ADDED
# WDF2.y -  significant, no multicollinearity and improved accuracy ## ADDED
# WDF2.x -  not significant, (but improved accuracy because of extra degree of freedom) not added


str(class.train)



predTestLog <- predict(log_reg, newdata=class.test, type="response") # type on default gives log-odds, "response" gives predicted probabilities
table(predTestLog >0.5)

table(predTestLog>threshold, class.test$DELAYED)
table(predTestLog>0.5, class.test$DELAYED)


acc_log = (38777  + 1047)/50257

acc_log

#### Bootstrapping Logistic Regression ####
preds_log = rep(FALSE,50257 )
preds_log[predTestLog>0.5] = TRUE
preds_log

# adjusted threshold for minimzing the loss
preds.loss.log = rep(FALSE,50257)
preds.loss.log[predTestLog>threshold] = TRUE

big_B = 1000
log_reg_df = data.frame(labels = class.test$DELAYED, predictions = preds_log)
log_reg_loss_df = data.frame(labels = class.test$DELAYED, predictions = preds.loss.log)
set.seed(5810)
LogReg_boot = boot(log_reg_df, boot_all_metrics, R = big_B)
LogReg_loss_boot = boot(log_reg_loss_df, boot_all_metrics, R = big_B)
LogReg_boot


LogReg_boot
boot.ci(LogReg_boot, index = 1, type = "basic") #accuracy confidence interval (95%)
boot.ci(LogReg_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LogReg_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LogReg_boot, index = 4, type = "basic") # average loss confidence interval (95%)

boot.ci(LogReg_loss_boot, index = 1, type = "basic") # accuracy confidence interval (95%)
boot.ci(LogReg_loss_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LogReg_loss_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LogReg_loss_boot, index = 4, type = "basic") # average loss confidence interval (95%)


#### ROC Curve Logistic Regression #####

rocr.log.pred <- prediction(predTestLog, class.test$DELAYED)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
logPerformance
plot(logPerformance, colorize = TRUE)
abline(0, 1)
# area under the curve
AUC_log1 = as.numeric(performance(rocr.log.pred, "auc")@y.values)
AUC_log1



####### LDA ########

str(class.train)
set.seed(1234)
LdaModel <- lda(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME
                ,data=class.train)

predTestLDA <- predict(LdaModel, newdata=class.test) 
predTestLDA_probs <- predTestLDA$posterior[,2]

t1=table(class.test$DELAYED, predTestLDA_probs > 1/2)

acc_lda = (38465+1364)/50257
acc_lda
# 0.7925065

rocr.lda.pred <- prediction(predTestLDA_probs, class.test$DELAYED)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)


#### Bootstrapping LDA ####

preds_lda = rep(FALSE,50257 )
preds_lda[predTestLDA_probs>=0.5] = TRUE
preds_lda

big_B = 1000
lda_df = data.frame(labels = class.test$DELAYED, predictions = preds_lda)
set.seed(5810)
LDA_boot = boot(lda_df, boot_all_metrics, R = big_B)

LDA_boot
boot.ci(LDA_boot, index = 1, type = "basic") #accuracy confidence interval (95%)
boot.ci(LDA_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LDA_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LDA_boot, index = 4, type = "basic") # average loss confidence interval (95%)

# with loss function

preds_loss_lda = rep(FALSE,50257)
preds_loss_lda[predTestLDA_probs> threshold] = TRUE

big_B = 1000
lda_loss_df = data.frame(labels = class.test$DELAYED, predictions = preds_loss_lda)
set.seed(5810)
LDA_loss_boot = boot(lda_loss_df, boot_all_metrics, R = big_B)

boot.ci(LDA_loss_boot, index = 1, type = "basic") # accuracy confidence interval (95%)
boot.ci(LDA_loss_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LDA_loss_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LDA_loss_boot, index = 4, type = "basic") # average loss confidence interval (95%)


######## CART ######
train.cart = train(DELAYED ~ . - FL_DATE - ARR_DELAY - NAME.x - NAME.y , 
                   data=class.train,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.04, 0.002)),
                   trControl = trainControl(method="cv", number=5,verboseIter = TRUE),
                   metric = "Accuracy")
train.cart$bestTune
prp(train.cart$finalModel, digits=3)
test.mm = as.data.frame(model.matrix(DELAYED ~ . + 0, data=class.test)) 
pred.best.cart = predict(train.cart$finalModel, newdata = test.mm, type="class")
t = table(class.test$DELAYED, pred.best.cart)
acc_log = sum(diag(t))/sum(t)
acc_log


## Cart with loss function ##

set.seed(123)
# we will first define a special loss function in CV
# we would like to have the small Average Loss
Loss = function(data, lev = NULL, model = NULL, ...) {
  c(AvgLoss = mean(data$weights * (data$obs != data$pred)),
    Accuracy = mean(data$obs == data$pred))
}

delays <- class.train %>% group_by(DELAYED) %>% summarise(delays = mean(ARR_DELAY))
avg.delay <- as.numeric(as.matrix(delays)[2,2])

# This specifies the weights i.e. the loss function
# Read this as: if observation is a violator and we make a mistake, assign weight 20
#               if observation is NOT a violator and we make a mistake, assign weight 1
weights = ifelse(class.train$DELAYED == 1, avg.delay*80.25/60 + 27, 75)
# DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST  
# +AWND.x +AWND.y+ DISTANCE + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME,
cart.loss <- train(
                 DELAYED ~ . - FL_DATE - ARR_DELAY - NAME.x - NAME.y,
                 data=class.train,
                 method="rpart",
                 weights = weights,
                 trControl = trainControl(method = "cv", number = 5,
                                        summaryFunction = Loss, verboseIter = TRUE),
                 tuneGrid = data.frame(cp = seq(0, .01, by=.0005)), #cpVals
                 metric="AvgLoss", 
                 maximize=FALSE)
cart.loss
cp.plot <- ggplot(cart.loss$results, aes(x=cp, y=AvgLoss)) + geom_line(lwd=1) +
  ylab("Average Loss of Predictions")
cp.plot

cart.loss$bestTune
mod3432 = cart.loss$finalModel
prp(mod3432, digits=3)

###### LASSO #######
set.seed(3439)
trainY = class.train$DELAYED
fo = DELAYED ~ . -1

x <- model.matrix(DELAYED~., class.train)[,-1]
testX <- model.matrix(DELAYED~., class.test)[,-1]

testY = class.test$DELAYED

mod.lasso <- glmnet(x = x, y = trainY, alpha = 1, family = "binomial")

mod.lasso$lambda
coefs.lasso <- coef(mod.lasso)
x
plot(mod.lasso, xvar = "lambda")

set.seed(821)
cv.lasso <- cv.glmnet(x = x, y = trainY, alpha = 1, family = "binomial")

cv.lasso$lambda.min
plot(cv.lasso)
pred.lasso.train <- predict(cv.lasso, newx = x)
pred.lasso.test <- predict(cv.lasso, newx = testX)

# tells us the non-zero coefficient indicies
nzero.lasso <- predict(cv.lasso, type = "nonzero")
nzero.lasso
mod.lasso$df
mod.lasso
# coef(mod.lasso)


mod.lasso$df # gives the number of non-zero coefficients for each value of lambda
coef(cv.lasso, cv.lasso$lambda.min)

#### Question: which ones are chosen? the ones with the dot? or with numbers? ####
# those with the dots are set to zero



# Final model with lambda.min
lasso.model <- glmnet(x, trainY, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
# Make prediction on test data
x.test <- model.matrix(DELAYED ~., class.test)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
probabilities>0.5

probabilities
predicted.classes
# Model accuracy
observed.classes <- class.test$DELAYED
table(predicted.classes == observed.classes)
acc_lasso = 39723 /50257
acc_lasso
acc_log
acc_lda


#### Bootstrapping LASSO ####
preds_lasso = rep(FALSE,50257 )
preds_lasso[predicted.classes==1] = TRUE
preds_lasso
length(predicted.classes) 
length(class.test$DELAYED)

big_B = 1000
lasso_df = data.frame(labels = class.test$DELAYED, predictions = preds_lasso)
set.seed(5810)
LASSO_boot = boot(lasso_df, boot_all_metrics, R = big_B)

LASSO_boot
boot.ci(LASSO_boot, index = 1, type = "basic") #accuracy confidence interval (95%)
boot.ci(LASSO_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LASSO_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LASSO_boot, index = 4, type = "basic") # average loss confidence interval (95%)


## with loss function
predicted.loss.classes <- ifelse(probabilities > threshold, 1, 0)
preds_loss_lasso = rep(FALSE,50257 )
preds_loss_lasso[predicted.loss.classes==1] = TRUE
lasso_loss_df = data.frame(labels = class.test$DELAYED, predictions = preds_loss_lasso)
set.seed(5810)
LASSO_loss_boot = boot(lasso_loss_df, boot_all_metrics, R = big_B)
boot.ci(LASSO_loss_boot, index = 1, type = "basic") # accuracy confidence interval (95%)
boot.ci(LASSO_loss_boot, index = 2, type = "basic") # tpr confidence interval (95%)
boot.ci(LASSO_loss_boot, index = 3, type = "basic") # fpr confidence interval (95%)
boot.ci(LASSO_loss_boot, index = 4, type = "basic") # average loss confidence interval (95%)
# make predictions
pred = predict(mod3432, newdata=test.mm, type="class")
t = table(class.test$DELAYED, pred)
acc_log = sum(diag(t))/sum(t)
acc_log

# Bootstrap 
library(boot)

tableAccuracy <- function(label, pred) {
  t = table(label, pred)
  a = sum(diag(t))/length(label)
  return(a)
}

tableTPR <- function(label, pred) {
  t = table(label, pred)
  return(t[2,2]/(t[2,1] + t[2,2]))
}

tableFPR <- function(label, pred) {
  t = table(label, pred)
  return(t[1,2]/(t[1,1] + t[1,2]))
}

tableLoss <- function(label, pred) {
  t = table(label, pred)
  return(((avg.delay*80.25/60 + 27)*t[2,1]+(t[1,2]+t[2,2])*75)/sum(t))
}


boot_accuracy <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableAccuracy(labels, predictions))
}

boot_tpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableTPR(labels, predictions))
}

boot_fpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableFPR(labels, predictions))
}

boot_avgloss<- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableLoss(labels, predictions))
}

boot_all_metrics <- function(data, index) {
  acc = boot_accuracy(data, index)
  tpr = boot_tpr(data, index)
  fpr = boot_fpr(data, index)
  avgloss =  boot_avgloss(data, index)
  return(c(acc, tpr, fpr,avgloss))
}

big_B = 10000

cart_df = data.frame(labels = class.test$DELAYED, predictions = pred)
set.seed(3526)
CART_boot = boot(cart_df, boot_all_metrics, R = big_B)
CART_boot
boot.ci(CART_boot, index = 1, type = "basic") # accuracy
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")
boot.ci(CART_boot, index = 4, type = "basic") # avg loss

#### CART With full loss matrix####
##### CART LOSS##### validating cv
library(stringr)

cp.vals = rep(NA, 200)
a= 1
for (cp in seq(0, 0.04, 0.0002)) {
  print(str_c("Trying cp value = ", cp))
  mod <- rpart(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
               +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME,
               data = class.train,
               method = "class", 
               minbucket=5,  
               control = rpart.control(cp = cp))
  preds <- predict(mod, newdata = class.test, type = "class")
  t = table(preds, class.test$DELAYED)
  cp.vals[a] <- t[1,2]*75+t[2,1]*118.7556+t[2,2]*75 
  a = a+1
}
cp.vals
seq(0, 0.04, 0.002)
seq(0, 0.04, 0.0002)



final.cart.mod <- rpart(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
             +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME,
             data = class.train,
             method = "class", 
             minbucket=5,  
             cp = 0.0042)
prp(train.cart$finalModel)
final.preds <- predict(final.cart.mod, newdata = class.test, type = "class")
table(final.preds)
table(final.preds, class.test$DELAYED)


big_B = 1000

cart_df = data.frame(labels = class.test$DELAYED, predictions = final.preds)
set.seed(3526)
CART_boot = boot(cart_df, boot_all_metrics, R = big_B)
CART_boot
boot.ci(CART_boot, index = 1, type = "basic") # accuracy
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")
boot.ci(CART_boot, index = 4, type = "basic") # avg loss
###### end CART full losss matrix 



#### Baseline #####
# prdicting most frequent
table(class.test$DELAYED)
acc_baseline = 39497/50257
acc_baseline

###### LASSO #######
set.seed(3439)
trainY = class.train$DELAYED
fo = DELAYED ~ . -1

x <- model.matrix(DELAYED~., class.train)[,-1]
testX <- model.matrix(DELAYED~., class.test)[,-1]

testY = class.test$DELAYED

trainX$DELAYED = NULL
mod.lasso <- glmnet(x = x, y = trainY, alpha = 1, family = "binomial")

mod.lasso$lambda
coefs.lasso <- coef(mod.lasso)
x
plot(mod.lasso, xvar = "lambda")

set.seed(821)
cv.lasso <- cv.glmnet(x = x, y = trainY, alpha = 1, family = "binomial")

cv.lasso$lambda.min
plot(cv.lasso)
pred.lasso.train <- predict(cv.lasso, newx = x)
pred.lasso.test <- predict(cv.lasso, newx = testX)

# tells us the non-zero coefficient indicies
nzero.lasso <- predict(cv.lasso, type = "nonzero")
nzero.lasso
mod.lasso$df
mod.lasso
# coef(mod.lasso)

mod.lasso$df # gives the number of non-zero coefficients for each value of lambda
coef(cv.lasso, cv.lasso$lambda.min) # gives the selected coefficients

#### Question: which ones are chosen? the ones with the dot? or with numbers? ####
# those with the dots are set to zero

# Final model with lambda.min
lasso.model <- glmnet(x, trainY, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
# Make prediction on test data
x.test <- model.matrix(DELAYED ~., class.test)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
probabilities>0.5
predicted.classes
# Model accuracy
observed.classes <- class.test$DELAYED
table(predicted.classes == observed.classes)
acc_lasso = 39723 /50257
acc_lasso


######## Random Forest Regression ######

set.seed(456)
# mod.rf <- randomForest(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
#                        +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME, 
#                        data = class.train,
#                        cutoff=c(.96,.04))
costMatrix <- matrix(c(0,118.7556182,75,75), nrow=2)
set.seed(456)
mod.rf <- randomForest(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                       +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME, 
                       data = class.train,
                       parms = list(loss=costMatrix))

pred.rf <- predict(mod.rf, newdata = class.test) # just to illustrate

# get the confusion matrix of the results
t = table(class.test$DELAYED, pred.rf)
t
# accuracy:
acc_rf = sum(diag(t)) / sum(t)
acc_rf

loss_rf = sum(t * costMatrix) / sum(t)
loss_rf



## try the cross-validated RF model

# we would like to have the small Average Loss

# reduce our data to 1% for now!
# set.seed(144)
# train.ids = sample(nrow(class.train), 0.01*nrow(class.train))
# class.train = class.train[train.ids,]

## Calculate the training set average delay (only consider those greater than 15min)
delays <- class.train %>% 
          group_by(DELAYED) %>% summarise(delays = mean(ARR_DELAY))
delays
avg.delay <- as.numeric(as.matrix(delays)[2,2])

Loss = function(data, lev = NULL, model = NULL, ...) {
  c(AvgLoss = mean(data$weights * (data$obs != data$pred)
                   + 75 * (data$obs == data$pred)*(data$obs == 1)),                
    Accuracy = mean(data$obs == data$pred))
}

weights = ifelse(class.train$DELAYED == 1, avg.delay / 60 * 80.25272727272727 + 27, 75)


set.seed(456)
train.rf <- train(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                  +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME,
                  data = class.train,
                  method = "rf",
                  weights = weights,
                  tuneGrid = data.frame(mtry=5:5),
                  trControl = trainControl(method="cv", 
                                           number=5, 
                                           summaryFunction = Loss,
                                           verboseIter = TRUE),
                  metric = "AvgLoss",
                  maximize=FALSE)
train.rf$results
train.rf
best.rf <- train.rf$finalModel

test.class.mm = as.data.frame(model.matrix(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                                           +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME + 0, data=class.test)) 


pred.best.rf <- predict(best.rf, newdata = test.class.mm) 


# make a graph and find the best mtry

# Best mtry on AvgLoss
p <- ggplot(train.rf$results, aes(mtry))

p <- p + geom_line(aes(y = Accuracy, colour = 'Accuracy')) +geom_point(aes(y = Accuracy, colour = 'Accuracy'), size = 2)

p <- p + geom_line(aes(y = AvgLoss/30, colour = 'Average Loss')) +geom_point(aes(y = AvgLoss/30, colour = 'Average Loss'), size = 2)

p <- p + scale_y_continuous(sec.axis = sec_axis(~.*30, name = "Average Loss"))

p <- p + ggtitle("RF AvgLoss/Accuracy VS mtry") +
                labs(y = "Accuracy",
                colour = "Metric") +
  theme_bw() + theme(axis.title=element_text(size=10), axis.text=element_text(size=8))

p


# Best mtry on Accuracy
ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 2) + geom_line() +
  ggtitle("Random Forest Accuracy VS mtry")+
  ylab("Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


t <- table(class.test$DELAYED, pred.best.rf)
t
acc_best_rf <- sum(diag(t)) / sum(t)
acc_best_rf


#### Bootstrapping Random Forest ####
big_B = 1000

rf_df = data.frame(labels = class.test$DELAYED, predictions = pred.best.rf)
set.seed(3526)
rf_boot = boot(rf_df, boot_all_metrics, R = big_B)
rf_boot
boot.ci(rf_boot, index = 1, type = "basic") # accuracy
boot.ci(rf_boot, index = 2, type = "basic")
boot.ci(rf_boot, index = 3, type = "basic")
boot.ci(rf_boot, index = 4, type = "basic") # avg loss


######## multiclass classification ######

df_final$DELAYED = 0
df_final$DELAYED[df_final$ARR_DELAY >15] = 1
df_final$DELAYED[df_final$ARR_DELAY >180] = 2
df_final$DELAYED[df_final$ARR_DELAY >300] = 3
df_final$DELAYED = as.factor(df_final$DELAYED)

set.seed(1234)
split = sample.split(df_final$DELAYED, SplitRatio = 0.7) 
class.train <- filter(df_final, split == TRUE) 
class.test <- filter(df_final, split == FALSE)


str(class.train)
set.seed(1234)
LdaModel <- lda(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME
                ,data=class.train)

predTestLDA <- predict(LdaModel, newdata=class.test) 
predTestLDAfunclass <- predTestLDA$class
predTestLDAfunprob <- predTestLDA$posterior

tab <- table(class.test$DELAYED, predTestLDAfunclass)
accuracy <- sum(diag(tab))/sum(tab)
accuracy
tab

pred = apply(predTestLDA, 1, which.max)
predpred = factor(pred, levels = c(1,2,3,4), labels = c("A", "B", "C", "D"))

pred



set.seed(456)
train.rf <- train(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                  +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME,
                  data = class.train,
                  method = "rf",
                  weights = weights,
                  tuneGrid = data.frame(mtry=1:18),
                  trControl = trainControl(method="cv", 
                                           number=5, 
                                           verboseIter = TRUE),
                  metric = "Accuracy",
                  maximize=FALSE)
train.rf$results
train.rf
best.rf <- train.rf$finalModel

test.class.mm = as.data.frame(model.matrix(DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                                           +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME + 0, data=class.test)) 


pred.best.rf <- predict(best.rf, newdata = test.class.mm) 


# make a graph and find the best mtry
ggplot(train.rf$results, aes(x = mtry, y = AvgLoss)) + geom_point(size = 2) + geom_line() +
  ggtitle("Random Forest AvgLoss VS mtry")+
  ylab("AvgLoss") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

t <- table(class.test$DELAYED, pred.best.rf)
t
acc_best_rf <- sum(diag(t)) / sum(t)
acc_best_rf

######## CART ######
train.cart = train(DELAYED ~ DELAYED ~ OP_UNIQUE_CARRIER + WDF2.y+ TAVG.y+TAVG.x+ SNOW.x+ SNOW.y + PRCP.x+PRCP.y+ DEST 
                                           +AWND.x +AWND.y+ DISTANCE  + ORIGIN + WEEKDAY + WT_Origin + WT_Destination + CRS_DEP_TIME + CRS_ARR_TIME + 0 , 
                   data=class.train,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.04, 0.002)),
                   trControl = trainControl(method="cv", number=5,verboseIter = TRUE),
                   metric = "Accuracy")
train.cart$bestTune
prp(train.cart$finalModel, digits=3)
test.mm = as.data.frame(model.matrix(DELAYED ~ . + 0, data=class.test)) 
pred.best.cart = predict(train.cart$finalModel, newdata = test.mm, type="class")
t = table(class.test$DELAYED, pred.best.cart)
acc_log = sum(diag(t))/sum(t)
acc_log
