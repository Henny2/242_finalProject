library(data.table) # needed for function rbindlist
library(dplyr)
library(ggplot2)
library(lubridate)
library(caTools)
library(randomForest)
library(caret)

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
dataNaCols_new <- colnames(df_final_weather[,naTF_new])
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

set.seed(1234)
split = sample.split(df_final$DELAYED, SplitRatio = 0.7) 
class.train <- filter(df_final, split == TRUE) 
class.test <- filter(df_final, split == FALSE)



######## Logistic Regression ######
log_reg <- glm(DELAYED ~ OP_UNIQUE_CARRIER+  DEST+ ORIGIN + WEEKDAY + WT_Origin + WT_Destination , data=class.train, family=binomial(link='logit'), maxit = 100)
summary(log_reg)

predTestLog <- predict(log_reg, newdata=class.test, type="response") # type on default gives log-odds, "response" gives predicted probabilities
table(predTestLog >0.5)
t = table(class.test$DELAYED, predTestLog>0.5)
t

acc_log = sum(diag(t)) / sum(t)
acc_log


######## Random Forest Regression ######

set.seed(456)
mod.rf <- randomForest(DELAYED ~ .-FL_DATE - ORIGIN_CITY_NAME - ORIGIN_STATE_NM - DEST_CITY_NAME - DEST_STATE_NM - CRS_DEP_TIME - CRS_ARR_TIME - ARR_DELAY, data = class.train)

pred.rf <- predict(mod.rf, newdata = class.test) # just to illustrate

# get the confusion matrix of the results
t = table(class.test$DELAYED, pred.rf)
t
# accuracy:
acc_rf = sum(diag(t)) / sum(t)
acc_rf

# try the cross-validated RF model
set.seed(456)
train.rf <- train(DELAYED ~ .-FL_DATE - ORIGIN_CITY_NAME 
                  - ORIGIN_STATE_NM - DEST_CITY_NAME - DEST_STATE_NM 
                  - CRS_DEP_TIME - CRS_ARR_TIME - ARR_DELAY,
                  data = class.train,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:20),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy")
train.rf$results
train.rf
best.rf <- train.rf$finalModel

test.class.mm = as.data.frame(model.matrix(DELAYED ~ .-FL_DATE - ORIGIN_CITY_NAME 
                                            - ORIGIN_STATE_NM - DEST_CITY_NAME - DEST_STATE_NM 
                                            - CRS_DEP_TIME - CRS_ARR_TIME - ARR_DELAY + 0, data=class.test)) 


pred.best.rf <- predict(best.rf, newdata = test.class.mm) 


# make a graph and find the best mtry
ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 2) + geom_line() +
  ggtitle("Random Forest Accuracy VS mtry")
ylab("Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

t <- table(class.test$DELAYED, pred.best.rf)
t
acc_best_rf <- sum(diag(t)) / sum(t)
acc_best_rf





