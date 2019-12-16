library(data.table) # needed for function rbindlist
library(dplyr)
library(ggplot2)
library(lubridate)
library(car)
library(ROCR)
library(caTools)
library(MASS)
library(glmnet)
library(boot)
library(rpart)
library(caret)
library(tictoc) #for timing R scripts

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

df_final = pickeddata[!is.na(pickeddata$ARR_DELAY),]


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

delays <- df_final%>% group_by(DELAYED) %>% summarise(delays = mean(ARR_DELAY))
avg.delay <- as.numeric(as.matrix(delays)[2,2])


#threshold = 75/(avg.delay*80.25/60 + 27)

# deleting the ARR_DELAY column to not use it as a feature
df_final$ARR_DELAY = NULL

set.seed(1234)
split = sample.split(df_final$DELAYED, SplitRatio = 0.7) 
class.train <- filter(df_final, split == TRUE) 
class.test <- filter(df_final, split == FALSE)

library(keras)
tensorflow::tf$random$set_seed(564)
# Prep for Keras
TrainX <- model.matrix(DELAYED ~ . , data = class.train)
head(TrainX)
str(TrainX)
TrainX = TrainX[,2:101]
TrainY <- model.matrix(~ DELAYED -1, data = class.train)

TestX <- model.matrix(DELAYED ~ . , data = class.test)
TestX = TestX[,2:101]
TestY <- model.matrix(~ DELAYED -1, data = class.test)

#### Model 1####
# Single hidden layer model sigmoid
nn_mod_1 <- keras_model_sequential() 
# Sequential models are created using the keras_model_sequential() function and are composed of a set of linear layers
# Add A Densely-Connected NN Layer To An Output using layer_dense()
# It has arguments object: model, units: number of units, input_shape: Dimensionality of the input, activation: choose activation function
nn_mod_1 %>%
  layer_dense(units = 100, activation = "sigmoid", input_shape = c(100)) %>% # Adding the hidden layer shape 100 for 100 categories
  layer_dense(units = 2, activation = "softmax") # adding the output layer  # output has 2 layers for our 2 categories

summary(nn_mod_1)

#Before training a model, we need to configure the learning process, which is done via the compile() function.
#It receives three arguments:
#1. An optimizer. 
#This could be the string identifier of an existing optimizer (e.g. as “rmsprop” or “adagrad”) or a call to an optimizer function (e.g. optimizer_sgd()).
#2. A loss function. 
#This is the objective that the model will try to minimize. It can be the string identifier of an existing loss function (e.g. “categorical_crossentropy” or “mse”) or a call to a loss function (e.g. loss_mean_squared_error()).
#3. A list of metrics. 
#For any classification problem you will want to set this to metrics = c('accuracy'). A metric could be the string identifier of an existing metric or a call to metric function (e.g. metric_binary_crossentropy()).

# rmsprop is basiclly : Divide the gradient by a running average of its recent magnitude. You can google it to get more information.
# Consider it as a sophisticated way to get gradient in gradient descent.

nn_mod_1 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 1:") # Neural Net 1:: 92.882 sec elapsed
training_history <- nn_mod_1 %>% 
  fit(TrainX, TrainY, 
      epochs = 100, validation_split = 0.2)       # epos maybe number of steps in sgd, another way how to overfit 
toc()

# evaluate
nn_mod_1 %>% evaluate(TestX, TestY)
nn_mod_1
##### change units of first layer #####
# Single hidden layer model sigmoid

nn_mod_1b <- keras_model_sequential() 

nn_mod_1b %>%
  layer_dense(units = 125, activation = "sigmoid", input_shape = c(100)) %>% # Adding the hidden layer shape 100 for 100 categories
  layer_dense(units = 2, activation = "softmax") # adding the output layer  # output has 2 layers for our 2 categories

summary(nn_mod_1b)


nn_mod_1b %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 1b:") # Neural Net 1:: 92.882 sec elapsed
training_history <- nn_mod_1b %>% 
  fit(TrainX, TrainY, 
      epochs = 100, validation_split = 0.2)       # epos maybe number of steps in sgd, another way how to overfit 
toc()

# evaluate
nn_mod_1b %>% evaluate(TestX, TestY)





####### Model 2######


# Single hidden layer model ReLU
# Switching sigmoid to ReLU max(0,a)

nn_mod_2 <- keras_model_sequential() 
nn_mod_2 %>%
  layer_dense(units = 120, activation = "relu", input_shape = c(100)) %>%
  layer_dense(units = 2, activation = "softmax")
summary(nn_mod_2)

nn_mod_2 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 2:")
training_history <- nn_mod_2 %>% 
  fit(TrainX, TrainY, 
      epochs = 100, validation_split = 0.2)
toc()

# evaluate
nn_mod_2 %>% evaluate(TestX, TestY)

