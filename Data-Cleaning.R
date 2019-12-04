library(data.table) # needed for function rbindlist
library(dplyr)
library(ggplot2)
library(lubridate)

# getting the data for 2019 in one df
files_2019 = list.files("242data/2019",pattern = ".csv")
temp_2019 = lapply(paste("242data/2019/",files_2019, sep = ""), read.csv, sep=",")
data_2019 <- rbindlist(temp_2019) 
# tranform FL Date into a Date variable instead of factor variable 
data_2019 <- transform(data_2019, FL_DATE = as.Date(FL_DATE))

# getting the data for 2018 in one df
files_2018 = list.files("242data/2018",pattern = ".csv")
temp_2018 = lapply(paste("242data/2018/",files_2018, sep = ""), read.csv, sep=",")
data_2018 <- rbindlist(temp_2018) 
# tranform FL Date into a Date variable instead of factor variable 
data_2018 <- transform(data_2018, FL_DATE = as.Date(FL_DATE))

# getting the data for 2017 in one df
files_2017 = list.files("242data/2017",pattern = ".csv")
temp_2017 = lapply(paste("242data/2017/",files_2017, sep = ""), read.csv, sep=",")
data_2017 <- rbindlist(temp_2017) 
# tranform FL Date into a Date variable instead of factor variable 
data_2017 <- transform(data_2017, FL_DATE = as.Date(FL_DATE))


data_2017$X = NULL
data_2018$X = NULL
data_2019$X = NULL
df_list = list( data_2017, data_2018, data_2019)
data_all = rbindlist(df_list)

head(data_all)


##### filtering for American Airlines ######
data_all_AA = filter(data_all, data_all$OP_UNIQUE_CARRIER == "AA")

#### Independent Variable ######

summary(data_all_AA$ARR_DELAY)

data_all_AA %>% ggplot(aes(x = ARR_DELAY)) + geom_histogram() + coord_cartesian(xlim=c(-100,500))
data_all_AA %>% ggplot(aes(x = ARR_DELAY)) + geom_density() + coord_cartesian(xlim=c(-100,500))

# data_all_AA %>% ggplot(aes(x = ARR_DELAY, y=ARR_DELAY )) + geom_point() 

#### Creating region variables ####
## Origin
data_all_AA$regionOrigin = rep(NA,2521943)
data_all_AA$regionDestination = rep(NA,2521943)

# WEST
west = c("Alaska",
         "Arizona",
         "California",
         "Colorado",
         "Hawaii",
         "Idaho",
         "Montana",
         "Nevada",
         "New Mexico",
         "Oregon",
         "Utah",
         "Washington",
         "Wyoming")
tf_west = match(data_all_AA$ORIGIN_STATE_NM, west) # numbers when it is part of west, NA's otherwise
data_all_AA$regionOrigin[tf_west != 0 ] = "West"
head(data_all_AA)

# MIDWEST
midwest = c("Illinois",
            "Indiana",
            "Iowa",
            "Kansas",
            "Michigan",
            "Missouri",
            "Minnesota",
            "Nebraska",
            "North Dakota",
            "Ohio",
            "South Dakota",
            "Wisconsin")
tf_midwest = match(data_all_AA$ORIGIN_STATE_NM, midwest) # numbers when it is part of west, NA's otherwise
data_all_AA$regionOrigin[tf_midwest != 0 ] = "Midwest"
head(data_all_AA)

# SOUTH
south = c("Alabama",
          "Arkansas",
          "Delaware",
          "Florida",
          "Georgia",
          "Kentucky",
          "Louisiana",
          "Maryland",
          "Mississippi",
          "Oklahoma",
          "North Carolina",
          "South Carolina",
          "Tennessee",
          "Texas",
          "Virginia",
          "West Virginia")

tf_south = match(data_all_AA$ORIGIN_STATE_NM, south) # numbers when it is part of west, NA's otherwise
data_all_AA$regionOrigin[tf_south != 0 ] = "South"
head(data_all_AA)
# NORTHEAST
northeast = c("Connecticut",
              "Maine",
              "New Hampshire",
              "Massachusetts",
              "New Jersey",
              "New York",
              "Pennsylvania",
              "Rhode Island",
              "Vermont")
tf_northeast = match(data_all_AA$ORIGIN_STATE_NM, northeast) # numbers when it is part of west, NA's otherwise
data_all_AA$regionOrigin[tf_northeast != 0 ] = "Northeast"
head(data_all_AA)


## Destination

## WEST
tf_west_dest = match(data_all_AA$DEST_STATE_NM, west) # numbers when it is part of west, NA's otherwise
data_all_AA$regionDestination[tf_west_dest != 0 ] = "West"
head(data_all_AA)

## MIDWEST
tf_midwest_dest = match(data_all_AA$DEST_STATE_NM, midwest) # numbers when it is part of west, NA's otherwise
data_all_AA$regionDestination[tf_midwest_dest != 0 ] = "Midwest"

## SOUTH
tf_south_dest = match(data_all_AA$DEST_STATE_NM, south) # numbers when it is part of west, NA's otherwise
data_all_AA$regionDestination[tf_south_dest != 0 ] = "South"

## NORTHEAST
tf_northeast_dest = match(data_all_AA$DEST_STATE_NM, northeast) # numbers when it is part of west, NA's otherwise
data_all_AA$regionDestination[tf_northeast_dest != 0 ] = "Northeast"
head(data_all_AA)


#####  Box Plots for Regions - Destination & Origin ####

# origin

data_all_AA$regionOrigin = as.factor(data_all_AA$regionOrigin)
data_all_AA %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_boxplot()
# data_all_AA %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_point() 

# destination

data_all_AA$regionDestination = as.factor(data_all_AA$regionDestination)
data_all_AA %>% ggplot(aes(x = regionDestination, y = ARR_DELAY)) + geom_boxplot()
# data_all_AA %>% ggplot(aes(x = regionDestination, y = ARR_DELAY)) + geom_point() 


#### Plot over time - Date of the flight ####

data_all_AA %>% ggplot(aes(x = FL_DATE, y = ARR_DELAY)) + geom_line()

# try grouped by month and taking the average to have a better plot

data_all_AA %>% group_by(month=floor_date(FL_DATE, "month")) %>%
  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = AverageDelay)) + geom_line()


#### NA values ####

naTF <- sapply(data_all_AA, anyNA)
dataNaCols <- colnames(data_all_AA[,naTF])
dataNaCols

#### Na's in Regions ####

table(is.na(data_all_AA$regionOrigin))
table(data_all_AA$ORIGIN_STATE_NM[is.na(data_all_AA$regionOrigin)])
# problem: the islands below florida are not captured by this division in regions
# I just put islands for them as the region
# Puerto Rico 13151 
# U.S. Virgin Islands 5221

# make them as character again to add Island as a new factor
data_all_AA$regionDestination = as.character(data_all_AA$regionDestination)
data_all_AA$regionOrigin = as.character(data_all_AA$regionOrigin)

data_all_AA$regionOrigin[is.na(data_all_AA$regionOrigin)] = "Island"
data_all_AA$regionDestination[is.na(data_all_AA$regionDestination)] = "Island"

# making them a factor again
data_all_AA$regionOrigin = as.factor(data_all_AA$regionOrigin)
data_all_AA$regionDestination = as.factor(data_all_AA$regionDestination)


##### NAs in ArrDelay #####
table(is.na(data_all_AA$ARR_DELAY)) # 52082 NA's
table(data_all_AA$CANCELLED[is.na(data_all_AA$ARR_DELAY)])
# most of the flights that do not have a value for the arrival delay were cancelled 
## decide to delete them 
df_final = data_all_AA[!is.na(data_all_AA$ARR_DELAY),]


# checking again for NA's
naTF <- sapply(df_final, anyNA)
dataNaCols <- colnames(df_final[,naTF])
dataNaCols
# the remaining columns are not interesting for us atm, thus don't do something about the 
# missing values


###### Outliers #####
#### Distance ####

# grouping does not help
#df_final %>% group_by(distance= DISTANCE) %>% 
#  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
#  ggplot(aes(x = distance, y = AverageDelay)) + geom_point()

df_final %>% ggplot(aes(x = DISTANCE, y = ARR_DELAY)) + geom_point()

df_final %>% ggplot(aes(x = DISTANCE, y = ARR_DELAY)) + geom_smooth() 
# looks like we need polynoms higher degrees (3rd or something)

## decision: everything that is more delayed than 24 hours, wont be considered as delayed
# rather cancelled, thus, wew cut ARR_DELAY at 1440 (60minutes*24)

##### decided not to cut! we want to use the model to make AA able to cancle those flights in
# advnace, thus we need to predcit them as well!

#table(df_final$ARR_DELAY >1440) #103 observations have a higher delay time, will be removed
#df_final <- df_final %>% filter(ARR_DELAY <= 1440)



#### Departure Time ####

table(df_final$CRS_DEP_TIME)
# as factor does not make sense, too many dep times, very individual, kinda continuous
df_final %>% 
  ggplot(aes(x = CRS_DEP_TIME, y = ARR_DELAY)) + geom_point() 

df_final %>% 
  ggplot(aes(x = CRS_DEP_TIME, y = ARR_DELAY)) + geom_snooth() # looks like we need pol at least 3rd
#df_final %>% 
#  ggplot(aes(x = CRS_DEP_TIME, y = ARR_DELAY, color= DEST)) + geom_point() 
# color does not really make it better at all

#### Arrival Time #####
table(df_final$CRS_ARR_TIME)

df_final %>% 
  ggplot(aes(x = CRS_ARR_TIME, y = ARR_DELAY)) + geom_point() 

df_final %>% 
  ggplot(aes(x = CRS_ARR_TIME, y = ARR_DELAY)) + geom_smooth() # looks like we need pol at least 3rd

#### Region Destination ####

df_final %>% ggplot(aes(x = regionDestination, y = ARR_DELAY)) + geom_boxplot()
df_final %>% ggplot(aes(x = regionDestination, y = ARR_DELAY)) + geom_point() 

##### Region Origin #####

df_final %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_boxplot()
df_final %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_point() 


#### Departure Delay ####
# I know we might not have this, my idea is to predict this in the 
# first step and then use it as a feature

df_final %>% ggplot(aes(x = DEP_DELAY, y = ARR_DELAY)) + geom_point() 
df_final %>% ggplot(aes(x = DEP_DELAY, y = ARR_DELAY)) + geom_smooth() # is a straight line :D

#### Add Weather Data ####

# Read data
weather <- read.csv("242data/Weather.csv")
weather <- transform(weather, DATE = as.Date(as.character(DATE),"%m/%d/%y"))
# merge datasets
df_final_weather <- merge(x= df_final, y = weather, by.x = c("ORIGIN","FL_DATE"), by.y = c("Airport.CODE","DATE"))
df_final_weather <- merge(x= df_final_weather, y = weather, by.x = c("DEST","FL_DATE"), by.y = c("Airport.CODE","DATE"))
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

df_final_weather[which((!is.na(df_final_weather$AWND.x))&
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
                           (!is.na(df_final_weather$WT_Destination))),])

## All weather variables
#Average wind speed (AWND);Direction of fastest 2-minute wind (WDF2)
#Precipitation (PRCP)
#Snowfall (SNOW)
#Average Temperature. (TAVG)
#Weather Type (WT)

#visualization
ggplot(data=df_final_weather[which(data_all_AA_weather$AWND.x<20),], aes(x=AWND.x, y=ARR_DELAY))+geom_point()
ggplot(data=df_final_weather[which(data_all_AA_weather$AWND.x<20),], aes(x=AWND.x, y=ARR_DELAY))+geom_smooth()

ggplot(data=df_final_weather[which(data_all_AA_weather$PRCP.x<10),], aes(x=PRCP.x, y=ARR_DELAY))+geom_point()
ggplot(data=df_final_weather[which(data_all_AA_weather$PRCP.x<10),], aes(x=PRCP.x, y=ARR_DELAY))+geom_smooth()


ggplot(data=df_final_weather, aes(x=TAVG.y, y=ARR_DELAY))+geom_point()
ggplot(data=df_final_weather, aes(x=TAVG.y, y=ARR_DELAY))+geom_smooth()

df_final <- df_final_weather
###### Splitting Data ####### (for regression)

# most recent data for test and evaluation 
# looking at the last 12 month and sampling data from those for test and evaluation to get
# flights from all months (and not only the most recent ones)
date = as.Date(c("2018-09-30"))
date
month(date)
test_val_data = df_final %>% filter(FL_DATE > date)
nrow(test_val_data)
number_obs_last12 = nrow(test_val_data)
train_data = df_final %>% filter(FL_DATE <= date)
total_number_obs = nrow(df_final)
percentage_of_data_we_sample_from = round(number_obs_last12/total_number_obs*100)


## getting data for every month
test_val_data_Jan = test_val_data %>% filter(month(FL_DATE) == 1)
test_val_data_Feb = test_val_data %>% filter(month(FL_DATE) == 2)
test_val_data_Mar = test_val_data %>% filter(month(FL_DATE) == 3)
test_val_data_Apr = test_val_data %>% filter(month(FL_DATE) == 4)
test_val_data_May = test_val_data %>% filter(month(FL_DATE) == 5)
test_val_data_Jun = test_val_data %>% filter(month(FL_DATE) == 6)
test_val_data_Jul = test_val_data %>% filter(month(FL_DATE) == 7)
test_val_data_Aug = test_val_data %>% filter(month(FL_DATE) == 8)
test_val_data_Sep = test_val_data %>% filter(month(FL_DATE) == 9)
test_val_data_Oct = test_val_data %>% filter(month(FL_DATE) == 10)
test_val_data_Nov = test_val_data %>% filter(month(FL_DATE) == 11)
test_val_data_Dec = test_val_data %>% filter(month(FL_DATE) == 12)


# 92% training 
# 4% test
# 2% each validation set

#### test set #####
percentage_test_set = 4
p = percentage_test_set/percentage_of_data_we_sample_from

rows_test_jan = sample(nrow(test_val_data_Jan), p*nrow(test_val_data_Jan))
test_jan = test_val_data_Jan[rows_test_jan,]
rest_jan = test_val_data_Jan[-rows_test_jan,]

rows_test_feb = sample(nrow(test_val_data_Feb), p*nrow(test_val_data_Feb))
test_feb = test_val_data_Feb[rows_test_feb,]
rest_feb = test_val_data_Feb[-rows_test_feb,]

rows_test_mar = sample(nrow(test_val_data_Mar), p*nrow(test_val_data_Mar))
test_mar = test_val_data_Mar[rows_test_mar,]
rest_mar = test_val_data_Mar[-rows_test_mar,]

rows_test_apr = sample(nrow(test_val_data_Apr), p*nrow(test_val_data_Apr))
test_apr = test_val_data_Apr[rows_test_apr,]
rest_apr = test_val_data_Apr[-rows_test_apr,]

rows_test_may = sample(nrow(test_val_data_May), p*nrow(test_val_data_May))
test_may = test_val_data_May[rows_test_may,]
rest_may = test_val_data_May[-rows_test_may,]

rows_test_jun = sample(nrow(test_val_data_Jun), p*nrow(test_val_data_Jun))
test_jun = test_val_data_May[rows_test_jun,]
rest_jun = test_val_data_May[-rows_test_jun,]

rows_test_jul = sample(nrow(test_val_data_Jul), p*nrow(test_val_data_Jul))
test_jul = test_val_data_Jul[rows_test_jul,]
rest_jul = test_val_data_Jul[-rows_test_jul,]

rows_test_aug = sample(nrow(test_val_data_Aug), p*nrow(test_val_data_Aug))
test_aug = test_val_data_Aug[rows_test_aug,]
rest_aug = test_val_data_Aug[-rows_test_aug,]

rows_test_sep = sample(nrow(test_val_data_Sep), p*nrow(test_val_data_Sep))
test_sep = test_val_data_Sep[rows_test_sep,]
rest_sep = test_val_data_Sep[-rows_test_sep,]

rows_test_oct = sample(nrow(test_val_data_Oct), p*nrow(test_val_data_Oct))
test_oct = test_val_data_Oct[rows_test_oct,]
rest_oct = test_val_data_Oct[-rows_test_oct,]

rows_test_nov = sample(nrow(test_val_data_Nov), p*nrow(test_val_data_Nov))
test_nov = test_val_data_Nov[rows_test_nov,]
rest_nov = test_val_data_Nov[-rows_test_nov,]

rows_test_dec = sample(nrow(test_val_data_Dec), p*nrow(test_val_data_Dec))
test_dec = test_val_data_Dec[rows_test_dec,]
rest_dec = test_val_data_Dec[-rows_test_dec,]

#### test set ####
test_list = list( test_jan, test_feb, test_mar, test_apr, test_may, test_jun, test_jul, test_aug
                  , test_sep, test_oct, test_nov, test_dec)

test_data = rbindlist(test_list)

##### getting validation sets ####
percentage_val1 = 2
p_val1 = percentage_val1/(percentage_of_data_we_sample_from - percentage_test_set)
# 2% for val 1
val1.ids_jan <- sample(nrow(rest_jan), p_val1*nrow(rest_jan))
val1_jan <- rest_jan[val1.ids_jan,]
rest_jan <- rest_jan[-val1.ids_jan,]

val1.ids_feb <- sample(nrow(rest_feb), p_val1*nrow(rest_feb))
val1_feb <- rest_feb[val1.ids_feb,]
rest_feb <- rest_feb[-val1.ids_feb,]

val1.ids_mar <- sample(nrow(rest_mar), p_val1*nrow(rest_mar))
val1_mar <- rest_mar[val1.ids_mar,]
rest_mar <- rest_mar[-val1.ids_mar,]

val1.ids_apr <- sample(nrow(rest_apr), p_val1*nrow(rest_apr))
val1_apr <- rest_apr[val1.ids_apr,]
rest_apr <- rest_apr[-val1.ids_apr,]

val1.ids_may <- sample(nrow(rest_may), p_val1*nrow(rest_may))
val1_may <- rest_may[val1.ids_may,]
rest_may <- rest_may[-val1.ids_may,]

val1.ids_jun <- sample(nrow(rest_jun), p_val1*nrow(rest_jun))
val1_jun <- rest_jun[val1.ids_jun,]
rest_jun <- rest_jun[-val1.ids_jun,]

val1.ids_jul <- sample(nrow(rest_jul), p_val1*nrow(rest_jul))
val1_jul <- rest_jul[val1.ids_jul,]
rest_jul <- rest_jul[-val1.ids_jul,]

val1.ids_aug <- sample(nrow(rest_aug), p_val1*nrow(rest_aug))
val1_aug <- rest_aug[val1.ids_aug,]
rest_aug <- rest_aug[-val1.ids_aug,]

val1.ids_sep <- sample(nrow(rest_sep), p_val1*nrow(rest_sep))
val1_sep <- rest_sep[val1.ids_sep,]
rest_sep <- rest_sep[-val1.ids_sep,]

val1.ids_oct <- sample(nrow(rest_oct), p_val1*nrow(rest_oct))
val1_oct <- rest_oct[val1.ids_oct,]
rest_oct <- rest_oct[-val1.ids_oct,]

val1.ids_nov <- sample(nrow(rest_nov), p_val1*nrow(rest_nov))
val1_nov <- rest_nov[val1.ids_nov,]
rest_nov <- rest_nov[-val1.ids_nov,]

val1.ids_dec <- sample(nrow(rest_dec), p_val1*nrow(rest_dec))
val1_dec <- rest_dec[val1.ids_dec,]
rest_dec <- rest_dec[-val1.ids_dec,]

val1_list = list( val1_jan, val1_feb, val1_mar, val1_apr, val1_may, val1_jun, val1_jul,
                  val1_aug, val1_sep, val1_oct, val1_nov, val1_dec)

val1_data = rbindlist(val1_list)


##### second validation set #######
percentage_val2 = 2
p_val2 = percentage_val2/(percentage_of_data_we_sample_from - percentage_test_set - percentage_val1 )
# 2% for val 2
val2.ids_jan <- sample(nrow(rest_jan), p_val2*nrow(rest_jan))
val2_jan <- rest_jan[val2.ids_jan,]
rest_jan <- rest_jan[-val2.ids_jan,]

val2.ids_feb <- sample(nrow(rest_feb), p_val2*nrow(rest_feb))
val2_feb <- rest_feb[val2.ids_feb,]
rest_feb <- rest_feb[-val2.ids_feb,]

val2.ids_mar <- sample(nrow(rest_mar), p_val2*nrow(rest_mar))
val2_mar <- rest_mar[val2.ids_mar,]
rest_mar <- rest_mar[-val2.ids_mar,]

val2.ids_apr <- sample(nrow(rest_apr), p_val2*nrow(rest_apr))
val2_apr <- rest_apr[val2.ids_apr,]
rest_apr <- rest_apr[-val2.ids_apr,]

val2.ids_may <- sample(nrow(rest_may), p_val2*nrow(rest_may))
val2_may <- rest_may[val2.ids_may,]
rest_may <- rest_may[-val2.ids_may,]

val2.ids_jun <- sample(nrow(rest_jun), p_val2*nrow(rest_jun))
val2_jun <- rest_jun[val2.ids_jun,]
rest_jun <- rest_jun[-val2.ids_jun,]

val2.ids_jul <- sample(nrow(rest_jul), p_val2*nrow(rest_jul))
val2_jul <- rest_jul[val2.ids_jul,]
rest_jul <- rest_jul[-val2.ids_jul,]

val2.ids_aug <- sample(nrow(rest_aug), p_val2*nrow(rest_aug))
val2_aug <- rest_aug[val2.ids_aug,]
rest_aug <- rest_aug[-val2.ids_aug,]

val2.ids_sep <- sample(nrow(rest_sep), p_val2*nrow(rest_sep))
val2_sep <- rest_sep[val2.ids_sep,]
rest_sep <- rest_sep[-val2.ids_sep,]

val2.ids_oct <- sample(nrow(rest_oct), p_val2*nrow(rest_oct))
val2_oct <- rest_oct[val2.ids_oct,]
rest_oct <- rest_oct[-val2.ids_oct,]

val2.ids_nov <- sample(nrow(rest_nov), p_val2*nrow(rest_nov))
val2_nov <- rest_nov[val2.ids_nov,]
rest_nov <- rest_nov[-val2.ids_nov,]

val2.ids_dec <- sample(nrow(rest_dec), p_val2*nrow(rest_dec))
val2_dec <- rest_dec[val2.ids_dec,]
rest_dec <- rest_dec[-val2.ids_dec,]

val2_list = list( val2_jan, val2_feb, val2_mar, val2_apr, val2_may, val2_jun, val2_jul,
                  val2_aug, val2_sep, val2_oct, val2_nov, val2_dec)

val2_data = rbindlist(val2_list)


###### adding everything we did not use from the test_val data from the last 12 month back
# to the training data 

train_list = list( train_data, rest_jan, rest_feb, rest_mar, rest_apr, rest_may, rest_jun, rest_jul,
                   rest_aug, rest_sep, rest_oct, rest_nov, rest_dec)

train_data = rbindlist(train_list)

