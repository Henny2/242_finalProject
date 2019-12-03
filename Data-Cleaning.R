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



###### Splitting Data ####### (for regression)

# most recent data for test and evaluation 
# looking at the last 12 month and sampling data from those for test and evaluation to get
# flights from all months (and not only the most recent ones)
date = as.Date(c("2018-09-30"))
date
month(date)
test_val_data = df_final %>% filter(FL_DATE > date)

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




rows_test_jan = sample(nrow(test_val_data_Jan), 0.01*nrow(test_val_data_Jan))
test_jan = test_val_data_Jan[rows_test_jan,]

rows_test_feb = sample(nrow(test_val_data_Feb), 0.01*nrow(test_val_data_Feb))
test_feb = test_val_data_Feb[rows_test_feb,]

rows_test_mar = sample(nrow(test_val_data_Mar), 0.01*nrow(test_val_data_Mar))
test_mar = test_val_data_Mar[rows_test_mar,]

rows_test_apr = sample(nrow(test_val_data_Apr), 0.01*nrow(test_val_data_Apr))
test_apr = test_val_data_Mar[rows_test_apr,]

