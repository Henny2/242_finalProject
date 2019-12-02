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




