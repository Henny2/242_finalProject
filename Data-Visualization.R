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

# getting the data for 2016 in one df
files_2016 = list.files("242data/2016",pattern = ".csv")
temp_2016 = lapply(paste("242data/2016/",files_2016, sep = ""), read.csv, sep=",")
data_2016 <- rbindlist(temp_2016) 
# tranform FL Date into a Date variable instead of factor variable 
data_2016 <- transform(data_2016, FL_DATE = as.Date(FL_DATE))

data_2016$X = NULL
data_2017$X = NULL
data_2018$X = NULL
data_2019$X = NULL
df_list = list(data_2016, data_2017, data_2018, data_2019)
data_all = rbindlist(df_list)


########## Data Exploration #########

# plot average delay ~ month of flight
data_all %>% group_by(month=floor_date(FL_DATE, "month")) %>%
  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = AverageDelay)) + geom_line()
head(data_all)


# plot average delay in month by carrier 
data_all %>% group_by(date= FL_DATE, carrier = OP_UNIQUE_CARRIER) %>% 
  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = AverageDelay, color=carrier)) + geom_line()

# plot averagedelay by distance
data_all %>% group_by(distance= DISTANCE) %>% 
  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = distance, y = AverageDelay)) + geom_point()
# also grouped by carrier
data_all %>% group_by(distance= DISTANCE, carrier = OP_UNIQUE_CARRIER) %>% 
  summarize(AverageDelay = mean(ARR_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = distance, y = AverageDelay, color=carrier)) + geom_point()

# plot for connection arr delay and departure delay
data_2019 %>% 
  ggplot(aes(x = DEP_DELAY, y = ARR_DELAY, color=OP_UNIQUE_CARRIER)) + geom_point()


##### only using American Airline #######
data_2019_AA = filter(data_2019, data_2019$OP_UNIQUE_CARRIER == "AA")
data_all_AA = filter(data_all, data_all$OP_UNIQUE_CARRIER == "AA")

# plotting the Arr-Delay against the Dep-Delay
plot = data_2019_AA %>% 
  ggplot(aes(x = DEP_DELAY, y = ARR_DELAY)) + geom_point()
plot


head(data_2019_AA)

# ideas: Dep Time, Origin, Dest, Weather Delay


# plotting the arr delay againt the Dep time
# as factor does not make sense, too many dep times, very individual
table(data_2019_AA$CRS_DEP_TIME)
table(data_2019_AA$ORIGIN)

table(data_2019_AA$ORIGIN == "VEL")
plot_DepTime = data_2019_AA %>% 
  ggplot(aes(x = CRS_DEP_TIME, y = ARR_DELAY)) + geom_point() 
plot_DepTime


# plotting the arr delas against the origin
table(data_2019_AA$ORIGIN)
plot_Origin = data_2019_AA %>% 
  ggplot(aes(x = ORIGIN, y = ARR_DELAY)) + geom_point() 
plot_Origin

plot_hist = data_2019_AA %>% 
  ggplot(aes(x=ARR_DELAY, color=ORIGIN)) + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(-12,500))

plot_hist
str(data_2019_AA$ORIGIN)
data_2019_AA$ORIGIN = as.character(data_2019_AA$ORIGIN)
data_2019_AA$ORIGIN = as.factor(data_2019_AA$ORIGIN)
# ggplot(data_2019_AA, aes(x=ARR_DELAY)) + geom_histogram(binwidth=1) + facet_grid(~ ORIGIN) does not work




### plotting against destination

table(data_2019_AA$DEST)
plot_Dest = data_2019_AA %>% 
  ggplot(aes(x = DEST, y = ARR_DELAY)) + geom_point() 
plot_Dest

plot_hist_dest = data_2019_AA %>% 
  ggplot(aes(x=ARR_DELAY, color=DEST)) + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(-12,500))


data_2019_AA %>% ggplot(aes(x = ORIGIN, y = ARR_DELAY)) + geom_boxplot()

plot_hist_dest
str(data_2019_AA$DEST)
# getting rid of factors that are not used 
data_2019_AA$DEST = as.character(data_2019_AA$DEST)
data_2019_AA$DEST = as.factor(data_2019_AA$DEST)

# idea maybe we can group them in different regions insetad of every single city/airport 
# idea to look up how america is split into parts (e.g East, midwest, west...)





# first do hist of the "independent variable"
data_all_AA %>% ggplot(aes(x = ARR_DELAY)) + geom_density()+ coord_cartesian(xlim=c(-50,250), ylim = c(0,0.07))
# looks right tailed -> try log(Delay)
data_all_AA %>% ggplot(aes(x = log(ARR_DELAY))) + geom_density()
# log makes it worse :D


## looking for NAs
table(is.na(data_all_AA))

# todo: 
# 1. do something about the NAs
# 2. create region variables and maybe take that for dest/origin plots


########### Handling Na's #############
head(data_all_AA)
# na in Arr delay 
#### idea: delete those rows (total 65165), most of them are cancelled flights
table(is.na(data_all_AA$ARR_DELAY))
table(data_all_AA$CANCELLED[is.na(data_all_AA$ARR_DELAY)])

# na in FL DAte
table(is.na(data_all_AA$FL_DATE))
#### no nas here


# nas in weather delay 
table(is.na(data_all_AA$WEATHER_DELAY))
#### only 600,000 wihtout na, so predicting the weather based on the delay is not possible

# nas in origin/dest
table(is.na(data_all_AA$ORIGIN)) # 0 na's
table(is.na(data_all_AA$DEST)) # 0 na's

# na's in distance
table(is.na(data_all_AA$DISTANCE)) # 0 na's

# na's in distance
table(is.na(data_all_AA$CRS_DEP_TIME)) # 0 na's

########## creating region variables ########

test_df = data_2019_AA
test_df$regionOrigin = rep(NA, 708777)
# WEST
'Alaska,
Arizona,
California,
Colorado,
Hawaii,
Idaho,
Montana,
Nevada,
New Mexico,
Oregon,
Utah,
Washington,
Wyoming'

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

tf_west = match(test_df$ORIGIN_STATE_NM, west) # numbers when it is part of west, NA's otherwise
test_df$regionOrigin[tf_west] = "nööö"
test_df$regionOrigin[is.na(tf_west)] = FALSE
test_df$regionOrigin[tf_west != 0 ] = "West"
tf_west
head(test_df)
# MIDWEST
'Illinois,
Indiana,
Iowa,
Kansas,
Michigan,
Missouri,
Minnesota,
Nebraska,
North Dakota,
Ohio,
South Dakota,
Wisconsin'

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
tf_midwest = match(test_df$ORIGIN_STATE_NM, midwest) # numbers when it is part of west, NA's otherwise
test_df$regionOrigin[tf_midwest != 0 ] = "Midwest"

head(test_df)

# SOUTH
'Alabama,
Arkansas,
Delaware,
Florida,
Georgia,
Kentucky,
Louisiana,
Maryland,
Mississippi,
Oklahoma,
North Carolina,
South Carolina,
Tennessee,
Texas,
Virginia,
West Virginia'
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


tf_south = match(test_df$ORIGIN_STATE_NM, south) # numbers when it is part of west, NA's otherwise
test_df$regionOrigin[tf_south != 0 ] = "South"
tf_south
head(test_df)
# NORTHEAST
'Connecticut,
Maine,
New Hampshire,
Massachusetts,
New Jersey,
New York,
Pennsylvania,
Rhode Island,
Vermont'
northeast = c("Connecticut",
             "Maine",
             "New Hampshire",
             "Massachusetts",
             "New Jersey",
             "New York",
             "Pennsylvania",
             "Rhode Island",
             "Vermont")
tf_northeast = match(test_df$ORIGIN_STATE_NM, northeast) # numbers when it is part of west, NA's otherwise
test_df$regionOrigin[tf_northeast != 0 ] = "Northeast"
tf_northeast
head(test_df)


#### plotting the region of the origin against the delay #####

test_df$regionOrigin = as.factor(test_df$regionOrigin)
test_df %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_boxplot()
test_df %>% ggplot(aes(x = regionOrigin, y = ARR_DELAY)) + geom_point() 


####### to do #####
# 1. regions for destination
# 2. smoothed plots to see whether we need to use polynoms 
# 3. dealing with NA's in general 
'naTF <- sapply(ames, anyNA)
amesNaCols <- colnames(ames[,naTF])
amesNaCols'

table(data_all_AA$ARR_DELAY != 0)


