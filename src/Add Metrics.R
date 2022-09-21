## Libraries

# library(conflicted)
# library(tidyverse)
# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("lag", "dplyr")
# library(lubridate)
# library(geosphere)
# conflicted::conflict_prefer("summarise", "dplyr")
# conflicted::conflict_prefer("select", "dplyr")
# library(glue)


## ________________________________________________________________________

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/01_NSR_tracking_data.RData"))

raw_data <- data
range(raw_data$timestamp)

unique(raw_data$id)
glimpse(raw_data)

#library(plyr)
data <- raw_data %>%
  # filter(timestamp > "2000-01-01") %>%
  mutate(id = as_factor(id)) %>%
  arrange(id, timestamp) %>%
  mutate_at(vars(timestamp), funs(year, month, day, hour, yday))

data %>%
  group_by(id) %>%
  tally()

# data<-data365#only run this line if you are using old data
# unique(data$id)
# glimpse(data)
#
# data$timestamp<-parse_date_time(data$timestamp, orders=c("dmy_HMS", "mdy_HMS", "ymd_HM",  "ymd_HMS"))
# class(data$timestamp)
data$timestamp[1:5]
tz(data$timestamp) <- "Africa/Johannesburg"
range(data$timestamp)

# #extract only hourly fixes from dataset
# data_hour <- data %>%
#   mutate(hour = hour(timestamp)) %>%
#   mutate(date = ymd(str_c(year(timestamp),month(timestamp),day(timestamp), sep = "-"))) %>%
#   group_by(id, date ,hour) %>%
#   filter(row_number() == 1)
# 
# ## Convert to tibble
# data <- as(data_hour, "data.frame") %>%
#   as_tibble()

data %>%
  group_by(id) %>%
  tally()


range(data$timestamp)

# # Calculate sunrise & sunset times ----------------------------------------
# data <- data_hour %>%
#   filter(timestamp > "2000-01-01") %>%
#   mutate(id = as_factor(id)) %>%
#   arrange(id, timestamp) %>%
#   mutate_at(vars(timestamp), funs(year, month, day, hour, yday))
#
# # range(raw_data$timestamp)
#
# ## Convert to tibble
# data <- as(data, "data.frame") %>%
#   as_tibble()

coords <- data %>%
  select(longitude, latitude) %>%
  as.matrix()

datetime <- data %>%
  pull(timestamp)

sunrise <- maptools::crepuscule(coords, datetime, solarDep = 6,
                                direction = "dawn", POSIXct.out = TRUE)$time

sunset <- maptools::crepuscule(coords, datetime, solarDep = 6,
                               direction = "dusk", POSIXct.out = TRUE)$time

data <- data %>%
  mutate(sunrise = sunrise, sunset = sunset) %>%
  mutate(dayNight = if_else(timestamp > sunrise & timestamp < sunset, "day", "night"))

## Set order
data <- data %>%
  arrange(id, timestamp)

data %>%
  group_by(id) %>%
  tally()

# range(data$dist_move_m)

# Distance ----------------------------------------------------------------

# TODO add new columns that start with zero every day so which accounts for big gaps in telemetry

## Function to create distances for each individual
calc_distance <- function(x) {

   df <- data %>%
     filter(id %in% x) %>%
     mutate(long_lead = lead(longitude), lat_lead = lead(latitude)) %>%
     rowwise() %>%
     mutate(dist_move_m = as.numeric(distm(
       x = c(longitude, latitude),
       y = c(long_lead, lat_lead)
     ))) %>%
     ungroup() %>%
     mutate(dist_move_m = lag(dist_move_m)) %>%
     mutate(dist_move_m = if_else(is.na(dist_move_m), 0, dist_move_m)) %>%
     mutate(dist_move_km = dist_move_m/1000) %>%
     return(df)
 }

data <- map_df(.x = unique(data$id),
                .f = calc_distance)

## Function to create distances for each individual calculated by day
# calc_distance <- function(x) {
#   
#   df <- data %>%
#     filter(id %in% x) %>%
#     group_by(year,month,day) %>%
#     mutate(long_lead = lead(longitude), lat_lead = lead(latitude)) %>%
#     rowwise() %>%
#     mutate(dist_move_m = as.numeric(distm(
#       x = c(longitude, latitude),
#       y = c(long_lead, lat_lead)
#     ))) %>%
#     ungroup() %>%
#     mutate(dist_move_m = lag(dist_move_m)) %>%
#     mutate(dist_move_m = if_else(is.na(dist_move_m), 0, dist_move_m)) %>%
#     mutate(dist_move_km = dist_move_m/1000) %>%
#     
#     return(df)
# }
# 
# data <- map_df(.x = unique(data$id),
#                .f = calc_distance)


data
# a=ddply(data,c("id"), summarise,
#                     n=length(id),
#                     tot_d=sum(na.omit(dist_move_km)))
# a


# Time --------------------------------------------------------------------

## Function to create time between points for each individual
calc_elev <- function(x){
  
  df <- data %>%
    filter(id %in% x) %>%
    group_by(year,month,day) %>%
    mutate(GPSAlt_lead = lead(GPSAlt)) %>%
    rowwise() %>%
    mutate(change_in_ele = GPSAlt_lead - GPSAlt) %>%
    ungroup() %>%
    mutate(change_in_ele = lag(change_in_ele)) %>%
    mutate(change_in_ele = if_else(is.na(change_in_ele), 0, change_in_ele))
  
  return(df)
}

data <- map_df(.x = unique(data$id),
               .f = calc_elev)

# Elevation ---------------------------------------------------------------

## Function to calculate time difference between points
calc_time <- function(x){
  
  df <- data %>%
    filter(id %in% x) %>%
    group_by(year,month,day) %>%
    mutate(time_diff_mins = difftime(timestamp, lag(timestamp, default = first(timestamp)),
                                     units = "mins"),
           time_diff_secs = as.numeric(time_diff_mins, units = 'secs'))
  
  return(df)
}

data <- map_df(.x = unique(data$id),
               .f = calc_time)

# Check numeric -----------------------------------------------------------

## Make sure times and distances are numeric
data <- data %>%
  mutate(across(.cols = c(dist_move_m, dist_move_km, change_in_ele,
                          time_diff_mins, time_diff_secs),
                .fn = as.numeric))

# Speed -------------------------------------------------------------------
data <- data %>%
  mutate(
    speed_m_s = if_else(time_diff_secs == 0, 0, dist_move_m / time_diff_secs),
    speed_km_h = speed_m_s * 3.6,
    lowess.speed = lowess(speed_km_h, f = 0.2)$y,
    lowess.ele = lowess(GPSAlt, f = 0.2)$y
  )

range(data$timestamp)

#time on the wing
data$activity <- if_else(data$dist_move_km >= 0.5, "flying", "not_flying")
data %>%
  group_by(activity) %>%
  tally()

#add sex and age to individuals:
unique(data$id)
data=as.data.table(data)

data[id %in% c("Bateleur_8887"), sex := "female"]
data[id %in% c("Bateleur_8887"), age := "subadult"]

data[id %in% c("Bateleur_8889"), sex := "female"]
data[id %in% c("Bateleur_8889"), age := "subadult"]

data[id %in% c("White_headed_8888"), sex := "female"]
data[id %in% c("White_headed_8888"), age := "subadult"]

data[id %in% c("WBV_NSR_4205"), sex := "male"]
data[id %in% c("WBV_NSR_4205"), age := "subadult"]

data[id %in% c("WBV_NSR_4245"), sex := "male"]
data[id %in% c("WBV_NSR_4245"), age := "subadult"]

data[id %in% c("WBV_NSR_4247"), sex := "female"]
data[id %in% c("WBV_NSR_4247"), age := "adult"]

data[id %in% c("WBV_NSR_4255"), sex := "male"]
data[id %in% c("WBV_NSR_4255"), age := "adult"]

data[id %in% c("WBV_NSR_4257"), sex := "male"]
data[id %in% c("WBV_NSR_4257"), age := "adult"]

data[id %in% c("WBV_NSR_4260"), sex := "male"]
data[id %in% c("WBV_NSR_4260"), age := "subadult"]

data[id %in% c("WBV_NSR_4261"), sex := "female"]
data[id %in% c("WBV_NSR_4261"), age := "adult"]

data[id %in% c("WBV_NSR_4262"), sex := "male"]
data[id %in% c("WBV_NSR_4262"), age := "subadult"]

data[id %in% c("WBV_NSR_4263"), sex := "female"]
data[id %in% c("WBV_NSR_4263"), age := "subadult"]

data[id %in% c("WBV_NSR_4264"), sex := "male"]
data[id %in% c("WBV_NSR_4264"), age := "adult"]

data[id %in% c("WBV_NSR_4265"), sex := "male"]
data[id %in% c("WBV_NSR_4265"), age := "adult"]

data[id %in% c("WBV_NSR_4266"), sex := "male"]
data[id %in% c("WBV_NSR_4266"), age := "subadult"]

data[id %in% c("WBV_NSR_4267"), sex := "female"]
data[id %in% c("WBV_NSR_4267"), age := "subadult"]

data[id %in% c("WBV_NSR_4268"), sex := "male"]
data[id %in% c("WBV_NSR_4268"), age := "subadult"]

data[id %in% c("WBV_NSR_4269"), sex := "male"]
data[id %in% c("WBV_NSR_4269"), age := "adult"]

data[id %in% c("WBV_NSR_4270"), sex := "female"]
data[id %in% c("WBV_NSR_4270"), age := "subadult"]

data[id %in% c("WBV_NSR_4271"), sex := "male"]
data[id %in% c("WBV_NSR_4271"), age := "adult"]

data[id %in% c("WBV_NSR_4272"), sex := "female"]
data[id %in% c("WBV_NSR_4272"), age := "adult"]

data[id %in% c("WBV_NSR_4273"), sex := "female"]
data[id %in% c("WBV_NSR_4273"), age := "adult"]

data[id %in% c("WBV_NSR_4274"), sex := "male"]
data[id %in% c("WBV_NSR_4274"), age := "adult"]

# Metric summaries --------------------------------------------------------
summary_data <- data %>%
  group_by(id) %>%
  summarise(
    n = length(id),
    min_d = min(dist_move_km),
    max_d = max(dist_move_km),
    ave_d = mean(dist_move_km),
    sd_d = sd(dist_move_km),
    tot_d = sum(na.omit(dist_move_km))
  )
names(data)
# Save and write to RData -------------------------------------------------
save(file = glue("{outdir}/02_NSR_tracking_data_metrics.RData"), list = c("data", "summary_data"))


data$timestamp[1:5]
data$sunrise[1:5]
data$sunset[1:5]
data$dist_move_m[1:5]
range(data$dist_move_m)
range(data$time_diff_mins)
range(data$change_in_ele)
range(data$timestamp)

data %>%
  group_by(id) %>%
  tally()

glimpse(data)

b=ddply(data,c("id"), summarise,
        n=length(id),
        tot_d=sum(na.omit(dist_move_km)))
b


# Bateleur_8887 <- subset(data, data$id %in% c("Bateleur_8887"))
# range(Bateleur_8887$timestamp)
# sum(Bateleur_8887$dist_move_km)
# 
# Bateleur_8889 <- subset(data, data$id %in% c("Bateleur_8889"))
# range(Bateleur_8889$timestamp)
# range(Bateleur_8889$dist_move_km)
# sum(Bateleur_8889$dist_move_km)
# 
# White_headed_8888  <- subset(data, data$id %in% c("White_headed_8888"))
# range(White_headed_8888$timestamp)
# range(White_headed_8888$dist_move_km)
# sum(White_headed_8888$dist_move_km)


## Writing to file loses the SAST time format (reverts to UTC). Note from write_csv() documentation:
## POSIXct values are formatted as ISO8601 with a
## UTC timezone Note: POSIXct objects in local or non-UTC
## timezones will be converted to UTC time before writing.
# 
# write_csv(glue("{outdir}/dataRSA_active_metrics.csv"))

# write_csv(data, glue("data input/dataNSR_active_metrics.csv"))
# nrow(data)

