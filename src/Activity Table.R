# # # Libraries
# library(htmlwidgets)
# library(geojsonio)
# library(geojsonlint)
# library(knitr)
# library(maptools)
# library(amt) 
# library(tibble)
# library(SDLfilter)
# library(suncalc)
# library(RAtmosphere)
# library(move)
# library(tidyverse)
# library(lubridate)
# library(geosphere)
# library(glue)
# library(maps)
# library(mapdata)
# library(raster)
# library(sp)
# library(rgdal)
# library(geosphere)
# library(dismo)
# library(rgeos)
# library(sf)
# library(recurse)
# library(plotly)
# library(data.table)
# library(leaftime)
# library(httr)
# library(jsonlite)
# library(leaflet)
# library(leafem)
# library(htmltools)
# library(leafpop)
# library(leaflet.extras)
# install.packages("paletteer")
library(formattable) #https://www.rdocumentation.org/packages/gt/versions/0.2.0.5
library(gt) #https://gt.rstudio.com/articles/intro-creating-gt-tables.html
library(paletteer)#https://gt.rstudio.com/reference/data_color.html

# Check
# conflicted::conflict_scout()

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

# NOTE data object contains all GPS points of all birds
load(glue("{outdir}/02_NSR_tracking_data_metrics.RData"))

dataVUL <- data

# Check tag status --------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
dataVUL$days_since_seen <- difftime(todaysdate, dataVUL$timestamp, units = "days")

## Check whether the tag is online or offline based on timestamp within the last 5 days
tagdead <- ymd_hms(todaysdate) - days(5)
dataVUL$tag_status <- ifelse(dataVUL$timestamp >= tagdead, "online", "offline")
names(dataVUL)

## Check the tag status of the most recent fix for each bird
tag_status <- dataVUL %>%
  group_by(id) %>%
  group_split() %>%
  map_dfr(~ filter(., row_number() %in% nrow(.))) %>%
  select(id, sex, age, timestamp, days_since_seen, tag_status, latitude, longitude) %>%
  write_csv(glue("{outdir}/01_tag_status_{format(todaysdate, '%Y-%m-%d')}.csv"))

# Stationary/mortality alarm ----------------------------------------------

## Extract the last 42 fixes for each individual
bird_activity <- dataVUL %>%
  group_by(id) %>%
  group_split() %>%
  map_dfr(~ filter(., row_number() %in% nrow(.):(nrow(.) - 42))) #use 42, as this reflects roughly five days

bird_activity <- bird_activity %>%
  group_by(id) %>%
  summarise(
    n = length(id),
    min_n = min(dist_move_m),
    ave_n = mean(dist_move_m),
    sd_n = sd(dist_move_m),
    tot_n = sum(na.omit(dist_move_m)),
    max_n = max(na.omit(dist_move_m))
  ) %>%
  mutate(tot_n_km = tot_n/1000)

## Birds that haven't moved more than 1.5kms are considered dead
bird_activity$movement <- if_else(bird_activity$tot_n_km >= 1.5, "moving", "stationary")

## Join tag status to activity data
activity_status <- bird_activity %>%
  left_join(tag_status %>%
              select(id, sex, age, latitude, longitude, days_since_seen, tag_status),
            by = "id")

## Write reference file
activity_status %>%
  write_csv(glue("{outdir}/02_bird_activity_status_{format(todaysdate, '%Y-%m-%d')}.csv"))

moving_birds <- activity_status %>%
  filter(movement %in% c("moving"))

dead_birds <- activity_status %>%
  filter(movement %in% c("stationary"))

offline_tags <- activity_status %>%
  filter(tag_status %in% c("offline"))

online_tags <- activity_status %>%
  filter(tag_status %in% c("online"))

# Combine activity and fix data -------------------------------------------

dataVUL <- dataVUL %>%
  left_join(activity_status %>%
              select(id, sex, age, movement, days_since_seen, tag_status),
            by = "id"
  )


GPSdataVUL=as_tibble(dataVUL)
dataVUL=as_tibble(dataVUL)

GPSdataVUL <- GPSdataVUL[order(GPSdataVUL$timestamp, GPSdataVUL$id),]

## Write reference file
# GPSdataVUL %>%
#   write_csv(glue("{outdir}/03_Vul_GPS_Tracking_data_active_{format(todaysdate, '%Y-%m')}.csv"))


#create table for tracking summary for the month

m1 <-
  dataVUL %>% 
  group_by(id) %>% 
  filter(row_number()==1)

m1<-as.data.frame(m1)
#range(m1$battery)
range(m1$timestamp) 
glimpse(m1)

# m1$Date_range<-range(m1$timestamp)

m1 <- m1[order(m1$id),]
m1

m2 <- dataVUL %>% 
  group_split(id) %>% 
  map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 

m2<-as.data.frame(m2)
m2$last_seen<-m2$timestamp
#m2$last_tag_voltage<-m2$battery
m2<-subset(m2, select=c("id", "last_seen"))
m2

m2 <- m2[order(m2$id),]

m1<-cbind(m1,m2[1:2])
m1

#specify last week of GPS fixes
todaysdate<-now()
oneweek<-ymd_hms(todaysdate)-days(3)
oneweek

#adding days monitored:
yearago=as.POSIXct(paste(Sys.Date()-365, "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
maxdate=as.POSIXct(paste(Sys.Date(), "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")

m1$todaysdate=as.POSIXct(paste(Sys.Date(), "00:00:01"), format="%Y-%m-%d %H:%M:%S")
m1

m1$daystagged<-round(as.POSIXct(paste(Sys.Date(), "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")-as.POSIXct(paste(m1$timestamp), format="%Y-%m-%d %H:%M:%S", tz="UTC"),0)
m1

# weeks
m1$weeks_tagged = as.numeric(difftime(m1$todaysdate, m1$timestamp, units = "weeks"))
m1

library(plyr)
library(gt)

fixes<-ddply(data, c("id"), summarise,n=length(id))

m1<-cbind(m1,fixes[2])
m1

m1 <- m1[order(m1$timestamp),]
m1


m1$Date_tagged<-m1$timestamp
m1$Number_fixes<-m1$n

names(m1) 
m1$Device_status <- ifelse(m1$last_seen >= oneweek, 'online', 'offline')
head(m1)

m1<-subset(m1, select=c("id","sex.x","age.x", "Date_tagged", "last_seen","Device_status",  "daystagged","weeks_tagged", "Number_fixes"))
head(m1)

summarydata_1 = ddply(dataVUL,c("id"), summarise,
                    n=length(longitude),
                    min_n=min(dist_move_m),
                    ave_n=mean(dist_move_m),
                    ave_speed=mean(GPSSpeed),
                    max_speed=max(GPSSpeed),
                    sd_n=sd(dist_move_m),
                    tot=sum(na.omit(dist_move_m)),
                    max_n=max(na.omit(dist_move_km)))

summarydata_1$totdistkms<-summarydata_1$tot/1000

range(summarydata_1$max_n)


summarydata2 = dataVUL%>%
  filter(activity == "flying")

range(summarydata2$time_diff_mins)

summarydata2 =
  ddply(summarydata2,c("id"), summarise,
                      n=length(id),
                      total_mins_flying=sum(na.omit(time_diff_mins)),
                      ave_mins_flying=mean(na.omit(time_diff_mins)))
summarydata2
unique(dataVUL$activity)
unique(summarydata2$activity)

summarydata2$total_hours_flying<-summarydata2$total_mins_flying/60
summarydata2$max_hours_flying<-summarydata2$ave_mins_flying/60

summarydata2

# summarydata3 = dataVUL%>%
#   filter(activity == "flying")
# 
# summarydata_3 = ddply(summarydata3,c("id", "day"), summarise,
#                       n=length(id),
#                       total_mins_flying=sum(na.omit(time_diff_mins)),
#                       ave_mins_flying=max(na.omit(time_diff_mins)))
# 
# summarydata_3

m1 <- m1 %>%
  left_join(summarydata_1 %>%
              select(id, totdistkms, max_n),
            by = "id"
  )


m1 <- m1 %>%
  left_join(summarydata2 %>%
              select(id,total_hours_flying),
            by = "id"
  )

class(m1$daystagged)
m1$daystagged<-as.numeric(m1$daystagged)

m1$ave_daily_kms = m1$totdistkms/m1$daystagged

m1$ave_daily_hrs_flying = m1$total_hours_flying/m1$daystagged
# m1$ave_hrs_daily_flying = m1$ave_mins_daily_flying/60

m1 <- m1 %>%
      select(id, sex.x, age.x, Date_tagged, last_seen, Device_status, daystagged,	weeks_tagged,	Number_fixes,	totdistkms,	ave_daily_kms, total_hours_flying	,	ave_daily_hrs_flying)

# m1$sex=m1$sex.x
# m1$age=m1$age.x

#rename columns 
colnames(m1)[which(names(m1) == "sex.x")] <- "sex"
colnames(m1)[which(names(m1) == "age.x")] <- "age"
names(m1)

today1<-now()
todaysdate<-now()
todaysdate<-format(todaysdate, format="%d %B %Y")
todaysdate

title<-paste("Niassa EITS Vulture GPS Tagging Table_", todaysdate, ".csv", sep="")
title


tbl <- gt(data = m1)
tbl

# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
tbl <-
  tbl %>%
  tab_header(
    title = md("Eye in the Sky Project, Niassa"),
    subtitle = paste("Vulture GPS Tracking Summary", todaysdate)
  )%>%
  cols_align(
    align = "left",
  )

# Display the `islands_tbl` data with a heading and
# two source notes
tbl <-
  tbl %>%
  tab_source_note(
    source_note = md("****") #use stars to make bold text
  )

tbl <-
  tbl %>%
  tab_options(
    data_row.padding = px(0.005)
  )

# Modify the table width to 100% (which
# spans the entire content width area)
tbl <-
  tbl %>%
  tab_options(
    table.width = pct(100)
  )

# Modify the table's background color
# to be "lightcyan"
tbl <-
  tbl %>%
  tab_options(
    table.background.color = "darkolivegreen4"
  )

tbl <-
  tbl %>%
  tab_options(heading.background.color="cornsilk")

tbl <-
  tbl %>%
  tab_options(
    table.font.size = "medium"
  )

tbl <-
  tbl%>%
  cols_label(
    id = md("Vulture ID"),
    #max_speed = md("Max Speed recorded (km/h)"),
    #totdistkms = md("Total distance traveled over month")
  )

tbl <-
  tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>%
  fmt_number( # Another column (also numeric data)
    columns = ave_daily_kms, # What column variable? BOD$demand
    decimals = 0
  )

tbl <-
  tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>%
  fmt_number( # Another column (also numeric data)
    columns = totdistkms, # What column variable? BOD$demand
    decimals = 0
  )



# class(m1$last_tag_voltage)
# m1$nest_location_x<-as.numeric(m1$nest_location_x)
# m1$nest_location_y<-as.numeric(m1$nest_location_y)

tbl <-
  tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = weeks_tagged, # What column variable? BOD$demand
    decimals = 1
  )

tbl <-
  tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>%
  fmt_number( # Another column (also numeric data)
    columns = total_hours_flying, # What column variable? BOD$demand
    decimals = 1
  )

tbl <-
  tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>%
  fmt_number( # Another column (also numeric data)
    columns = ave_daily_hrs_flying, # What column variable? BOD$demand
    decimals =2
  )
#%>% 
# fmt_number( # Another column (also numeric data)
#   columns = nest_location_x, # What column variable? BOD$demand
#   decimals = 2 
# )%>% 
#   fmt_number( # Another column (also numeric data)
#     columns = nest_location_y, # What column variable? BOD$demand
#     decimals = 2
#   )

tbl 

tbl <-
  tbl%>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = Device_status,
      rows = Device_status == "offline"
    )
  ) %>% 
  tab_style(
    style = cell_text(color = "green", weight = "bold"),
    locations = cells_body(
      columns = Device_status,
      rows = Device_status == "online"
    )
  )
tbl 


tbl <-tbl%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = totdistkms, # What column variable? BOD$demand
    decimals = 0
  )

tbl <-
  tbl%>%
  cols_label(
    totdistkms = md("Cumulative distance traveled (km)"),
    total_hours_flying = md("Total Hours in flight"),
    ave_daily_hrs_flying = md("Average daily Hours in flight"),
    #max_n = md("Maximum daily distance recorded (km)"),
    ave_daily_kms = md("Average daily distance (km)")
  )


tbl  


title<- paste("Vulture GPS Tracking Summary Table", " ", todaysdate, ".png", sep="")
title

tbl %>%
  gtsave(
    title, expand = 1,vwidth = 1400, vheight = 1000,
    path = {outdir}
  )

todaysdate<-now()
observation_days<-31
oneweek<-ymd_hms(todaysdate)-days(observation_days)
oneweek

last.loc <-subset(dataVUL, timestamp >= oneweek)  
ddply(last.loc, c("id"), summarise,n=length(id))

#pull out summary data for the last week of tracking data
last.loc$GPSSpeed<-as.numeric(last.loc$GPSSpeed)
#range(last.loc$GPSSpeed)

summarydata = ddply(last.loc,c("id"), summarise,
                    n=length(longitude),
                    min_n=min(dist_move_m),
                    ave_n=mean(dist_move_m),
                    ave_speed=mean(GPSSpeed),
                    max_speed=max(GPSSpeed),
                    sd_n=sd(dist_move_m),
                    tot=sum(na.omit(dist_move_m)),
                    max_n=max(na.omit(dist_move_m)))

summarydata$totdistkms<-summarydata$tot/1000

#pull out last points for each eagle:
verylast <- last.loc %>% 
  group_split(id) %>% 
  map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 

# now combine summarydata to verylast
verylast<-cbind(verylast, summarydata)

threedays<-now()-days(observation_days)
threedays<-format(threedays, format="%d %B")
today<-now()
today<-format(today, format="%d %B %Y")

maptitle<-paste("Vulture movements ",threedays," - ", today ,sep="")
maptitle  
names(verylast)
t_data<-subset(verylast, select=c("id" ,"totdistkms"))

gt_tbl2 <- gt(data = t_data)
gt_tbl2

# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
gt_tbl2 <-
  gt_tbl2 %>%
  tab_header(
    title = md(maptitle),
    subtitle = "Eye in the Sky Project, Niassa"
  )%>%
  cols_align(
    align = "left",
  )

# Show the gt Table
gt_tbl2

# Display the `islands_tbl` data with a heading and
# two source notes
gt_tbl2 <-
  gt_tbl2 %>%
  tab_source_note(
    source_note = md("****") #use stars to make bold text
  )

# Show the gt Table
gt_tbl2

gt_tbl2 <-
  gt_tbl2 %>%
  tab_options(
    data_row.padding = px(0.005)
  )

gt_tbl2

# Modify the table width to 100% (which
#   # spans the entire content width area)
#   gt_tbl2 <-
#     gt_tbl2 %>%
#     tab_options(
#       table.width = pct(50)
#     )
#   
# gt_tbl2
#   

# Modify the table's background color
# to be "lightcyan"
gt_tbl2 <-
  gt_tbl2 %>%
  tab_options(
    table.background.color = "darkolivegreen4"
  )

gt_tbl2 <-
  gt_tbl2 %>%
  tab_options(heading.background.color="cornsilk")

gt_tbl2

gt_tbl2 <-
  gt_tbl2 %>%
  tab_options(
    table.font.size = "large"
  )

gt_tbl2


gt_tbl2 <-gt_tbl2%>%
  # fmt_number( # A column (numeric data)
  #   columns = last_tag_voltage, # What column variable? BOD$Time
  #   decimals = 1
  # ) %>% 
  fmt_number( # Another column (also numeric data)
    columns = totdistkms, # What column variable? BOD$demand
    decimals = 1
  )

gt_tbl2 <-
  gt_tbl2%>%
  cols_label(
    id = md("Vulture Name"),
    #max_speed = md("Max Speed recorded (km/h)"),
    totdistkms = md("Total kms traveled over reporting period")
  )

gt_tbl2  



gt_tbl2 %>%
  gtsave(
    paste(maptitle,".png"), expand = 1,
    path = {outdir}
  )


# Save and write to RData -------------------------------------------------
save(
  file = glue("{outdir}/NSR_EITS_tracking_data_activity.RData"),
  list = c(
    "data", "dataVUL", "summary_data", "activity_status","dead_birds",
    "offline_tags",
    "online_tags", "verylast", "last.loc", "summarydata","bird_activity"
  )
)

