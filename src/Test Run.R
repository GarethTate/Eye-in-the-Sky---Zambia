
#CREATE QUERY DATE OUTPUT FOLDER -----------------------------------------
##################################################################################################################################
todaysdate <- lubridate::now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue::glue("data output/{todaysdate}")
dir.create(outdir)
##################################################################################################################################

#DEFINE CREDENTIALS-----------------------------------------
##################################################################################################################################
loginStored <- movebankLogin(username="Garethjtate", password="Accipiter@0626")
##################################################################################################################################


#DATA EXTRACTIONS-----------------------------------------
{
##################################################################################################################################
#EXTRACT MPIAB TAG DATA
#getMovebankAnimals(study="EWT_DRUID_RAPTORS", login=loginStored)
# getMovebankAnimals

getMovebankStudy(study="South Africa vultures VfA MPIAB", login=loginStored) # see study-level info

data.move.MPIAB <- getMovebankData(study="South Africa vultures VfA MPIAB", login=loginStored, 
                                   removeDuplicatedTimestamps=T)#, animalName=c("CV_3602"))
## Convert to tibble
raw_data_MPIAB <- as(data.move.MPIAB, "data.frame") %>%
  as_tibble()

# write_csv(raw_data_MPIAB, glue("data output/raw_data_MPIAB.csv"))
# unique(raw_data_MPIAB$ground_speed)
# names(raw_data_MPIAB)

# Clean data
data_MPIAB <- raw_data_MPIAB %>%
  # filter(sensor_type %in% "GPS") %>%
  mutate(GPSAlt = as.numeric(height_above_ellipsoid)) %>%
  mutate(battery = as.numeric(eobs_battery_voltage)) %>%
  # filter(!is.na(GPSAlt)) %>%
  rename(
    id = local_identifier,
    longitude = location_long,
    latitude = location_lat
  ) %>%
  mutate(coords = str_c(latitude, longitude, sep = ", ")) %>%
  rename(GPSCourse = heading, GPSSpeed = ground_speed) %>%
  select(individual_id, id, timestamp, latitude, longitude, GPSAlt, GPSCourse, GPSSpeed, coords, battery)

data_MPIAB$id= gsub("White-headed_8888", "White_headed_8888", data_MPIAB$id)
## Checks
data_MPIAB
glimpse(data_MPIAB)
#
data_MPIAB %>%
  group_by(id) %>%
  tally()

# range(raw_data_MPIAB$timestamp)
# 
# # Check this is correct
# data_MPIAB$timestamp[1:5]
# data_MPIAB$GPSAlt[1:5]
# data_MPIAB$GPSCourse[1:5]
# data_MPIAB$GPSSpeed[1:5]
# range(data_MPIAB$timestamp)
# range(data_MPIAB$GPSAlt)
# range(data_MPIAB$GPSCourse)
# range(data_MPIAB$GPSSpeed)

# raw_data_MPIAB %>%
#   group_by(local_identifier) %>%
#   tally()

#IMPORT DATA FOR SAVANNAH TRACKING UNITS
savannahvultures <- fread("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures/Savannah_Vultures_Active2.csv", sep=",") 
names(savannahvultures)
head(savannahvultures$DATETIME)
savannahvultures$timestamp=savannahvultures$DATETIME
savannahvultures$timestamp<- ymd_hms(savannahvultures$timestamp)
head(savannahvultures$timestamp)

## Convert to tibble
raw_data_SAVANNAH <- as(savannahvultures, "data.frame") %>%
  as_tibble()

# Clean data
data_SAVANNAH <- raw_data_SAVANNAH %>%
  mutate(GPSAlt = as.numeric(ALTITUDE)) %>%
  mutate(battery = as.numeric(BATTERY)) %>%
  # filter(!is.na(GPSAlt)) %>%
  rename(
    id = collarid,
    longitude = X,
    latitude = Y
  ) %>%
  mutate(coords = str_c(latitude, longitude, sep = ", ")) %>%
  rename(GPSCourse = HEADING, GPSSpeed = SPEED) %>%
  select(id, timestamp, latitude, longitude, GPSAlt, GPSCourse, GPSSpeed, coords, battery) 

data_SAVANNAH <- subset(data_SAVANNAH, data_SAVANNAH$id %in% c("IRI2016-4205", "IRI2016-4245" ,"IRI2016-4247", "IRI2016-4255", "IRI2016-4257",
                                                               "IRI2016-4260" ,"IRI2016-4261", "IRI2016-4262","IRI2016-4263", "IRI2016-4264",
                                                               "IRI2016-4265" ,"IRI2016-4266", "IRI2016-4267", "IRI2016-4268" ,"IRI2016-4269",
                                                               "IRI2016-4270","IRI2016-4271", "IRI2016-4272" ,"IRI2016-4273" ,"IRI2016-4274"))#"IRI2016-4273", "IRI2016-4267", "IRI2016-4257", "IRI2016-4266", "IRI2016-4261", "IRI2016-4245", "IRI2016-4265", "IRI2016-4205", "IRI2016-4262", "IRI2016-4260", "IRI2016-4268", "IRI2016-4247", "IRI2016-4271", "IRI2016-4264", "IRI2016-4263","IRI2016-4269", "IRI2016-4255", "IRI2016-4274", "IRI2016-4270"



# ("IRI2016-4205", "IRI2016-4245" ,"IRI2016-4247", "IRI2016-4255", "IRI2016-4257",
# "IRI2016-4260" ,"IRI2016-4261", "IRI2016-4262","IRI2016-4263", "IRI2016-4264",
# "IRI2016-4265" ,"IRI2016-4266", "IRI2016-4267", "IRI2016-4268" ,"IRI2016-4269",
# "IRI2016-4270","IRI2016-4271", "IRI2016-4272" ,"IRI2016-4273" ,"IRI2016-4274")

## Checks
data_SAVANNAH
glimpse(data_SAVANNAH)
#
unique(data_SAVANNAH$id)
data_SAVANNAH$id= gsub("IRI2016-", "WBV_NSR_", data_SAVANNAH$id)

data_SAVANNAH %>%
  group_by(id) %>%
  tally()


# JOIN DATA SOURCES -------------------------------------------------------
##################################################################################################################################
vars_keep <- c("id", "timestamp", "latitude", "longitude",
               "GPSAlt", "GPSSpeed", "GPSCourse")

data <- data_MPIAB %>%
  select(all_of(vars_keep)) %>%
  bind_rows(data_SAVANNAH %>%
              select(all_of(vars_keep)))

## Check
nrow(data_MPIAB) + nrow(data_SAVANNAH) == nrow(data)

data %>%
  group_by(id) %>%
  tally()
unique(data$id)

# Save and write to RData -------------------------------------------------
save(file = glue::glue("{outdir}/01_NSR_tracking_data.RData"),
     list = c("data", "data_MPIAB", "data_SAVANNAH", "outdir"))


# write_csv(data, glue("data input/data_EITS_NSR_active.csv"))
##################################################################################################################################
}

#ADDING METRICS
{ 
  #for panafrican data:
  #data <-subset(data, latitude < 0)
  #correct deployment date
  
  data <-subset(data, timestamp >= "2000-01-01")
  #update timezone to SAST
  local.tz <- "Africa/Johannesburg"
  data$timestamp <- as.POSIXct(format(data$timestamp, tz=local.tz), tz=local.tz)
  data$id<-as.factor(data$id)
  
  data <- data[order(data$id, data$timestamp),]
  
  #adding sunrise sunset data
  data$date <- as.Date(data$timestamp, tz=local.tz)
  data$hour<-hour(data$timestamp)
  data$day<-day(data$timestamp)
  data$month<-month(data$timestamp)
  data$year<-year(data$timestamp)
  data$julian_day<- yday(data$date)
  
  #now to add movement metrics
  data <- data[order(data$id, data$timestamp),] #MAKE SURE THE ORDER IS CORRECT!!!
  
  library(geosphere)
  library(data.table)
  library(dplyr)
  library(data.table)
  library(plotly)
  
  #calculating distnce between consecutive points
  shift.vec <- function (vec, shift) {
    if(length(vec) <= abs(shift)) {
      rep(NA ,length(vec))
    }else{
      if (shift >= 0) {
        c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
      else {
        c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
  
  #convert it to a data_breedingtable
  #creating new data table
  df=data.table(data)
  df[1,dist_move_m:=0]
  coord1 = data.frame(long=data$longitude, lat=data$latitude)
  for(i in 2:length(data$longitude))
  {d=distm(coord1[i,],coord1[i-1,])
  df[i,dist_move_m:=d]}
  data$dist_move_m=df$dist_move_m
  #class(data$dist_move_m)
  data$dist_move_m<-as.numeric(data$dist_move_m)
  data$dist_move_km=data$dist_move_m/1000 #if you want to convert it to kms
  
  #to make next ID start from zero
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$dist_move_km=ifelse(ind,data$dist_move_km, "0")
  
  #to make next ID start from zero
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$dist_move_m=ifelse(ind,data$dist_move_m, "0")
  
  #head(data) 
  
  #adding change in elevation
  data<-as.data.table(data)
  setkey(data, id, timestamp)
  data$GPSAlt<-as.numeric(data$GPSAlt)
  summary(data$GPSAlt)
  
  data[, change_in_ele := GPSAlt - shift(GPSAlt, fill = first(GPSAlt)), by = id]
  
  #calculating time difference between points
  library(tidyverse)
  data <- data %>% 
    mutate(timestamp = ymd_hms(timestamp, tz=local.tz)) %>% 
    arrange(id, timestamp) %>% 
    group_by(id) %>%
    mutate(time_diff_mins = difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "mins"),
           time_diff_secs = as.numeric(time_diff_mins, units = 'secs'))
  
  #to make next ID start from zero
  ind2 <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind2)
  data$time_diff_mins=ifelse(ind2,data$time_diff_mins, "0")
  data$time_diff_secs=ifelse(ind2,data$time_diff_secs, "0")
  
  data<-as.data.frame(data) 
  head(data$timestamp) 
  data[1:10,]
  
  data$time_diff_mins<-as.numeric(data$time_diff_mins)
  data$time_diff_secs<-as.numeric(data$time_diff_secs)
  data$dist_move_km<-as.numeric(data$dist_move_km)
  data$dist_move_m<-as.numeric(data$dist_move_m)
  
  # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
  data$speed_m_s <- data$dist_move_m / data$time_diff_secs
  data$speed_km_h <- data$speed_m_s * 3.6
  data$speed_km_h <- ifelse(is.na(data$speed_km_h), 0, data$speed_km_h)
  data$lowess.speed <- lowess(data$speed_km_h, f = 0.2)$y
  data$lowess.ele <- lowess(data$GPSAlt, f = 0.2)$y
  
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$speed_m_s=ifelse(ind,data$speed_m_s, "0")
  
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$speed_km_h=ifelse(ind,data$speed_km_h, "0")
  
  range(data$speed_km_h)
  library(plyr)
  
  summarydata = ddply(data,c("year","month","julian_day" ,"id"), summarise,
                      n=length(id),
                      min_d=min(dist_move_km),
                      max_d=max(dist_move_km),
                      ave_d=mean(dist_move_km),
                      sd_d=sd(dist_move_km),
                      tot_d=sum(na.omit(dist_move_km)))
  # 
  # bird1 = summarydata[summarydata$id%in% c("CV_296"),]
  # bird2 = summarydata[summarydata$id%in% c("HV_792"),]
  # bird3 = summarydata[summarydata$id%in% c("HV_793"),]
  # bird4 = summarydata[summarydata$id%in% c("LFV_378"),]
  # bird5 = summarydata[summarydata$id%in% c("LFV_504"),]
  # bird6 = summarydata[summarydata$id%in% c("LFV_597"),]
  # bird7 = summarydata[summarydata$id%in% c("TBC_507"),]
  # bird8 = summarydata[summarydata$id%in% c("WBV_003"),]
  # bird9 = summarydata[summarydata$id%in% c("WBV_503"),]
  # bird10 = summarydata[summarydata$id%in% c("WBV_506"),]
  
  # plot(bird1$julian_day, bird1$tot_d)
  # plot(bird2$month, bird2$tot_d)
  # plot(bird3$month, bird3$tot_d)
  # plot(bird4$month, bird4$tot_d)
  # plot(bird5$month, bird5$tot_d)
  # plot(bird6$month, bird6$tot_d)
  # plot(bird7$month, bird7$tot_d)
  # 
  # plot(bird1$month, bird1$max_d)
  # plot(bird2$month, bird2$max_d)
  # plot(bird3$month, bird3$max_d)
  # 
  # plot(bird1$month, bird1$ave_d)
  # plot(bird2$month, bird2$ave_d)
  # plot(bird3$month, bird3$ave_d)
  # 
  # plot(bird1$month, bird1$ave_d)
  # plot(bird2$month, bird2$ave_d)
  # plot(bird3$month, bird3$ave_d)
  
  
  # data$sunrise <- suncalc(d=as.POSIXlt(data$timestamp)$yday,
  #                         Lat= data$latitude,
  #                         Long= data$longitude,
  #                         UTC= F)$sunrise
  # 
  # data$sunset <- suncalc(d=as.POSIXlt(data$timestamp)$yday,
  #                        Lat= data$latitude,
  #                        Long= data$longitude,
  #                        UTC= F)$sunset
  # 
  # data$sunrise <- format(as.POSIXct(data$sunrise*3600, origin="2020-01-01", tz=local.tz), "%H:%M")
  # data$sunset <- format(as.POSIXct(data$sunset*3600, origin="2020-01-01", tz=local.tz), "%H:%M")
  # data$dayNight <- ifelse(data$hour > data$sunrise & data$hour < data$sunset, 'day', 'night')
  
  data$id_all<- "vul"
  #add coordinates column
  data$coords <-paste(data$latitude, data$longitude, sep=", ")
  
  dataBOTS <-data
  #class(last.loc$id)
  
  names(dataBOTS)
  
  # library(Amelia) 
  # missmap(dataBOTS, y.at = F,y.labels = F)
  
  #head(dataBOTS$timestamp)
  
  
  #now add two databases looking at tag status, movement and activity
  
  todaysdate<-now()
  dataBOTS$days_since_seen <- difftime(todaysdate, dataBOTS$timestamp,units="days")
  
  #based on whether device has checked in within last five days?
  tagdead<-ymd_hms(todaysdate)-days(5)
  tagdead
  
  dataBOTS$tag_status <- ifelse(dataBOTS$timestamp >= tagdead, 'online', 'offline')
  
  tag_stat <- dataBOTS %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 
  
  tag_stat<-as.data.frame(tag_stat)
  tag_stat
  
  #now look at last few movements per bird to see whose gets a stationary/mortality alarm
  #dataBOTS
  
  #look at last 42 fixes
  bird_activity <- dataBOTS %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-42)))
  
  
  #then add up total distances for each bird over those 50 days
  bird_activity = ddply(bird_activity,c("id"), summarise,
                        n=length(longitude),
                        min_n=min(dist_move_m),
                        ave_n=mean(dist_move_m),
                        sd_n=sd(dist_move_m),
                        tot=sum(na.omit(dist_move_m)),
                        max_n=max(na.omit(dist_move_m)))
  
  bird_activity$totdistkms<-bird_activity$tot/1000
  
  #birds that havent moved more than 1.5kms are considered dead
  bird_activity$movement <- ifelse(bird_activity$tot >=1500 , 'moving', 'stationary')
  
  names(tag_stat)
  bird_activity<-cbind(bird_activity,tag_stat[3],tag_stat[4],tag_stat[25],tag_stat[26] )
  bird_activity
  
  moving_birds<-subset(bird_activity, bird_activity$movement %in% c("moving")) 
  dead_birds<-subset(bird_activity, bird_activity$movement %in% c("stationary")) 
  
  offline_tags<- subset(bird_activity, bird_activity$tag_status %in% c("offline")) 
  online_tags<- subset(bird_activity, bird_activity$tag_status %in% c("online"))
  
  vlookup<-subset(bird_activity, select=c("id","movement", "days_since_seen", "tag_status"))
  
  dataBOTS2 <- (merge(dataBOTS,vlookup , by = 'id'))
  head(dataBOTS2)
  #dataBOTS
  #unique(dataBOTS2$movement)
  
  dataBOTS<-dataBOTS2
  #range(dataBOTS$timestamp)
  # paste("The dataset contains: " , nrow(dataBOTS2), " locations",sep="")
  # paste("The dataset contains: ", length(unique(dataBOTS2$id)), " individual/s",sep="")
  # 
  # paste("The dataset contains: " , nrow(dataBOTS), " locations",sep="")
  # paste("The dataset contains: ", length(unique(dataBOTS$id)), " individual/s",sep="")
  
  
  #View(dataBOTS)
  #setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures/Raptors Bots")
  # setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures/EYE_IN_THE_SKY")
  # write.csv(dataBOTS, file="dataBOTS_active.csv")
  
}

#MAP FOR ALL BIRDS
{
  data<-dataBOTS
  
  library(leafem)  
  library(htmltools)
  
  
  obs_days=31
  
  todaysdate<-now()
  oneweek<-ymd_hms(todaysdate)-days(obs_days)
  oneweek
  
  icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
  icon.fa2 <- makeAwesomeIcon(icon= 'piggy-bank', markerColor = 'white', iconColor = 'red') #thumbs-down, #star, #star-empty #record #piggy-bank
  icon_bird_down <- makeAwesomeIcon(icon= 'star', markerColor = 'white', iconColor = 'red') #thumbs-down, #star, #star-empty #record #piggy-bank
  
  data$id<-as.factor(data$id)  
  
  
  todaysdate<-now()
  oneweek<-ymd_hms(todaysdate)-days(obs_days)
  oneweek
  
  last.loc <-subset(data, timestamp >= oneweek)  
  
  ddply(last.loc, c("id"), summarise,n=length(id))
  
  #pull out summary data for the last week of tracking data
  last.loc$speed_km_h<-as.numeric(last.loc$speed_km_h)
  #range(last.loc$GPSSpeed)
  
  summarydata = ddply(last.loc,c("id"), summarise,
                      n=length(longitude),
                      min_n=min(dist_move_m),
                      ave_n=mean(dist_move_m),
                      ave_speed=mean(speed_km_h),
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
  
  pal <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "wheat3","forestgreen"),
                     domain = unique(last.loc$id))
  
  
  #FOR TIMELINE SLIDE
  power<-last.loc
  power$start<-power$timestamp 
  #power$end<-power$timestamp+3600 
  power[1:10,]#first 10 rows
  
  power <- as_tibble(power) %>% 
    mutate(start = timestamp) %>% 
    mutate(end = lead(timestamp))
  
  power[1:10,]#
  #power[,14:15]#
  summary(power$end)
  power_geo <- geojsonio::geojson_json(power,lat="latitude",lon="longitude")
  
  birdmap <- leaflet(last.loc) %>% 
    addMouseCoordinates()%>%
    #setView(lng = 18, lat = -32, zoom = 8) %>%
    addTiles(group = "OSM (default)") %>% #Esri.WorldImagery
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
    #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addProviderTiles(providers$GeoportailFrance.orthos, group = "Satellite")%>%
    addProviderTiles(providers$HERE.hybridDay, group = "Satellite Day")%>% #HERE.satelliteDay 
    addProviderTiles(providers$Esri.DeLorme, group = "Esri.DeLorme")%>% #Esri.WorldShadedRelief
    addProviderTiles(providers$Esri.WorldShadedRelief, group = "Esri.WorldShadedRelief")%>% #
    #addCircles(~longitude, ~latitude, color = ~pal(id))%>%
    #addAwesomeMarkers(lng=19.195508, lat=-30.807727, popup="Klaas se plaashuis",icon = icon.ion, labelOptions = labelOptions(noHide = F), label="Klaas se plaashuis")%>%
    addCircleMarkers(data=offline_tags, color = "red" ,radius=10, opacity = 1, fillOpacity = 0.2, ~longitude, ~latitude,  label=~paste0(id, ":",longitude,", ", latitude ), popup = ~paste0("days since last seen:",days_since_seen), group="Tag offline alert")%>%
    #addCircleMarkers(data=offline_tags, color = "red" ,radius=10, opacity = 1, fillOpacity = 0.2, ~longitude, ~latitude,  label=~paste0(longitude,", ", latitude ), popup = ~as.character(days_since_seen), group="Tag offline alert")%>%
    addAwesomeMarkers(data=dead_birds, icon=icon_bird_down, ~longitude, ~latitude,  label = ~paste0(longitude,", ", latitude ), popup = ~paste0("Distance traveled over last 42 fixes:", totdistkms), group="Nesting/Mortality Alert")%>%  
    #addCircleMarkers(data=lastseven, radius=2 ,color = ~pal(id), stroke = FALSE, fillOpacity = 1,  ~longitude, ~latitude, label=~paste0(id, ": Ascending (+) or Descending (-):  ", formatC(change_in_ele, big.mark = ",")), popup=~paste0("Date:", timestamp, "Coordinates:", coords), group="GPS Fixes")%>% 
    #addAwesomeMarkers(data=SFS, icon=icon.fa2, ~longitude, ~latitude,  label = ~Restaurant_Name, popup = ~as.character(RESPONSIBILITY), group="Feeding Site")%>%
    #addLabelOnlyMarkers(data=SFS,   ~longitude, ~latitude, label = ~Restaurant_Name,labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "top", textsize = "12px", textOnly = TRUE,offset = c(0, -25),  opacity = 1, group="Feeding Site"))%>%
    addLegend("topleft", pal = pal, values = ~id, title = "Individual id", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
    addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
               activeColor = "#3D535D",
               completedColor = "#7D4479")%>% setView(0,0,3)%>%
    addTimeline(data = power_geo,timelineOpts = timelineOptions(styleOptions = styleOptions(radius = 6,color = "red",fillColor = "black",
                                                                                            fillOpacity = 1)))%>% 
    addMarkers(data=verylast, ~longitude, ~latitude, label=~id, group="Last location", popup = paste(
      # HTML CODE TO FORMAT TABLE ####
      '<b>',
      toupper(verylast$id),
      '</b>',
      '<table>',
      
      '<tr><td>Last seen</td><td><b>',
      toupper(verylast$timestamp),
      '</b></td></tr>',
      
      '<tr><td>Days since last seen</td><td><b>',
      toupper(verylast$days_since_seen.x),
      '</b></td></tr>',
      
      '<tr><td>Tag Status</td><td><b>',
      toupper(verylast$tag_status.x),
      '</b></td></tr>',
      
      '<tr><td>Bird Movement</td><td><b>',
      toupper(verylast$movement),
      '</b></td></tr>',
      
      '<tr><td>Last position</td><td><b>',
      toupper(verylast$coords),
      '</b></td></tr>',
      
      '<tr><td>Total kms traveled this week</td><td>',
      verylast$totdistkms,
      '</td></tr>',
      
      '<tr><td>Average speed km/h</td><td>',
      verylast$ave_speed,
      '</td></tr>',
      
      '<tr><td>Max speed recorded km/h</td><td>',
      verylast$max_speed,
      '</td></tr>',
      
      # '<tr><td>Tag Voltage</td><td>',
      # verylast$battery,
      # '</td></tr>',
      
      '</table>'))%>%
    addLabelOnlyMarkers(data=verylast, ~longitude, ~latitude, label = as.character(verylast$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1))%>%
    addCircleMarkers(data=last.loc, color = ~pal(id), group=~as.character(id), radius=5,stroke = FALSE, label=~paste0(id,": ",  timestamp), popup=~coords, fillOpacity = 1,  ~longitude, ~latitude)#%>%
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: normal;
    font-size: 20px;
  }
"))
  
  threedays<-now()-days(obs_days)
  threedays<-format(threedays, format="%d %B")
  today<-now()
  today<-format(today, format="%d %B %Y")
  
  maptitle<-paste("Vulture movements ",threedays," - ", today ,sep="")
  maptitle  
  
  title <- tags$div(
    tag.map.title, HTML(maptitle)
  )
  
  for(id in levels(last.loc$id)){
    birdmap = addPolylines(birdmap, 
                           lng= ~ longitude,
                           lat= ~ latitude,
                           data = last.loc[last.loc$id==id,], 
                           color= ~ pal(id),
                           weight = 0.05,opacity = 0.7, popup=id, smoothFactor = 5, group=~id)
  }
  
  quickbirdmap<- birdmap %>%
    # setView(22.65073,-28, zoom = 5) %>% 
    addEasyButton(
      easyButton(
        icon = shiny::icon("home"),
        title= "Reset Zoom",
        onClick = JS(
          c("function(btn, map) {map.setView(new L.LatLng(-29.727017, 22.988433), 7);}"))))%>%
    addLayersControl(
      baseGroups = c("OSM (default)", "Satellite", "Satellite Day", "ESRI Satellite", "Esri.DeLorme", "Esri.WorldShadedRelief"),
      overlayGroups = c("Last location" , "Feeding Site","Tag offline alert", "Nesting/Mortality Alert", c(unique(as.character(last.loc$id)))),
      options = layersControlOptions(collapsed = T))   %>%
    addControl(title, position = "topleft", className="map-title")  %>%
    addMiniMap(tiles = providers$OSM, toggleDisplay = TRUE,
               position = "bottomright") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  
  quickbirdmap 
  
}

#save map, table and movement info
{  
  today<-now()
  todaysdate<-now()
  todaysdate<-format(todaysdate, format="%d %B %Y")
  todaysdate
  
  observation_days<-31
  
  oneweek<-now()-days(observation_days)
  oneweek<-format(oneweek, format="%d %B")
  oneweek
  
  #setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures/EYE_IN_THE_SKY/Botswana_Reports")
  
  
m1 <-   dataBOTS %>% 
    group_by(id) %>% 
    filter(row_number()==1)
  
  
m1<-as.data.frame(m1)
  
glimpse(m1)
  
  #range(m1$timestamp)  
  
m1$Date_range=range(m1$timestamp)
  
m1 <- m1[order(m1$id),]
m1
  
m2 <- dataBOTS %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 
  
m2<-as.data.frame(m2)
  m2$last_seen<-m2$timestamp
  glimpse(m2)
  m2<-subset(m2, select=c("id", "last_seen"))
  
  m2 <- m2[order(m2$id),]
  m2
  m1
  
  
  m1<-cbind(m1,m2[2])
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
  
  
  fixes<-ddply(dataBOTS, c("id"), summarise,n=length(id))
  
  m1<-cbind(m1,fixes[2])
  m1
  
  m1 <- m1[order(m1$timestamp),]
  m1
  m1$Date_tagged<-m1$timestamp
  m1$Number_fixes<-m1$n
  
  names(m1) 
  m1$Device_status <- ifelse(m1$last_seen >= oneweek, 'online', 'offline')
  head(m1)
  
  m1<-subset(m1, select=c("id","Date_tagged", "last_seen","Device_status",  "daystagged","weeks_tagged", "Number_fixes" ))
  head(m1)
  
  today<-now()
  todaysdate<-now()
  todaysdate<-format(todaysdate, format="%d %B %Y")
  todaysdate
  
  title<-paste("NSR Vulture GPS Tagging Table_", todaysdate, ".csv", sep="")
  title
  
  # title<-"Martial GPS Tagging Table_Active.csv"
  # title
  
#setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures/EYE_IN_THE_SKY/Botswana_Reports")
#write.csv(m1, file=title)
  
  library(formattable) #https://www.rdocumentation.org/packages/gt/versions/0.2.0.5
  library(gt) #https://gt.rstudio.com/articles/intro-creating-gt-tables.html
  library(tidyverse)
  library(glue)
  library(paletteer)#https://gt.rstudio.com/reference/data_color.html
  
  tbl <- gt(data = m1)
  tbl
  
  # Make a display table with the `islands_tbl`
  # table; put a heading just above the column labels
  tbl <-
    tbl %>%
    tab_header(
      title = md("Eye in the Sky, Niassa Special Reserve"),
      subtitle = "GPS Tracking Summary"
    )%>%
    cols_align(
      align = "left",
    )
  
  # Show the gt Table
  tbl
  
  # Display the `islands_tbl` data with a heading and
  # two source notes
  tbl <-
    tbl %>%
    tab_source_note(
      source_note = md("****") #use stars to make bold text
    )
  
  # Show the gt Table
  tbl
  
  tbl <-
    tbl %>%
    tab_options(
      data_row.padding = px(0.005)
    )
  
  tbl
  
  # Modify the table width to 100% (which
  # spans the entire content width area)
  tbl <-
    tbl %>%
    tab_options(
      table.width = pct(100)
    )
  
  tbl
  
  
  # Modify the table's background color
  # to be "lightcyan"
  tbl <-
    tbl %>%
    tab_options(
      table.background.color = "lightgrey"
    )
  
  tbl <-
    tbl %>%
    tab_options(heading.background.color="green")
  
  tbl
  
  tbl <-
    tbl %>%
    tab_options(
      table.font.size = "small"
    )
  
  tbl
  
  tbl <-
    tbl%>%
    cols_label(
      id = md("Vulture Name"),
      # Sex = md("Sex "),
      # Age = md("Age Class"),
      #max_speed = md("Max Speed recorded (km/h)"),
      #totdistkms = md("Total distance traveled over month")
    )
  
  tbl 
  
  
#second stats table
  
  
t_data<-subset(verylast, select=c("id", "max_speed" ,"totdistkms"))
  
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
      max_speed = md("Max Speed recorded (km/h)"),
      totdistkms = md("Total kms traveled over reporting period")
    )
  
  gt_tbl2    
  
  
  
  
}


