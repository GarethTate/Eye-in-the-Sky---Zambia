#clear all loaded packages
{
  #pulling out clusters bigger than 10 points
  # 
  detachAllPackages <- function() {
    basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
    package.list <- setdiff(package.list,basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  }
  
  detachAllPackages()
  
}

#Packages
{
  # #library(adehabitatLT)                                                    
  # #library(adehabitatHR)
  # library(raster)
  # library(rgdal)
  # library(maptools)
  # library(maps)
  # library(mapdata)
  # #library(scales)
  # library(RgoogleMaps)
  # library(ggplot2)
  # library(GISTools)
  # #library(SDMTools)
  # library(mapproj)
  # library(ggmap)
  # library(ggmap2)
  # library(rgeos)
  # #library(grid)
  #update.packages(oldPkgs="tidyverse", repos="http://R-Forge.R-project.org")
  # library(ctmmweb)
  # library(ctmm)
  # library(data.table)
  # library(lubridate)
  # library(sp)
  # library(tidyverse)
  # library(sf)
  # library(plyr)
  #install.packages("rlang")
  #install.packages("tidyverse")
  # #devtools::install_github("Nowosad/spDataLarge")
  
  library(htmlwidgets)
  library(htmltools)
  library(leaflet)
  library(geojsonio)
  library(tidyverse)
  library(geojsonlint)
  
  library(knitr)
  library(lubridate)
  library(maptools)
  library(raster)
  library(move)
  library(amt) 
  #install.packages("suncalc")
  #library(ggmap)
  library(tibble)
  library(leaflet)
  library(plyr)
  library(data.table)
  library(SDLfilter)
  library(tidyverse)
  library(leaftime)
  #library(suncalc)
  library(RAtmosphere)
  library(leaflet.extras)
  library(leafpop)
  # library(animove)
  
}

rm(list=ls())
####PREPPING DATA####

#reading in data
{
  # todaysdate<-now()
  # # startdate<-ymd_hms(todaysdate)-days(90)
  # # startdate  
  # 
  # loginStored <- movebankLogin(username="Garethjtate", password="Accipiter@0626")
  # #getMovebankAnimals(study="EWT_DRUID_RAPTORS", login=loginStored)
  # # getMovebankAnimals
  # 
  # getMovebankStudy(study="South Africa vultures VfA MPIAB", login=loginStored) # see study-level info
  # 
  # data.move <- getMovebankData(study="South Africa vultures VfA MPIAB", login=loginStored, removeDuplicatedTimestamps=T)#, animalName=c("CV_3602"))
  # # names(data.move)
  # # unique(data.move$tag_local_identifier)
  # range(data.move$timestamp)
  # 
  # data <- as(data.move, "data.frame")
  # 
  # # names(data)
  # 
  # range(data$timestamp)
  # class(data$timestamp)
  # data$id<-data$local_identifier
  # #data$id= gsub("CV_3802.1", "CV_3802", data$id)
  # ddply(data, c("id"), summarise,n=length(id))    
  # 
  # data$longitude <-data$location_long
  # data$latitude <-data$location_lat
  # data$GPSAlt <-data$height_above_ellipsoid
  # 
  # data$GPSAlt=as.numeric(data$GPSAlt)
  # 
  # names(data)
  # 
  # #add coordinates column
  # data$coords <-paste(data$latitude, data$longitude, sep=", ")
  # data$battery<-data$eobs_battery_voltage
  # names(data)
  # # data$coords= gsub("-", "a: -", data$coords)
  # 
  # data <- subset(data, select=c("individual_id", "id", "timestamp", "latitude", "longitude","GPSAlt" , "coords", "ground_speed", "heading", "battery"))
  # data
  # data <- data[order(data$id,data$timestamp),]
  # data$GPSSpeed <-data$ground_speed
  # data$GPSCourse <-data$heading
  # 
  # 
  # palcv <- colorFactor(c("yellow", "green"),
  #                      domain = unique(data$id))
  # 
  # #quick visulaistion
  # # leaflet(data) %>%
  # #       addTiles(group = "OSM (default)") %>% #Esri.WorldImagery
  # #       addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
  # #       addProviderTiles(providers$GeoportailFrance.orthos, group = "Satellite")%>%
  # #       addProviderTiles(providers$HERE.hybridDay, group = "Satellite Day")%>% #HERE.satelliteDay
  # #       addProviderTiles(providers$Esri.DeLorme, group = "Esri.DeLorme")%>% #Esri.WorldShadedRelief
  # #       addProviderTiles(providers$Esri.WorldShadedRelief, group = "Esri.WorldShadedRelief")%>% #
  # #       addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
  # #       addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
  # #                  activeColor = "#3D535D",
  # #                  completedColor = "#7D4479")%>% setView(0,0,3)%>%
  # #       addCircleMarkers(data=data, radius=0.5, color = palcv(id), stroke = FALSE, fillOpacity = 0.5,  ~longitude, ~latitude)%>%
  # #       addLayersControl(
  # #         baseGroups = c("OSM (default)", "Satellite", "Satellite Day", "ESRI Satellite", "Esri.DeLorme", "Esri.WorldShadedRelief"),
  # #         overlayGroups = c(unique(data$id)),
  # #         options = layersControlOptions(collapsed = T))   %>%
  # #       addControl(title, position = "topleft", className="map-title")  %>%
  # #       addMiniMap(tiles = providers$OSM, toggleDisplay = TRUE,
  # #                  position = "bottomright") %>%
  # #       htmlwidgets::onRender("
  # #     function(el, x) {
  # #       var myMap = this;
  # #       myMap.on('baselayerchange',
  # #         function (e) {
  # #           myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
  # #         })
  # #     }")
  # 
  # 
  # # 
  # # df <- as_tibble(data) %>% 
  # #   mutate(hour = hour(timestamp)) %>% 
  # #   mutate(date = ymd(str_c(year(timestamp),month(timestamp),day(timestamp), sep = "-"))) %>% 
  # #   group_by(id, date ,hour) %>% 
  # #   filter(row_number() == 1)
  # # df[1:10,]
  # 
  # #export dataset with all fixes
  # # setwd("C:/Users/GarethT/OneDrive - The EWT/BOPP/DATA/GPS/Eagles")
  # # write.csv(df, file="WE_Active_all_hourly.csv")
  # 
  # # data<-df
  # 
  # data_druid<-data
  # 
  # #ddply(data_druid, c("id"), summarise,n=length(id))
  # # range(data_druid_CV$timestamp)
  
} 

#ADDING METRICS TO DATA
{
  ddply(data, c("id"), summarise,n=length(id))
  class(data)
  data<-as.data.table(data)
  
  data$id<-as.factor(data$id)
  
  #removing duplicate time stamps
  data$Lyear <-paste(data$id, year(data$timestamp),sep="_")
  data$idtime <-paste(data$id, data$timestamp,sep="_")
  head(data)
  data<-data[!duplicated(data$idtime), ]
  head(data)
  
  data$date<-date(data$timestamp)
  
  #making julian day
  #data$julianday<- format(data$date, "%j")
  #range(data$julianday)
  
  data$julian_day<- yday(data$date)
  
  # data$status <- ifelse(data$timestamp >= "2020-05-01", 'alive', 'dead') #this depends on the current date, generally hope to see a bird within the last 7 days of analysis or considered dead
  data$coords <-paste(data$longitude , data$latitude, sep=", ")
  # data$sexage <-paste(data$Sex, data$Age, sep=", ")
  
  # data[1,]
  
  #for panafrican data:
  #data <-subset(data, latitude < 0)
  #correct deployment date
  
  #data <-subset(data, timestamp >= "2000-01-01")
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
  
  # library(geosphere)
  # library(data.table)
  # library(dplyr)
  # library(data.table)
  # library(plotly)
  
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
  data$dist_move_km=data$dist_move_m/1000 #if you want to convert it to kms
  
  #to make next ID start from zero
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$dist_move_km=ifelse(ind,data$dist_move_km, "0")
  
  #to make next ID start from zero
  ind <- with(data,c(FALSE,id[-1L] == id[-length(id)])& id!='NULL')
  summary(ind)
  data$dist_move_m=ifelse(ind,data$dist_move_m, "0")
  
  #adding change in elevation
  data<-as.data.table(data)
  setkey(data, id, timestamp)
  data<-subset(data, !is.na(data$GPSAlt))
  data$GPSAlt<-as.numeric(data$GPSAlt)
  range(data$GPSAlt)
  data[, change_in_ele := GPSAlt - shift(GPSAlt, fill = first(GPSAlt)), by = id]
  
  glimpse(data)
  #calculating time difference between points
  library(tidyverse)
  data <- data %>% 
    mutate(timestamp = ymd_hms(timestamp, tz=local.tz)) %>% 
    arrange(id, timestamp) %>% 
    group_by(id) %>%
    mutate(time_diff_mins = difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "mins"),
           time_diff_secs = as.numeric(time_diff_mins, units = 'secs'))
  
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
  
  summarydata = ddply(data,c("year","month","julian_day" ,"id"), summarise,
                      n=length(id),
                      min_d=min(dist_move_km),
                      max_d=max(dist_move_km),
                      ave_d=mean(dist_move_km),
                      sd_d=sd(dist_move_km),
                      tot_d=sum(na.omit(dist_move_km)))
  #   
  #   #plot this
  #   plot(data$timestamp, data$dist.to.prev, type = "l", bty = "n", ylab = "Distance (Km)", xlab = "Time", col = "grey40")
  #   # Plot elevations and smoother
  #   plot(data$GPSAlt, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
  #   lines(data$lowess.ele, col = "red", lwd = 3)
  #   legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"), col = c("grey40", "red"), lwd = c(1,3), bty = "n")
  #   
  # 
  # # Plot speeds and smoother
  #   plot(data$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
  #        col = "grey40", ylim=c(-50,150))
  #   lines(data$lowess.speed, col = "blue", lwd = 3)
  #   legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
  #          col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
  #   abline(h = mean(data$speed.km.per.h), lty = 2, col = "blue")
  #   
  #   
  #   range(data$GPSAlt)
  #   plot(data$GPSAlt,  type = "l", bty = "n", xaxt = "n", ylab = "Altitude (masl)", xlab = "",
  #        col = "grey40", ylim=c(800,3050))
  #   
  #   plot(data$timestamp, data$GPSAlt,type = "l", ylab = "Altitude (masl)")
  #   
  #   # Plot the track without any map, the shape of the track is already visible.
  #   plot(rev(data$longitude), rev(data$latitude), type = "l", col = "red", lwd = 2, bty = "n", ylab = "Latitude", xlab = "Longitude")
  # 
  #   #adding a column with change in altitude betweem points
  #   class(data$GPSAlt)
  #   # 
  #   # data %>%
  #   #   group_by(id) %>%
  #   #   mutate(change_in_alt = GPSAlt - lag(GPSAlt, default = first(GPSAlt), order_by = timestamp()))
  #   # 
  #   # data$change_in_alt  
  #   # 
  #   # data %>%
  #   #   group_by(id) %>%
  #   #   arrange(date) %>%
  #   #   mutate(change_in_alt = GPSAlt - lag(GPSAlt, default = first(GPSAlt)))
  #   # 
  #   # data %>%
  #   #   group_by(id) %>%
  #   #   arrange(date, .by_group = TRUE) %>%
  #   #   mutate(change_in_alt = GPSAlt - lag(GPSAlt, default = first(value)))
  #   
  #   setkey(data, id, date)
  #   data[, change_in_alt := GPSAlt - shift(GPSAlt, fill = first(GPSAlt)), by = id]
  #   data$lowess.chane_ele <- lowess(data$change_in_alt, f = 0.2)$y
  #   
  #   plot(data$change_in_alt, type = "l", bty = "n", xaxt = "n", ylab = "Change in elevation", xlab = "", col = "grey40")
  #   range(data$timestamp)
  #   
  #   plot(data$timestamp, data$change_in_alt, type = "l", bty = "n", ylab = "Change in elevation", xlab = "Time", col = "grey40")
  #   lines(data$lowess.chane_ele, col = "red", lwd = 3)
  #   legend(x="bottomright", legend = c("Change in elevation", "LOWESS delta elevation"), col = c("grey40", "red"), lwd = c(1,3), bty = "n")
  
  # 
  # names(data)
  # fig <- plot_ly(data%>% filter(id=="MEagle307"))
  # fig <- fig %>% add_trace(x = ~timestamp, y = ~speed_km_h, type = 'scatter', name = 'Speed',mode = 'lines',
  #                          marker = list(color = 'red'),
  #                          hoverinfo = "text",
  #                          text = ~paste(speed_km_h, 'kmh'))
  # fig <- fig %>% add_trace(x = ~timestamp, y = ~GPSAlt, type = 'scatter', mode = 'lines', name = 'Elevation', yaxis = 'y2',
  #                          line = list(color = '#45171D', dash="dash"),
  #                          hoverinfo = "text",
  #                          text = ~paste(GPSAlt, 'm'))
  # fig <- fig %>% layout(title = 'EAgle Metrics',
  #                       xaxis = list(title = "Time"),
  #                       yaxis = list(side = 'left', title = 'Speed kmh', showgrid = FALSE, zeroline = FALSE),
  #                       yaxis2 = list(side = 'right', overlaying = "y", title = 'Altitude m', showgrid = FALSE, zeroline = FALSE))
  # 
  # #add date range Slider
  # fig <- fig %>% layout(title = "ME HR",xaxis = list( rangeselector = list(  buttons = list(list(  count = 3,
  #                                                                                                  label = "3 mo", step = "month",   stepmode = "backward"),   list(     count = 6,  label = "6 mo",   step = "month",
  #                                                                                                                                                                        stepmode = "backward"),  list(count = 1,label = "1 yr",    step = "year", stepmode = "backward"),list(count = 1,
  #                                                                                                                                                                                                                                                                              label = "YTD",    step = "year",  stepmode = "todate"),   list(step = "all"))), rangeslider = list(type = "date")), 
  #                       yaxis = list(title = "Metric"))
  # 
  # fig
  
  #head(data)
  
  data$month_character<- months(as.Date(data$timestamp))
  
  Druid_data<-data
  range(Druid_data$timestamp)
  # # 
  # setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/Vultures")
  # write.csv(Druid_data, file="Druid_data_Active.csv")
  
}

#birds online or offline?
{
  Druid_data=data
  todaysdate<-now()
  Druid_data$days_since_seen <- difftime(todaysdate, Druid_data$timestamp,units="days")
  
  #based on whether device has checked in within last seven days?
  #specify last week of GPS fixes
  todaysdate<-now()
  oneweek<-ymd_hms(todaysdate)-days(3)
  oneweek
  
  Druid_data$status <- ifelse(Druid_data$timestamp >= oneweek, 'online', 'offline')
  
  Druid_data<-as_tibble(Druid_data)  
  Druid_data
  
  status <- Druid_data %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 
  
  palstatus <- colorFactor(c("red", "green"),
                           domain = unique(status$status))
  
  map <- leaflet(status) %>% 
    addTiles(group = "OSM (default)") %>% 
    # addCircles(~longitude, ~latitude, color = ~pal(id))%>%
    addLegend("topleft", pal = palstatus, values = ~status, title = "Status", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
    addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
               activeColor = "#3D535D",
               completedColor = "#7D4479")%>% setView(0,0,3)%>%
    addCircleMarkers(color = ~palstatus(status), stroke = FALSE, fillOpacity = 1,  ~longitude, ~latitude,  label = ~timestamp, popup=~coords)%>% # popup = popupGraph(p2)
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
    addLabelOnlyMarkers(data=status, ~longitude, ~latitude, label = as.character(status$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1))%>%
    addLabelOnlyMarkers(data=status, ~longitude, ~latitude, label = ~paste0( "Days since Last Seen:  ", formatC(days_since_seen, big.mark = ",")),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "right", textsize = "12px", textOnly = TRUE,offset = c(0, -2),  opacity = 1))#%>%  
  #addPopupImages(img, group = "data") # if you want to add a picture to your plot
  
  esri <- grep("^Esri", providers, value = TRUE)
  
  for (provider in esri) {
    map <- map %>% addProviderTiles(provider, group = provider)
  }
  
  map %>%
    addTiles(group = "OSM (default)") %>% 
    # setView(22.65073,-30.219717, zoom = 6) %>% 
    addLayersControl(baseGroups = names(esri),options = layersControlOptions(collapsed = T)) %>%
    addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
               position = "bottomright") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  
}

#LOOKING AT YOUR BIRDS LAST LOCATION
{
  
  
  ME_GPS_data=Druid_data
  
  icon.glyphicon <- makeAwesomeIcon(icon= 'star-empty', markerColor = 'blue', iconColor = 'yellow') #thumbs-down, #star, #star-empty #record #piggy-bank
  icon.fa2 <- makeAwesomeIcon(icon= 'piggy-bank', markerColor = 'white', iconColor = 'red') #thumbs-down, #star, #star-empty #record #piggy-bank
  icon.fa <- makeAwesomeIcon(icon = 'remove', markerColor = 'white', library='fa', iconColor = 'red')#, squareMarker =  TRUE)
  icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
  icon.fa3 <- makeAwesomeIcon(icon= 'thumbs-down', markerColor = 'red', iconColor = 'yellow') #thumbs-down
  
  todaysdate<-now()
  ME_GPS_data$days_since_seen <- difftime(todaysdate, ME_GPS_data$timestamp,units="days")
  
  library(leaflet)
  last.loc <- ME_GPS_data %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 
  
  ddply(last.loc, c("id"), summarise,n=length(id))
  
  pal <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "cyan","forestgreen"),
                     domain = unique(last.loc$id))
  
  palid <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "cyan","forestgreen"),
                       domain = unique(last.loc$id))
  
  map <- leaflet(last.loc) %>% 
    addCircles(~longitude, ~latitude, color = ~palid(id))%>%
    addLegend("topleft", pal = palid, values = ~id, title = "INDIVIDUAL ID", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
    addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
               activeColor = "#3D535D",
               completedColor = "#7D4479")%>% setView(0,0,3)%>%
    addMarkers(~longitude, ~latitude, popup = ~timestamp, label=~paste0( "Days since Last Seen:  ", formatC(days_since_seen, big.mark = ",")))%>%
    addCircleMarkers(color = ~palid(id), stroke = FALSE, fillOpacity = 1,  ~longitude, ~latitude,  label = ~id, popup = ~coords)#%>%
  #addLegend(data=colony, "topleft", pal = palcolony, values = ~ColonyType, title = "CV COLONY", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
  #addCircleMarkers(data=colony, color = ~palcolony(ColonyType), ~longitude, ~latitude, stroke = FALSE, fillOpacity = 0.5,label = ~ColonyType, radius=4, popup=~Location)#%>%
  #addLabelOnlyMarkers(data=colony, ~longitude, ~latitude, label = ~Location ,labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "right", textsize = "12px", textOnly = TRUE,offset = c(0, -4),  opacity = 1))
  
  esri <- grep("^Esri", providers, value = TRUE)
  
  for (provider in esri) {
    map <- map %>% addProviderTiles(provider, group = provider)
  }
  
  map %>%
    setView(22.65073,-30.219717, zoom = 6) %>% 
    addLayersControl(baseGroups = names(esri),
                     options = layersControlOptions(collapsed = T)) %>%
    addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
               position = "bottomright") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  
  #range(data$timestamp)
}

#LOOKING AT YOUR birds LAST week of LOCATIONs and current fix and timeline
{
  obs_days=14
  
  todaysdate<-now()
  oneweek<-ymd_hms(todaysdate)-days(obs_days)
  oneweek
  
  Druid_data$status <- ifelse(Druid_data$timestamp >= oneweek, 'online', 'offline')
  
  Druid_data<-data.table(Druid_data)  
  
  status <- Druid_data %>% 
    group_split(id) %>% 
    map_dfr(~filter(., row_number() %in% nrow(.):(nrow(.)-0))) 
  
  
  icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
  
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
  
  map <- leaflet(last.loc) %>% 
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
    addLegend("topleft", pal = pal, values = ~id, title = "Individual id", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
    addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
               activeColor = "#3D535D",
               completedColor = "#7D4479")%>% setView(0,0,3)%>%
    addTimeline(data = power_geo)%>% 
    addMarkers(data=verylast, ~longitude, ~latitude, label=~id, group="Last location", popup = paste(
      # HTML CODE TO FORMAT TABLE ####
      '<b>',
      toupper(verylast$id),
      '</b>',
      '<table>',
      
      '<b>:</td>',
      toupper(verylast$sexage),
      '</b>',
      '<table>',
      
      '<tr><td>Last seen</td><td><b>',
      toupper(verylast$timestamp),
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
      
      '<tr><td>Tag Voltage</td><td>',
      verylast$battery,
      '</td></tr>',
      
      '</table>'))%>%
    addLabelOnlyMarkers(data=status, ~longitude, ~latitude, label = as.character(status$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1))%>%
    #addCircleMarkers(data=verylast, color = ~pal(id), stroke = FALSE, fillOpacity = 1,  ~longitude, ~latitude,  label = ~as.character(id), popup = ~coords)%>%
    #addMarkers(~longitude, ~latitude, popup = ~as.character(timestamp()), label = ~as.character(timestamp))%>%
    # addAwesomeMarkers(lng=23.5342, lat=-31.52476, label="Volcano Rock Nest",icon = icon.ion, labelOptions = labelOptions(noHide = F), group="Active Nests")%>%
    # addAwesomeMarkers(lng=20.8461078, lat=-29.7270151, label="Kenhardt Tree Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F), group="Active Nests")%>%
    # addAwesomeMarkers(lng=21.2681851, lat=-29.6552902, label="Louis'Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
    # addAwesomeMarkers(lng=20.1152539, lat=-30.1611738, label="Bernards Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
    # addAwesomeMarkers(lng=19.1739535, lat=-30.837578,label="Klaas Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F), group="Active Nests")%>%
    # addAwesomeMarkers(lng=22.673992, lat=-30.202398,label="Johannes Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
    # addAwesomeMarkers(lng=19.4403326, lat=-30.62772,label="Rooiberg Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
    # #addAwesomeMarkers(lng=20.8461078, lat=-29.7270151, label="Kenhardt Tree Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
    # addAwesomeMarkers(lng=18.752483, lat=-31.387233, label="Spot's Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
  # addAwesomeMarkers(lng=18.63350, lat=-31.50843, label="Rustys Nest" ,icon = icon.ion, labelOptions = labelOptions(noHide = F),  group="Active Nests")%>%
  addCircleMarkers(data=last.loc, color = ~pal(id), group=~as.character(id), radius=5,stroke = FALSE, label=~timestamp, popup=~coords, fillOpacity = 1,  ~longitude, ~latitude)#%>%
  # addRasterImage(KernelDensityRaster, colors = palRaster2, opacity = .8) %>%
  # addLegend(pal = palRaster, values = KernelDensityRaster@data@values,  title = "Kernel Density of Points")
  
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
    map = addPolylines(map, 
                       lng= ~ longitude,
                       lat= ~ latitude,
                       data = last.loc[last.loc$id==id,], 
                       color= ~ pal(id),
                       weight = 0.05,opacity = 0.7, popup=id, smoothFactor = 5, group=~id)
  }
  
  map %>%
    setView(22.65073,-28, zoom = 5) %>% 
    addEasyButton(
      easyButton(
        icon = shiny::icon("home"),
        title= "Reset Zoom",
        onClick = JS(
          c("function(btn, map) {map.setView(new L.LatLng(-29.727017, 22.988433), 7);}"))))%>%
    addLayersControl(
      baseGroups = c("OSM (default)", "Satellite", "Satellite Day", "ESRI Satellite", "Esri.DeLorme", "Esri.WorldShadedRelief"),
      overlayGroups = c("Last location" , c(unique(as.character(last.loc$id)))),
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
  
  
}