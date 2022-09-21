# library(htmlwidgets)
# library(htmltools)
# library(leaflet)
# library(geojsonio)
# library(tidyverse)
# library(geojsonlint)
# 
# library(knitr)
# library(lubridate)
# library(maptools)
# library(raster)
# library(move)
# library(amt)
# #install.packages("suncalc")
# #library(ggmap)
# library(tibble)
# library(leaflet)
# library(plyr)
# library(data.table)
# library(SDLfilter)
# library(tidyverse)
# library(leaftime)
# library(suncalc)
# library(RAtmosphere)
# library(leaflet.extras)
# library(leafpop)
# # require(leaflet)
# # require(leaflet.extras)
# require(rgdal)


# Create query date output folder -----------------------------------------
todaysdate1 <- lubridate::now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate1, format = "%Y-%m-%d")
today <- format(todaysdate1, format = "%Y-%m")
outdir <- glue::glue("data output/{today}")
dir.create(outdir)


####add new iridium Kruger tags###########
##########################################

#reading in savannah units
loginStored <- movebankLogin(username="Garethjtate", password="Accipiter@0626")
getMovebankAnimals(study="Savannah_Vultures", login=loginStored)

data_new_iridium <- getMovebankData(study="Savannah_Vultures", animalName=c("WBV-4304-C11", "WBV-4305-C11", "WBV-4306-C11", "WBV-4303-C11"), login=loginStored)#, removeDuplicatedTimestamps=T)

names(data_new_iridium)
## Convert to tibble
raw_datanew_iridium<- as(data_new_iridium, "data.frame") %>%
  as_tibble()

glimpse(raw_datanew_iridium)
unique(raw_datanew_iridium$savannah_alarm_type)

raw_datanew_iridium$battery<-raw_datanew_iridium$tag_voltage
# Clean iridium data ----------------------------------------------------------
data_new_iridium <- raw_datanew_iridium %>%
  filter(sensor_type %in% "GPS") %>%
  #mutate(GPSAlt = as.numeric(height_above_ellipsoid)) %>%
  #filter(!is.na(GPSAlt)) %>%
  rename(
    id = local_identifier,
    longitude = location_long,
    latitude = location_lat
  ) %>%
  mutate(coords = str_c(latitude, longitude, sep = ", ")) %>%
  mutate(GPSAlt = 0) %>%
  #rename(GPSCourse = heading, GPSSpeed = ground_speed) %>%
  select(individual_id, id, timestamp, latitude, longitude, GPSAlt, coords, battery)

## Checks
data_new_iridium
glimpse(data_new_iridium)

unique(data_new_iridium$id)
data<-data_new_iridium

data$id<-as.factor(data$id)
# unique(data$id)

#if you want to subset speific eagles:
#data <- subset(data, data$nickname %in% c("Brakkie", "Rooiberg Female"))
data <- data[order(data$id, data$timestamp),]
Savannah_vultures_dataall<-data

glimpse(Savannah_vultures_dataall)
# range(KNPVul_dataall$battery)

range(Savannah_vultures_dataall$timestamp)

##Original
# Save and write to RData -------------------------------------------------
save(file = glue::glue("{outdir}/01_Savannah_vultures_dataall_Iridium_tracking_data.RData"),
     list = c("data", "Savannah_vultures_dataall"))


