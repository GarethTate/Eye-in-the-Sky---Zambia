# Libraries
# library(conflicted)
# library(tidyverse)
# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("lag", "dplyr")
# conflicted::conflict_prefer("select", "dplyr")
# conflicted::conflict_prefer("summarise", "dplyr")
# library(lubridate)
# library(glue)
#
# # Check
# conflicted::conflict_scout()

## ________________________________________________________________________

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

# NOTE data object contains all GPS points of all birds
load(glue("{outdir}/02_NSR_tracking_data_metrics.RData")) 

# NOTE dataNSR starts as a duplicate of data
dataNSR <- data

names(dataNSR)

## Add misc columns
dataNSR$id_all <- "vul"
dataNSR$coords <- paste(data$latitude, data$longitude, sep = ", ")

# Check tag status --------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
dataNSR$days_since_seen <- difftime(todaysdate, dataNSR$timestamp, units = "days")

## Check whether the tag is online or offline based on timestamp within the last 5 days
tagdead <- ymd_hms(todaysdate) - days(5)
dataNSR$tag_status <- ifelse(dataNSR$timestamp >= tagdead, "online", "offline")

## Check the tag status of the most recent fix for each bird
tag_status <- dataNSR %>%
  group_by(id) %>%
  group_split() %>%
  map_dfr(~ filter(., row_number() %in% nrow(.))) %>%
  select(id,sex, age, timestamp, days_since_seen, tag_status, latitude, longitude) %>%
  write_csv(glue("{outdir}/01_tag_status_{format(todaysdate, '%Y-%m-%d')}.csv"))

# Stationary/mortality alarm ----------------------------------------------

## Extract the last 42 fixes for each individual
bird_activity <- dataNSR %>%
  group_by(id) %>%
  group_split() %>%
  map_dfr(~ filter(., row_number() %in% nrow(.):(nrow(.) - 42)))

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

glimpse(activity_status)

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

# QUESTION Why are tag statuses different in join?
# QUESTION Why do we need duplicate columns here?

dataNSR <- dataNSR %>%
  left_join(activity_status %>%
              select(id, movement, days_since_seen, tag_status),
            by = "id"
  )

# dataNSR <- dataNSR %>%
#   rename(tag_status = tag_status.y)
# 
# glimpse(activity_status)
# glimpse(dataNSR)
# head(dataNSR$tag_status.y)
# # View(dataNSR)

dataNSR %>%
  group_by(id, tag_status.y) %>%
  tally()

# # Read in poi data --------------------------------------------------------
# poi <- read_csv("data input/Eye_in_the_sky_RSA_Points_of_interest_active.csv")
# 
# local.tz <- "Africa/Johannesburg"
# poi$date_found <- as.POSIXct(format(poi$date_found, tz = local.tz), tz = local.tz)
# 
# ## Make sure date order is correct
# poi <- poi[order(poi$date_found), ]
# 
# ## Lat long as numeric
# poi$latitude <- as.numeric(poi$latitude)
# poi$longitude <- as.numeric(poi$longitude)
# 
# ## All carcass
# carcass <- subset(poi, poi$findings %in% c("carcass"))
# 
# ## Carcasses within the last 21 days
# todaysdate <- now(tzone = "Africa/Johannesburg")
# days <- ymd_hms(todaysdate) - days(21)
# carcasses_recent <- subset(carcass, date_found >= days)
# 
# ## Nest sites
# nests <- subset(poi, poi$findings %in% c("nesting site"))
# 
# 
# #supplementary feeding sites
# SFS <- read.csv("data input/SFSDatabase_2019.csv")
# names(SFS)
# class(SFS)
# 
# #remove na's'
# ##removing NAs from coordinates
# SFS<-subset(SFS, !is.na(SFS$longitude))
# SFS<-subset(SFS, !is.na(SFS$latitude))
# 
# 
# #ddply(SFS, c("Status_Category"), summarise,n=length(Status_Category))
# SFS<-subset(SFS, Status_Category=="Active")
# names(SFS)
# glimpse(SFS)


# pal<- colorFactor(c("red", "brown", "green", "orange"),
#                    domain = unique(SFS$Status_Category))
#
# #addProviderTiles        https://leaflet-extras.github.io/leaflet-providers/preview/
#
# leaflet(SFS) %>%
#    addProviderTiles("CartoDB.Positron")%>%#HERE.hybridDay HERE.satelliteDay   Esri.WorldGrayCanvas CartoDB.Positron  CartoDB.DarkMatter Stamen.TonerLines # Stamen.TonerLabels  Esri.WorldPhysical GeoportailFrance.orthos
#    #addProviderTiles("Stamen.TonerLines")%>%
#    addLegend("topleft", pal = pal, values =~Status_Category, title = "SFS Status", labFormat = labelFormat(prefix = " "), opacity = 1) %>%
#    addLabelOnlyMarkers(data=SFS, ~longitude, ~latitude, label = as.character(SFS$Restaurant_Name),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1))%>%
#    addCircleMarkers(data=SFS, color =~pal(Status_Category), stroke = FALSE,radius=8, fillOpacity = 1,  ~longitude, ~latitude,  label = ~Restaurant_Name, popup=~RESPONSIBILITY)%>%
#    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))
# #


# Save and write to RData -------------------------------------------------
save(
  file = glue("{outdir}/03_NSR_tracking_data_activity.RData"),
  list = c(
    "data", "dataNSR", "summary_data", "activity_status","dead_birds", "offline_tags",
    "online_tags"
  )
)

