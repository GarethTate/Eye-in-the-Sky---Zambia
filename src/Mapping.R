# library(glue)
# library(leaftime)
# library(httr)
# library(jsonlite)
# library(leaflet)
# library(leafem)
# library(htmltools)
# # #conflicted::conflict_prefer("span", "htmltools")
# library(leafpop)
# library(lubridate)
# library(leaflet.extras)
# conflicted::conflict_prefer("config", "httr")
# conflicted::conflict_prefer("flatten", "purrr")
# # Check
# conflicted::conflict_scout()

# ?sliderInput To add moving animations

## ________________________________________________________________________

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/04_NSR_mapping_data.RData"))

# LEAFLET MAP 1 -----------------------------------------------------------
source("src/Helper Functions.R")

here_app_id = 'ehftALetcOLjvopsXsZP'
here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'

palbots <- colorFactor(topo.colors(45, alpha = 1), NSR$NAME)

pal <- colorFactor(c("blue", "red", "green", "yellow", "purple",
                     "brown", "black", "pink", "orange"),
                   domain = unique(lastseven$id))

# map <- leaflet(data) %>%
#   addMouseCoordinates() %>%
#   addTimeline(data = power_geo,
#               timelineOpts = timelineOptions(styleOptions = styleOptions(radius = 5,
#                                                                          color = "black",
#                                                                          fillColor = "yellow",
#                                                                          fillOpacity = 0.5))) %>%
#   addTiles(group = "OSM (default)") %>% # Esri.WorldImagery
#   #addTiles(group = "OpenStreetMap.France (default)") %>% # Esri.WorldImagery
#   addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
#   addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMap") %>%
#   addProviderTiles(providers$GeoportailFrance.orthos, group = "Satellite") %>%
#   addProviderTiles(providers$HERE.hybridDay, group = "Satellite Day") %>% # HERE.satelliteDay
#   addProviderTiles("Stamen.TerrainBackground", group="Stamen") %>%
#   addProviderTiles("OpenStreetMap.France"  , group="OSM_2") %>%#"CartoDB.Voyager" "CartoDB.VoyagerLabelsUnder" "Stadia.Outdoors" "OpenStreetMap.France"

map <- leaflet(data) %>%
  addMouseCoordinates() %>%
  addTimeline(data = power_geo,
              timelineOpts = timelineOptions(styleOptions = styleOptions(radius = 5,
                                                                         color = "black",
                                                                         fillColor = "yellow",
                                                                         fillOpacity = 0.5
              ))) %>%
  addTiles(group = "OSM (default)") %>% # Esri.WorldImagery
  #addTiles(group = "OpenStreetMap.France (default)") %>% # Esri.WorldImagery
  addProviderTiles("HERE.terrainDay", group="Terrain", options = leaflet::providerTileOptions(
    detectRetina = TRUE,
    app_id = here_app_id,
    app_code = here_app_code))%>%
  addProviderTiles("HERE.satelliteDay", group="Satellite", options = leaflet::providerTileOptions(
    detectRetina = TRUE,
    app_id = here_app_id,
    app_code = here_app_code))%>%
   addAwesomeMarkers(lng = 38.4111201, lat = -11.930148, popup = "CHIULEXI HQ AND CAMP",
                    icon = icon.ion, labelOptions = labelOptions(noHide = F),
                    label = "CHIULEXI HQ AND CAMP") %>%
  addAwesomeMarkers(lng = 38.096081, lat = -12.174958, popup = "MARIRI HQ AND CAMP",
                    icon = icon.ion, labelOptions = labelOptions(noHide = F),
                    label = "MARIRI HQ AND CAMP") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
 addPolygons(data=NSR_admin, color =~palbots(NAME), weight = 0.5, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.05,fillColor =~palbots(NAME), popup=~NAME, group="Buffer Zone")%>%
  addPolygons(data=NSR, color ="green", weight = 0.5, smoothFactor = 0.5, #~palsa(NAME)
              opacity = 0.8, fillOpacity = 0.55,fillColor ="green", popup=~NAME, group="NSR_area",highlightOptions = highlightOptions(color = "red", weight = 2))

map

map <- map %>%
  addLegend(data = lastseven, "topleft",
            pal = pal,
            values = ~id,
            title = "INDIVIDUAL ID",
            labFormat = labelFormat(prefix = " "),
            opacity = 1,
            group = "Bird ID legend") %>%
  addMeasure(
    position = "bottomleft", primaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479"
  ) %>%
  setView(0, 0, 3)
map

map <- map %>%
  addCircleMarkers(
    data = lastseven,
    radius = 2,
    color = ~ pal(id),
    stroke = FALSE,
    fillOpacity = 1, ~longitude, ~latitude,
    label = ~ paste0(id,
                     ": Ascending (+) or Descending (-):  ",
                     formatC(change_in_ele, big.mark = ",")),
    popup = ~ paste0("Date:", timestamp, "Coordinates:", coords), group = "GPS Fixes"
  ) %>%
  addScaleBar(
    position = c("bottomleft"),
    options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)
  )

#map

## CORRECT HERE
palclusttype <- colorFactor(c("blue", "green"),
                            domain = unique(Activity_clusters$predicted_activity)
)

unique(Activity_clusters$predicted_activity)

map <- map %>%
  addPolygons(
    data = Activity_clusters,
    color = ~ palclusttype(predicted_activity),
    weight = 4, smoothFactor = 0.5,
    opacity = 1, fillOpacity = 0.6,
    fillColor = "yellow", label = ~predicted_activity,
    labelOptions = leaflet::labelOptions(
      noHide = FALSE, direction = "bottom", textsize = "10px",
      textOnly = TRUE, offset = c(0, -1), opacity = 1, style = list("color" = "red")
    ), group = "Activity Clusters",
    popup = ~ paste0(
      "<h3>CLUSTER DETAILS</h3><br>",
      "<b>Unique CLUSTER_ID</b>: ", UniqueClusterID, "<br>",
      "<b>Predicted Cluster Activity</b>: ", predicted_activity, "<br>",
      "<b>Cluster Score</b>: ", score, "<br>",
      "<b>total_individs_in_cluster over last week</b>: ", total_individs_in_clus__7, "<br>",
      "<b>Cluster Coordinates</b>: ", coords, "<br>",
      "<b>individIDs_in_clust</b>: ", individIDs_in_clust__7, "<br>",
      "<b>Total no. revisits in Cluster:</b>: ", totrev7, "<br>",
      "<b>Total_hours_in_clust:</b>: ", tot_time_in_clust__7, "<br>",
      "<b>Total_points_in_clust:</b>: ", tot_points_in_clust__7, "<br>",
      "<b>Revisits over last month:</b>: ", totrev31, "<br>",
      "<b>Total_points_in_clust over last month:</b>: ", tot_points_in_clust__31, "<br>",
      "<b>Revisits over last 180 days:</b>: ", totrev180, "<br>"#,
      #"<b>Total_points_in_clust over last 180 days:</b>: ", tot_points_in_clust__180, "<br>",
      #"<b>Revisits over last year</b>: ", totrev365, "<br>",
      #"<b>Total_points_in_clust over last year:</b>: ", tot_points_in_clust__365
    ),
  )

map

## CORRECT HERE

map <- map %>%
  addPolygons(
    data = Feeding_clusters_map,
    color = "red",
    weight = ~ tot_points_in_clust__7 / 1.3,
    smoothFactor = 0.5,
    opacity = 1, fillOpacity = 0.6, fillColor = "yellow",
    label = ~predicted_activity,
    labelOptions = leaflet::labelOptions(
      noHide = FALSE, direction = "bottom", textsize = "10px",
      textOnly = TRUE, offset = c(0, -1), opacity = 1, style = list("color" = "red")
    ), group = "Feeding Clusters", # %>%
    popup = ~ paste0(
      "<h3>FEEDING CLUSTER DETAILS</h3><br>",
      "<b>Predicted Cluster Activity</b>: ", predicted_activity, "<br>",
      "<b>Unique CLUSTER_ID</b>: ", UniqueClusterID, "<br>",
      "<b>Cluster Coordinates</b>: ", clust_coords, "<br>",
      "<b>total_individs_in_cluster over last week</b>: ", total_individs_in_clus__7, "<br>",
      "<b>individIDs_in_clust</b>: ", individIDs_in_clust__7, "<br>",
      "<b>Revisits over last week:</b>: ", totrev7, "<br>",
      "<b>Total_hours_in_clust over last week:</b>: ", tot_time_in_clust__7, "<br>",
      "<b>Total_points_in_clust:</b>: ", tot_points_in_clust__7, "<br>",
      "<b>Revisits over last month:</b>: ", totrev31, "<br>",
      "<b>Revisits over last 180 days:</b>: ", totrev180, "<br>",
      "<b>Revisits over last year</b>: ", totrev365
    ),
  )


#map
# CORRECT HERE

# glimpse(week_data_sub)


## THIS week_data needs to be a much smaller subset that intersects only within the clusters
library(data.table)
# SFS<-as.data.frame(SFS)
# names(SFS)

map <- map %>%
  addCircleMarkers(
    data = week_data_sub, lng = ~longitude, lat = ~latitude, popup = ~ paste0(
      "<h3>Point of interest Details</h3><br>",
      "<b>Unique event_id</b>: ", event_id, "<br>",
      "<b>NAME</b>: ", NAME, "<br>",
      "<b>Area</b>: ", NAME, "<br>",
      "<b>Date</b>: ", timestamp, "<br>",
      "<b>Days since alarm triggered</b>: ", days_lapsed_since_feeding_event, "<br>",
      "<b>coords</b>: ", coords
    ),
    label = ~ paste0("Point of interest: ",
                     timestamp, ": Days since triggered  ",
                     formatC(days_lapsed_since_feeding_event, big.mark = ",")),
    radius = 0.5, color = "red", group = "Points of Interest"
  )

map

# # glimpse(SFS)
# # class(SFS)
# # SFS<-as.data.frame(SFS)
# palSFS<- colorFactor(c("red", "brown", "green", "orange"),
#                   domain = unique(SFS$Status_Category))
#
# map <- map %>%
#   addCircleMarkers(data=SFS, color =~palSFS(Status_Category), stroke = FALSE,radius=8, fillOpacity = 1,  ~longitude, ~latitude,  label = ~Restaurant_Name, popup=~RESPONSIBILITY)
#
# leaflet(SFS) %>%
#   addProviderTiles("CartoDB.Positron")%>%#HERE.hybridDay HERE.satelliteDay   Esri.WorldGrayCanvas CartoDB.Positron  CartoDB.DarkMatter Stamen.TonerLines # Stamen.TonerLabels  Esri.WorldPhysical GeoportailFrance.orthos
#   addCircleMarkers(data=SFS, color =~pal(Status_Category), stroke = FALSE,radius=8, fillOpacity = 1,  ~longitude, ~latitude,  label = ~Restaurant_Name, popup=~RESPONSIBILITY)
#
#addCircles(data=SFS, color = "red" ,radius=4, fillOpacity = 0.5, ~longitude, ~latitude,  label = ~Restaurant_Name, popup = ~as.character(RESPONSIBILITY), group="Feeding Site", labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "10px",
#                                                                                                                                                                                                               textOnly = TRUE,offset = c(0, -1),  opacity = 1, style = list("color" = "blue")))
# addCircleMarkers(
#   data=SFS, color = "green" ,radius=4, fillOpacity = 0.5, lng = ~longitude, lat = ~latitude,group= "Feeding Site"
# )
#
# map


# NOTE Colors/legend all good up to here

for (id in levels(lastseven$id)) {
  map <- addPolylines(map,
                      lng = ~longitude,
                      lat = ~latitude,
                      data = lastseven[lastseven$id == id, ],
                      color = ~ pal(id),
                      weight = 0.05, opacity = 0.7, popup = id, smoothFactor = 5, group = "7 day GPS tracks"
  )
}

map <- map %>%
  #setView(23.40646, -23.07691, zoom = 5) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","Terrain", "Satellite", "ESRI Satellite"),# "OSM (default)","OSM_2", "Stamen", "DarkMap", "Satellite", "Satellite Day", "ESRI Satellite"
    overlayGroups = c("Bird ID legend", "Feeding Clusters",
                      "Activity Clusters", "GPS Fixes", "Points of Interest",
                      "7 day GPS tracks", "Buffer Zone","NSR_area"),
    options = layersControlOptions(collapsed = F)
  ) %>%
    addMiniMap(
    tiles = providers$OSM, toggleDisplay = TRUE,
    position = "bottomright"
  ) %>%
  htmlwidgets::onRender(., jsCode = "
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")

map

# # Adding API clusters to map ----------------------------------------------
# url <- "https://6jvshe0882.execute-api.af-south-1.amazonaws.com/default/api_ewt_clusters_all"
# resp <- httr::GET(url, add_headers("x-api-key" = "cipJjCOt4S3yrpfAn8iBD8ofbZGD4ZpM8Ucmlphq"))
# 
# apidata <- fromJSON(content(resp, "text"), simplifyDataFrame = TRUE)[[1]]

# apidata.table <- apidata[[2]][[1]]$properties
# head(apidata.table)

# class(dead_birds$days_since_seen)
dead_birds$days_since_seen <- as.numeric(dead_birds$days_since_seen)
dead_birds2 <- subset(dead_birds, days_since_seen < 7)

# LEAFLET MAP 2 -----------------------------------------------------------
finalmap <- map %>%
  # setView(23.40646, -23.07691, zoom = 5) %>%
  # addMouseCoordinates() %>%
  # addAwesomeMarkers(data = nests, lng = ~longitude, lat = ~latitude,
  #                   popup = ~carcass_species, label = ~date_found,
  #                   icon = icon.ion, group = "Vulture Nests") %>%
  # addAwesomeMarkers(lng = 23.63696, lat = -19.52278, popup = "DOG CAMP",
  #                   icon = icon.ion, labelOptions = labelOptions(noHide = F),
  #                   label = "DOG CAMP") %>%
  # addAwesomeMarkers(lng = 21.772, lat = -23.994, icon = icon.ion,
  #                   labelOptions = labelOptions(noHide = F),
  #                   label = "Raptors Botswana HQ", group = "Raptors Botswana HQ") %>%
  addAwesomeMarkers(data = dead_birds2, icon = icon_bird_down, ~longitude, ~latitude,
                    label = ~ paste0(latitude, ", ", longitude),
                    popup = ~ paste0("Days since last seen:", days_since_seen),
                    group = "Vulture Mortality Alert") %>%
  # addGeoJSONv2(jsonlite::toJSON(apidata),
  #              weight = 10, fillOpacity = 1, color = "blue", fillColor = "yellow",
  #              popupProperty = propstoHTMLTable(
  #                props = c("cluster_date ", "daycluster ", "count_overlappingclusters",
  #                          "number_of_birds_assoc", "points_in_geomcollected", "individual_bird_days_30",
  #                          "distinct_birds_30", "individual_bird_days_180", "distinct_birds_180",
  #                          "individual_bird_days_365", "distinct_birds_365"),
  #                table.attrs = list(class = "table table-striped table-bordered"), drop.na = TRUE
  #              ), group = "Activity Clusters API"
  # ) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","Terrain", "Satellite", "ESRI Satellite"),
    overlayGroups = c("Bird ID legend", "Vulture Mortality Alert", "Feeding Clusters",
                      "Activity Clusters", "Activity Clusters API", "GPS Fixes",
                      "Points of Interest", "7 day GPS tracks", "Buffer Zone", "NSR_area"),
     options = layersControlOptions(collapsed = T)
  ) #%>%
  # addMiniMap(
  #   tiles = providers$OSM, toggleDisplay = TRUE,
  #   position = "bottomright"
  # ) %>%
  # htmlwidgets::onRender(., jsCode = "
  #   function(el, x) {
  #     var myMap = this;
  #     myMap.on('baselayerchange',
  #       function (e) {
  #         myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
  #       })
  #   }")
finalmap

finalmap=finalmap%>% hideGroup("7 day GPS tracks")%>% 
  hideGroup("GPS Fixes")


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

todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%d %B %Y")

oneweek <- now(tzone = "Africa/Johannesburg") - days(obs_days)
oneweek <- format(oneweek, format = "%d %B")

maptitle <- paste("NIASSA EYE IN THE SKY FEEDING EVENTS ", oneweek, " - ", todaysdate, sep = "")
maptitle

title <- tags$div(
  tag.map.title, HTML(maptitle)
)

finalmap <- finalmap%>%
  addControl(title, position = "topleft", className = "map-title")

# img <- "C:/Users/GarethT/OneDrive - EWT/BOPP/DOCUMENTS/Logos/Endangered Wildlife Trust.jpg"
# plot(img)
# 
# finalmap <- leafem::addLogo(finalmap,
#                             "C:/Users/GarethT/OneDrive - EWT/BOPP/DOCUMENTS/Logos/EWTRBOTS.jpg", # LARGE_Landscape_Endangered Wildlife Trust+Tag_HIGH RES-01
#                             position = "topleft",
#                             alpha = 1,
#                             src = c("local"),
#                             # offset.x = 5,
#                             # offset.y = 40,
#                             width = 200,
#                             height = 60
# ) %>%
#   addMouseCoordinates() %>%
#   addControl(title, position = "topleft", className = "map-title")
# 
finalmap

# save.image("tempwedspm.RData")

# Export maps and data sent to rangers/users ------------------------------

# TODO This section can be formatted further for improved clarity

## Points are week_data
# week_data
week_data_sub

# week_pts <- SpatialPointsDataFrame(
#   coordinates(cbind(week_data$longitude, week_data$latitude)),
#   data = week_data,
#   proj4string = CRS("+proj=longlat +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   )

week_pts <- SpatialPointsDataFrame(
  coordinates(cbind(week_data_sub$longitude, week_data_sub$latitude)),
  data = week_data_sub,
  proj4string = CRS("+proj=longlat +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
)

Feeding_clus_poly_final

## Plot to visualize
# plot(as(Feeding_clus_poly_final, "Spatial"), col=colorRampPalette(blues9)(12))
# plot(week_pts, pch=16, cex=.3,col="red")
# plot(week_pts, pch=16, cex=.3,col="red", add=TRUE)
# map.axes()

range(week_pts$UniqueClusterID)

range(Feeding_clus_poly_final$UniqueClusterID)

Feeding_clus_poly_final$UniqueClusterID_1 = Feeding_clus_poly_final$UniqueClusterID

# Feeding_clus_poly_final <- Feeding_clus_poly_final %>%
#   rename(UniqueClusterID_1 = UniqueClusterID  )

week_data.area <- week_pts %over% as(Feeding_clus_poly_final, "Spatial")
week_data.area

names(week_data.area)

week_data_sub <- cbind(week_data_sub, week_data.area)
names(week_data_sub)

data_feeding <- subset(week_data_sub,
                       select = c("event_id", "individIDs_in_clust__7", "NAME",
                                  "timestamp",
                                  "longitude", "latitude", "UniqueClusterID_1",
                                  "clust_coords", "predicted_activity"))

data_feeding$Area <- data_feeding$NAME
data_feeding$Parent_Cluster <- data_feeding$UniqueClusterID_1
data_feeding$Parent_Cluster_co_ords <- data_feeding$clust_coords
range(data_feeding$UniqueClusterID_1)

data_feeding <- subset(data_feeding,
                       select = c("event_id", "NAME", "Area", "timestamp",
                                  "longitude", "latitude", "Parent_Cluster",
                                  "Parent_Cluster_co_ords", "predicted_activity", "individIDs_in_clust__7"))

data_feeding <- data_feeding[order(data_feeding$timestamp), ]

today <- now(tzone = "Africa/Johannesburg")
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%d %B %Y")
todaysdate

oneweek <- now(tzone = "Africa/Johannesburg") - days(obs_days)
oneweek <- format(oneweek, format = "%d %B")
oneweek

data_feeding$days_lapsed_since_PoI_triggered <- difftime(today,
                                                         data_feeding$timestamp, units = "days")

data_feeding$longitude <- format(round(data_feeding$longitude, 6), nsmall = 6) # specify decimal places
data_feeding$latitude <- format(round(data_feeding$latitude, 6), nsmall = 6) # specify decimal places

data_feeding$Co_Ordinates <- paste(data_feeding$latitude,
                                   data_feeding$longitude,
                                   sep = ", ")

data_feeding <- data_feeding[!duplicated(data_feeding$Co_Ordinates), ]
data_feeding

data_feeding <- data_feeding[order(data_feeding$NAME,
                                   data_feeding$Parent_Cluster), ] # order by location

data_feeding <- subset(data_feeding,
                       select = c("event_id", "NAME", "Area",
                                  "timestamp", "days_lapsed_since_PoI_triggered", "longitude", "latitude",
                                  "Co_Ordinates", "Parent_Cluster",
                                  "Parent_Cluster_co_ords", "predicted_activity", "individIDs_in_clust__7"))

data_feeding$Date_searched <- ""
data_feeding$"Findings: (carcass/breeding site/roost/water)" <- ""
data_feeding$species <- ""
data_feeding$Approx_carcass_age_days <- ""
data_feeding$"Approx_cause_of_death: (poached/natural etc.)" <- ""

names(data_feeding)

# FIXME There shouldn't be any NA's in the columns - check what's potting

data_feeding$timestamp <- format(data_feeding$timestamp, format = "%Y/%m/%d %H:%M")

data_feeding %>%
  write_csv(glue("{outdir}/NSR_Vulture_feeding {oneweek}-{todaysdate}.csv"))

clust_data <- as.data.frame(Feeding_clus_poly_final)

clust_data <- subset(clust_data,
                     select = c("UniqueClusterID",
                                "individIDs_in_clust__7", "tot_revisits_in_clust__7", "tot_revisits_in_clust__31",
                                "tot_revisits_in_clust__180", "tot_revisits_in_clust__365","tot_time_in_clust__7",  "tot_time_in_clust__31",  "tot_time_in_clust__180", "tot_time_in_clust__365",
                                "Cluster_Type", "coords"))

clust_data <- subset(clust_data, !is.na(clust_data$coords))

clust_data %>%
  write_csv(glue("{outdir}/NSR_Vulture_feeding_clusters {oneweek}-{todaysdate}.csv"))

# range(clust_data$UniqueClusterID)
# range(data_feeding$Parent_Cluster)


# Export as HTML file
htmlwidgets::saveWidget(
  widget = finalmap,
  file = glue("{outdir}/NSR_EyeintheSky {oneweek}-{todaysdate}.html"),
  selfcontained = T)


# Write leaflet map -------------------------------------------------------

saveRDS(finalmap, glue("{outdir}/finalmap.rds"))


# Save workspaces ---------------------------------------------------------
save.image(glue("{outdir}/all_finalmap_data.RData"))

save(file = glue("{outdir}/shiny_global_data.RData"),
     list = c("lastseven","power_geo","Feeding_clusters_map","Feeding_clus_poly_final",
              "week_data_sub","clust_data","data_feeding","dead_birds2","NSR"))

# END ---------------------------------------------------------------------

