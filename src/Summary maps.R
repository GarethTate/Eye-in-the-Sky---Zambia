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
library(leafpop)
library(shiny)
library(leaflet.extras)
library(formattable) #https://www.rdocumentation.org/packages/gt/versions/0.2.0.5
library(gt) #https://gt.rstudio.com/articles/intro-creating-gt-tables.html
library(paletteer)#https://gt.rstudio.com/reference/data_color.html
library(plyr)

todaysdate1 <- lubridate::now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate1, format = "%Y-%m")
today <- format(todaysdate1, format = "%Y-%m")
outdir <- glue::glue("data output/{today}")

load(glue("{outdir}/NSR_EITS_tracking_data_activity.RData"))

dataNSR<-data

dataNSR$coords <-paste(dataNSR$longitude , dataNSR$latitude, sep=", ")

head(dataNSR$coords)

# glimpse(dataNSR)
# glimpse(dead_birds)
# glimpse(dead_birds)


#OVERALL MAP FOR VultureS 


obs_days=30

todaysdate<-now()
oneweek<-ymd_hms(todaysdate)-days(obs_days)
oneweek

icon.glyphicon <- makeAwesomeIcon(icon = "flag", markerColor = "blue",
                                  iconColor = "black", library = "glyphicon",
                                  squareMarker =  TRUE)

icon.fa <- makeAwesomeIcon(icon = "flag", markerColor = "red", library = "fa",
                           iconColor = "yellow")

icon.ion1 <- makeAwesomeIcon(icon = "home", markerColor = "green",
                             library = "ion")

icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
icon.fa2 <- makeAwesomeIcon(icon= 'piggy-bank', markerColor = 'white', iconColor = 'red') #thumbs-down, #star, #star-empty #record #piggy-bank
icon_bird_down <- makeAwesomeIcon(icon= 'star', markerColor = 'white', iconColor = 'red') #thumbs-down, #star, #star-empty #record #piggy-bank

danger = makeAwesomeIcon(icon = "exclamation", library = "fa", 
                         markerColor = "white")


child = makeAwesomeIcon(icon = "child", library = "fa", 
                        markerColor = "blue")

triangle_green <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/green-normal-triangle-png-8.png",
                           iconWidth = 18, iconHeight = 18)
square_red <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/red-square-png-14.png",
                       iconWidth = 18, iconHeight = 18)

data$id<-as.factor(data$id)  
verylast$id<-as.factor(verylast$id)  
last.loc$id<-as.factor(last.loc$id)  

prj <- "+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
prjCRS <- CRS(prj)
wgs <- "+proj=longlat +datum=WGS84"

NSR<- spTransform(readOGR("data input/NSR_shapefiles/Niassa Special Reserve/NSR_Zonning_Detailed_in_Community_Tourism_Units.shp"), wgs)


# verylast=as.data.table(verylast)
# verylast=as_tibble(verylast)
# 
# ddply(last.loc, c("id"), summarise,n=length(id))


last.loc$speed_km_h<-as.numeric(last.loc$speed_km_h)


#summarydata$tot_n_km<-summarydata$tot/1000

pal <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "wheat3","forestgreen"),
                   domain = unique(last.loc$id))



# names(verylast)
# range(last.loc$timestamp)
# obs_days<-14
oneweek1<-ymd_hms(todaysdate)-days(obs_days)
# oneweek


last.loc <-subset(dataNSR, timestamp >= oneweek1) 

# class(last.loc)
last.loc <-as_tibble(last.loc)
last.loc$id<-as.factor(last.loc$id)  
# last.loc <-as.data.table(last.loc)

glimpse(last.loc)

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


range(power$timestamp)
range(last.loc$timestamp)


birdmap <- leaflet(last.loc) %>% 
  addMouseCoordinates()%>%
  #setView(lng = 18, lat = -32, zoom = 8) %>%
  addTiles(group = "OSM (default)") %>% #Esri.WorldImagery
  addProviderTiles("OpenStreetMap.France"  , group="OSM_2") %>%#"
  addProviderTiles("Stamen.TerrainBackground", group="Stamen") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") %>%
  addProviderTiles(providers$GeoportailFrance.orthos, group = "Satellite")%>%
  addProviderTiles(providers$HERE.hybridDay, group = "Satellite Day")%>% #HERE.satelliteDay 
  addProviderTiles(providers$Esri.DeLorme, group = "Esri.DeLorme")%>% #Esri.WorldShadedRelief
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "Esri.WorldShadedRelief")%>% #
  addCircleMarkers(data=offline_tags, color = "red" ,radius=10, opacity = 1, fillOpacity = 0.2, ~longitude, ~latitude,  label=~paste0(id,": ",longitude,", ", latitude ), popup = ~paste0(id,": ","days since last seen:",days_since_seen), group="Tag offline alert")%>%
  addAwesomeMarkers(data=dead_birds, icon=danger, ~longitude, ~latitude,  label = ~paste0(id,": ",longitude,", ", latitude ), popup = ~paste0("Nesting/Mortality Alert: ",id,": ", "Distance traveled over last 42 fixes:", tot_n_km), group="Nesting/Mortality Alert")%>%  
  addLegend("topleft", pal = pal, values = ~id, title = "Individual id", labFormat = labelFormat(prefix = " "), opacity = 1)%>%
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
  addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>% setView(0,0,3)%>%
  addPolygons(data=NSR, color ="green", weight = 0.5, smoothFactor = 0.5, #~palsa(NAME)
              opacity = 0.8, fillOpacity = 0.55,fillColor ="green", popup=~NAME, group="NSR_area")%>%
  addTimeline(data = power_geo,timelineOpts = timelineOptions(styleOptions = styleOptions(radius = 6,color = "red",fillColor = "black",
                                                                                          fillOpacity = 1)))%>% 
  addAwesomeMarkers(data=verylast,icon=icon.fa, ~longitude, ~latitude, label=~id, group="Last location", popup = paste(
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
    
    '<tr><td>Total kms traveled this month</td><td>',
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
  addCircleMarkers(data=last.loc, color = ~pal(id), group=~as.character(id), radius=1,stroke = FALSE, label=~paste0(id,": ",  timestamp), popup=~coords, fillOpacity = 1,  ~longitude, ~latitude)#%>%

# birdmap

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

maptitle<-paste("Vulture Movements ",threedays," - ", today ,sep="")
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

birdmap<- birdmap %>%
  #setView(22.65073,-28, zoom = 5) %>% 
  addEasyButton(
    easyButton(
      icon = shiny::icon("home"),
      title= "Reset Zoom",
      onClick = JS(
        c("function(btn, map) {map.setView(new L.LatLng(-29.727017, 22.988433), 7);}"))))%>%
  addLayersControl(
    baseGroups = c("OSM (default)","OSM_2", "Stamen", "Satellite", "Satellite Day", "ESRI Satellite", "Esri.DeLorme", "Esri.WorldShadedRelief"),
    overlayGroups = c("Last location" , "Tag offline alert", "Nesting/Mortality Alert", "NSR_area", c(unique(as.character(last.loc$id)))),
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

birdmap 

# addGraticule(
#   birdmap,
#   interval = 1,
#   sphere = FALSE,
#   style = list(color = "#333", weight = 0.1),
#   layerId = NULL,
#   group = NULL,
#   options = pathOptions(pointerEvents = "none", clickable = T))

# Export as HTML file
htmlwidgets::saveWidget(
  widget = birdmap,
  file = glue("{outdir}/{maptitle}.html"),
  selfcontained = T)


# Write leaflet map -------------------------------------------------------

saveRDS(birdmap, glue("{outdir}/{maptitle}.rds"))


# Save workspaces ---------------------------------------------------------
save.image(glue("{outdir}/04_NSR_all_finalmap_data.RData"))

save(file = glue("{outdir}/05_NSR_shiny_global_data.RData"),
     list = c("data","power_geo","birdmap"))

# END ---------------------------------------------------------------------
# #Looking at movements using ggmap and ggplot
require(ggplot2)
require(ggmap)
#   
cdat<-as.data.frame(dataNSR)
# class(cdat$latitude)

cdat<-subset(cdat, latitude < -5)

cdat$doy <- as.numeric(strftime(cdat$timestamp, format = "%j"))
cdat$year <- as.numeric(format(cdat$timestamp, format="%Y"))

cbbox <- make_bbox(lon = cdat$longitude, lat = cdat$latitude, f = .02) #from ggmap
sq_map <- get_map(location = cbbox, maptype = "hybrid", source = "google")
# sq_map2 <- get_map(location = cbbox,source="stamen", maptype="watercolor", crop=FALSE)
# 
# new_map <- get_map(location = cbbox, map_service = "osm", map_type = "streets_de")

# ggmap(sq_map3)

plot(sq_map)

# map=ggmap(sq_map3) +
#   geom_path(data = cdat, aes(x = longitude, y = latitude, color = id),
#               size = 0.5, lineend = "round") +
# labs(x = " ", y = " ", title = "Inividual tracks") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# map

library(rgdal)

shpData <- readOGR(dsn="data input/NSR_shapefiles/Niassa Special Reserve/NSR_Zonning_Detailed_in_Community_Tourism_Units.shp")
proj4string(shpData) # describes data’s current coordinate reference system
# to change to correct projection:
shpData <- spTransform(shpData,
                       CRS("+proj=longlat +datum=WGS84"))

#https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf
map=ggmap(sq_map) +
  theme(legend.position = "none")+
  geom_polygon(aes(x = long, y = lat, group=group),
             data = shpData, color ="white", fill ="green",
             alpha = .4, size = .1)+
  geom_path(data = cdat, aes(x = longitude, y = latitude, color = id),
          size = 0.5, lineend = "round") +
  labs(x = " ", y = " ", title = "Inividual tracks") +
  theme_minimal() +
  theme(legend.position = "none")
map


# ggmap(sq_map) +
#   geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=1)+
#   geom_path(aes(x=longitude, y=latitude), data=cdat, alpha=0.3, colour="white")+
#   geom_polygon(data=NSR,aes(x=long,y=lat, group=group),alpha=0.25,colour="red", fill=NA)


# ggmap(sq_map) +
#   geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=0.5)+
#   labs(x="Longitude", y="Latitude")+
#   ggtitle("White-backed Vulture movements") + 
#   theme(plot.title = element_text(lineheight=0.8,size=11, hjust = 0.5))+
#   theme(legend.title = element_text(hjust = 0.5))+
#   guides(color=guide_legend(title.hjust=0.5))+
#   theme_bw()+
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
#   theme(legend.position= c(0.86, 0.885))+ #can use legend.position= "right" or "bottom"
#   theme(legend.title = element_text(colour="blue", size=10,face="bold"))+
#   theme(legend.text = element_text(colour="black", size=10))+
#   theme(legend.background = element_rect(fill="white",size=0.5, 
#                                          linetype="solid",colour ="darkblue"))+
#   guides(color = guide_legend(override.aes = list(size=3)))+
#   ggtitle("White-backed Vulture movements") + 
#   theme(plot.title = element_text(lineheight=0.8,size=11, hjust = 0.5))+
#   theme(legend.title = element_text(hjust = 0.5))+
#   guides(color=guide_legend(title.hjust=0.5))


#making an overview map
CA<-map_data("worldHires","Mozambique", lwd=0.5)
CAcounty<-map_data("worldHires","Tanzania", lwd=0.5,boundary=FALSE,lty=3, col="gray30")
SA<-map_data("worldHires","South Africa", lwd=0.5,boundary=FALSE,lty=3, col="gray30")
# EA<-map_data('worldHires',xlim=c(-20,15),  ylim=c(-35,40),lwd=0.5,boundary=FALSE,lty=3, col="gray30",add=TRUE)
EA<-map_data("worldHires",c('Namibia', 'South Africa', 'Botswana', 'Angola',
                            'Zambia', 'Zimbabwe'), lwd=0.5,boundary=FALSE,lty=3, col="gray30")


CA<-fortify(CA)
CAcounty<-fortify(CAcounty)
SA<-fortify(SA)
EA<-fortify(EA)
# #EA<-map_data('worldHires',xlim=c(-20,15),  ylim=c(-35,40))
# a=map_data('worldHires', 
#     xlim=c(-20,50),  # longitude
#     ylim=c(-30,10))  # latitude
# 
# overviewmap<-ggplot() + 
#   geom_polygon(data=a, aes(long, lat, group=group), 
#                color="black", fill=NA) 
# overviewmap


overviewmap<-ggplot() + 
  geom_polygon(data=CA, aes(long, lat, group=group), 
               color="black", fill= "grey90") +
  geom_polygon(data=CAcounty, aes(long, lat, group=group), 
               color="gray50", fill="antiquewhite", linetype=3)+
  geom_polygon(data=EA, aes(long, lat, group=group),
               color="gray50", fill="antiquewhite", linetype=3)+
  geom_polygon(data=shpData, aes(long, lat, group=group), 
               color="black", fill= "green", linetype=3)+
  theme(panel.grid.major = element_line(color = gray(.2),
                                        linetype = "dashed", size = 0.05), panel.background = element_rect(fill = "aliceblue"))+
  # geom_point(aes(x = 31.5, y=-24), size=4, # this is where sites are
  #            pch=1, col="white", bg="red")+
  geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=1)+
  coord_equal()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

overviewmap

library(maptools)
library(maps)
library(cowplot)
library(grid)

finalmap <- ggmap(sq_map) +
                    geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=0.5)+
  labs(x="Longitude", y="Latitude")+
  # geom_polygon(aes(x = long, y = lat, group=id),
  #              data = NSR, color ="white", fill ="green",
  #              alpha = .4, size = .1)+
  geom_path(data = cdat, aes(x = longitude, y = latitude, color = id),
            size = 0.5, lineend = "round") +
  # geom_polygon(data=CA, aes(long, lat, group=group), 
  #              color="black", fill="gray50") +
  # geom_polygon(data=CAcounty, aes(long, lat, group=group), 
  #              color="gray50", fill=NA, linetype=3)+
  # geom_polygon(data=EA, aes(long, lat, group=group),
  #              color="gray50", fill=NA, linetype=3)+
  # geom_polygon(data=shpData, aes(long, lat, group=group), 
  #              color="black", fill= "green", linetype=3)+
  theme(panel.grid.major = element_line(color = gray(.2),
                                        linetype = "dashed", size = 0.05), panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Eye in the Sky Niassa; GPS tracked Bird movements") + 
  theme(plot.title = element_text(lineheight=0.8,size=5, hjust = 0.5))+
  theme(legend.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(title.hjust=0.5))+
  # theme_bw()+
  theme(panel.grid.major = element_line(color = "white")) +
  theme(plot.background =
          element_rect(fill = "white", linetype = 1,
                       size = 0.3, colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  #theme(legend.position = c(.15, .95))+#can use legend.position= "right" or "bottom"
  theme(legend.position = "bottom",
        legend.box = "vertical")+
  theme(legend.title = element_text(colour="blue", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=10))+
  theme(legend.background = element_rect(fill="white",size=0.5, 
                                         linetype="solid",colour ="darkblue"))+
  guides(color = guide_legend(override.aes = list(size=5)))+
  ggtitle("Eye in the Sky Niassa; GPS tracked Bird movements") + 
  theme(plot.title = element_text(lineheight=0.8,size=11, hjust = 0.5))+
  theme(legend.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(title.hjust=0.5))+
  geom_text(x = 32, y = -25, label = "Nesting site", colour="white")


finalmap=finalmap+
  geom_point()+
  guides(color = guide_legend(override.aes = list(size = 2))) + 
  scale_color_discrete(name = "BIRD ID") # Manual legend title

finalmap

finalmap<-ggdraw() + 
  draw_plot(finalmap, 0, 0, 1, 1) +
  draw_plot(overviewmap, 0.6,0.63,0.25,0.3)  #("left/right","up/down", "size of map",size of map )


finalmap

ggsave("NSR_Vulture Map.pdf", width = 30, height = 20, units = "cm", dpi = 640, path=outdir)


#map 2

library(rgdal) # used to read world map data
library(rgeos) # to fortify without needing gpclib
library(maptools)
library(ggplot2)
library(scales) # for formatting ggplot scales with commas
library("ggplot2")
theme_set(theme_bw())
library("sf")
library(cowplot)


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world1=as(world, "Spatial")
centroids.df <- as.data.frame(coordinates(world1))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
# This shapefile contained population data, let's plot it.
popList <- world1@data$abbrev_len
idList <- world1@data$name

pop.df <- data.frame(id = idList, population = popList, centroids.df)

library("ggspatial")
ggplot(data = world) +
  geom_sf(fill="antiquewhite") +
  geom_sf_label(fill = "white",  # override the fill from aes()
                fun.geometry = sf::st_centroid) +
  geom_text(aes(label = pop.df$id, x = pop.df$Longitude, y = pop.df$Latitude)) +
  geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=1)+
  coord_equal()+
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = "none")+
  geom_polygon(data=CA, aes(long, lat, group=group), 
               color="black", fill="gray50") +
  geom_polygon(data=CAcounty, aes(long, lat, group=group), 
               color="gray50", fill=NA, linetype=3)+
  geom_polygon(data=EA, aes(long, lat, group=group),
               color="gray50", fill=NA, linetype=3)+
  geom_polygon(data=shpData, aes(long, lat, group=group), 
               color="black", fill= "green", linetype=3)+
  theme(panel.grid.major = element_line(color = gray(.2), 
                                        linetype = "dashed", size = 0.05), panel.background = element_rect(fill = "aliceblue"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 50), ylim = c(40, -40), expand = T)+
  ggtitle("Overview Map")

overviremap2<-ggplot(data = world) +
  geom_sf(fill="antiquewhite") +
  # geom_sf_label(fill = "white",  # override the fill from aes()
  #               fun.geometry = sf::st_centroid) +
  # geom_text(aes(label = pop.df$id, x = pop.df$Longitude, y = pop.df$Latitude)) +
  geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=1)+
  coord_equal()+
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = "none")+
  geom_polygon(data=CA, aes(long, lat, group=group), 
               color="black", fill="gray50") +
  geom_polygon(data=CAcounty, aes(long, lat, group=group), 
               color="gray50", fill=NA, linetype=3)+
  geom_polygon(data=EA, aes(long, lat, group=group),
               color="gray50", fill=NA, linetype=3)+
  geom_polygon(data=shpData, aes(long, lat, group=group), 
               color="black", fill= "green", linetype=3)+
  theme(panel.grid.major = element_line(color = gray(.2),
                                        linetype = "dashed", size = 0.05), panel.background = element_rect(fill = "aliceblue"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 50), ylim = c(40, -40), expand = T)+
  # ggtitle("Overview Map")+
  theme(plot.background =
          element_rect(fill = "white", linetype = 1,
                       size = 0.3, colour = "black"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

overviremap2


finalmap2 <- ggmap(sq_map) +
  geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=0.5)+
  labs(x="Longitude", y="Latitude")+
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # geom_polygon(aes(x = long, y = lat, group=id),
  #              data = NSR, color ="white", fill ="green",
  #              alpha = .4, size = .1)+
  # geom_polygon(data=CA, aes(long, lat, group=group), 
  #              color="black", fill="gray50") +
  # geom_polygon(data=CAcounty, aes(long, lat, group=group), 
  #              color="gray50", fill=NA, linetype=3)+
  geom_path(data = cdat, aes(x = longitude, y = latitude, color = id),
            size = 0.5, lineend = "round") +
  ggtitle("Eye in the Sky Niassa; GPS tracked Bird movements") + 
  theme(plot.title = element_text(lineheight=0.8,size=5, hjust = 0.5))+
  theme(legend.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(title.hjust=0.5))+
  scale_color_discrete(name = "BIRD ID")+
  theme_bw()+
  theme(panel.grid.major = element_line(color = "white")) +
  theme(plot.background =
          element_rect(fill = "white", linetype = 1,
                       size = 0.3, colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(legend.position= "bottom")+ #can use legend.position= "right" or "bottom"
  theme(legend.title = element_text(colour="blue", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=10))+
  theme(legend.background = element_rect(fill="white",size=0.5, 
                                         linetype="solid",colour ="darkblue"))+
  guides(color = guide_legend(override.aes = list(size=5)))+
  ggtitle("Eye in the Sky; GPS tracked Bird movements") + 
  theme(plot.title = element_text(lineheight=0.8,size=11, hjust = 0.5))+
  theme(legend.title = element_text(hjust = 0.5))+
  # guides(color=guide_legend(title.hjust=3))+
  geom_text(x = 32, y = -25, label = "Nesting site", colour="white")

finalmap2

finalmap2<-ggdraw() + 
  draw_plot(finalmap2, 0, 0, 1, 1) +
  draw_plot(overviremap2, 0.6,0.63,0.25,0.3)  #("left/right","up/down", "size of map",size of map )


# finalmap2=finalmap2+
#   guides(color = guide_legend(override.aes = list(size = 2))) + 
#   scale_color_discrete(name = "BIRD ID") # Manual legend title

finalmap2

ggsave("NSR_Vulture Map_2.pdf", width = 30, height = 20, units = "cm", dpi = 320, path=outdir)


# bbox=make_bbox(cdat$longitude, cdat$latitude)
# socalmap<-get_stamenmap( bbox = bbox,maptype = "toner-lite")
# 
# ggmap(socalmap)+
#   geom_polygon(aes(x = long, y = lat, group=group),
#                data = shpData, color ="white", fill ="green",
#                alpha = .4, size = .1)+
#   geom_path(data = cdat, aes(x = longitude, y = latitude, color = id),
#             size = 0.5, lineend = "round")


#TMAP https://r-tmap.github.io/tmap-book/nutshell.html

# dev.off()
# maps::map("worldHires","Mozambique", lwd=0.5)
# maps::map("worldHires","Tanzania", lwd=0.5,lty=3, col="gray30",add=TRUE)
# plot(shpData, col="green",add=T)

# #heatmap
# ggmap(sq_map) +
#   geom_point(data=cdat,aes(x=longitude, y=latitude, color=id), size=0.5)+
#   stat_density2d(aes(x=longitude, y=latitude, fill = ..level..,alpha=..level..), bins = 20, geom = "polygon", data = data) +
#   scale_fill_gradient(low = "black", high = "red")

# library(sf)
# NSR     <- st_read("C:/Users/GarethT/OneDrive - EWT/BOPP/R/EITS_NIASSA/data output/NSR_shapefiles/Niassa Special Reserve/NSR_Zonning_Detailed_in_Community_Tourism_Units.shp")
# wgs <- "+proj=longlat +datum=WGS84"
# NSR=st_transform(NSR, wgs)
# crs(NSR)
# NSR$NAME<- as.factor(NSR$NAME)
# plot(NSR)
# class(NSR)
# 
# data <- fortify(NSR)
# plot(data)
# 
# plot(st_transform(NSR, crs = 3857)[1], bgMap = sq_map)

# Read in South Africa shapefiles ---------------------------------------------

# #So DOY 1 is January 1st and DOY 365 is December 31st. DOY 182 will be the peak of summer which will also be the midpoint of the color ramp. In this map DOY 182 is green so summer will be the greener colors and purple/red will be winter.
# 
# ggmap(sq_map) +
#     geom_path(data = cdat, aes(x = longitude, y = latitude, color = doy, group = id), size = 0.8) +
#     labs(x = " ", y = " ", title = "Seasonal movement") +
#     theme_minimal() +
#     scale_color_gradientn("DOY", colours = rainbow(7), breaks = c(0, 100, 200, 300, 365))
# 
# # movement over time and show the year in different colors
# ggmap(sq_map) +
#     geom_path(data = cdat, aes(x = longitude, y = latitude, color = year, group = id)) +
#     labs(x = " ", y = " ", title = "Movement over time") +
#     theme_minimal() +
#     scale_color_distiller("Year", palette = "Spectral", breaks = c(1990, 2000, 2009))
# 
# # movement over time and show the year in different colors
# g=ggmap(sq_map) +
#   geom_path(data = cdat, aes(x = longitude, y = latitude, color = id)) +
#   labs(x = " ", y = " ", title = "Movement over last month")
# g
# 
# g + theme(legend.position = "none")
# 
# 
# g +
#   theme(legend.key.size = unit(5, 'cm'), #change legend key size
#         legend.key.height = unit(1, 'cm'), #change legend key height
#         legend.key.width = unit(1, 'cm'), #change legend key width
#         legend.title = element_text(size=14), #change legend title font size
#         legend.text = element_text(size=10)) #change legend text font size
# # 
# # library(ggplot2)
# # library(grid)
# # library(gridExtra)
# # library(lemon)
# # 
# # g1 <- reposition_legend(g, 'bottom', plot=TRUE)
# #   
# # 
# # # #https://cran.r-project.org/web/packages/move/vignettes/move.html
# # library(ggmap)
# # # install.packages("ggmap")
# # require(mapproj)
# # library(move)
# # # 
# # leroyDF <- as.data.frame(dataNSR)
# # df<-dataNSR
# # #Converting and aligning trajectories
# # m1 <- move(x = df[["longitude"]], y = df[["latitude"]],
# #            time = df[["timestamp"]], animal = df[["id"]],
# #            proj = "+proj=longlat +datum=WGS84 +no_defs",
# #            removeDuplicatedTimestamps = TRUE) 
# # 
# # m <- get_map(bbox(extent(m1)*1.5), source="stamen", zoom=5)
# # 
# # ggmap(m)+
# #   geom_path(data=leroyDF, aes(x=longitude, y=latitude, color=id))
# 

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}


detachAllPackages()

# remotes::install_github("ctmm-initiative/ctmmweb")

#install.packages("ctmmweb")
library(ctmmweb)
library(ctmm)
library(data.table)
library(glue)
library(magrittr)
library(glue)
library(lubridate)

data <- subset(data, timestamp > "2022-06-01")

data$id<-as.character(data$id)

data_tel <- as.telemetry(data, timezone="SAST",  projection=NULL,drop = FALSE)

# basic data structure
collected_data <- collect(data_tel)
# a list of locations data.table/data.frame and information table
loc_data <- collected_data$data
info <- collected_data$info
head(loc_data)
summary(loc_data)

library(leaflet)
heat_map(loc_data)
point_map(loc_data)

# library(htmltools)
library(leafpop)
library(shiny)
library(leaflet.extras)
library(formattable) #https://www.rdocumentation.org/packages/gt/versions/0.2.0.5
library(gt) #https://gt.rstudio.com/articles/intro-creating-gt-tables.html
library(paletteer)#https://gt.rstudio.com/reference/data_color.html
library(plyr)

{
GRID_GROUP <- "_graticule_"
# TILES_INFO hold information of tiles needed for initialization. HERE maps need api key and different init code, so they are placed in separate list items, also need to keep here api key. The layer names are also needed in layer control code in other places
TILES_INFO <- list(here = c("HERE.terrainDay", "HERE.satelliteDay",
                            "HERE.hybridDay"),
                   open = c("OpenTopoMap",
                            "Esri.WorldTopoMap", "Esri.WorldImagery"),
                   here_app_id = 'ehftALetcOLjvopsXsZP',
                   here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'
)
# build map ----

#' Build a leaflet base map
base_map <- function(tiles_info = TILES_INFO,
                     grid = TRUE) {
  leaf <- leaflet::leaflet(options = leaflet::leafletOptions(
    attributionControl = FALSE))
  for (prov in tiles_info$here) {
    leaf <- leaf %>%
      leaflet::addProviderTiles(leaflet::providers[[prov]],
                                group = prov,
                                options = leaflet::providerTileOptions(
                                  detectRetina = TRUE,
                                  app_id = tiles_info$here_app_id,
                                  app_code = tiles_info$here_app_code))
  }
  
  for (prov in tiles_info$open) {
    leaf <- leaf %>%
      leaflet::addProviderTiles(leaflet::providers[[prov]], group = prov)
  }
  if (grid) {
    leaf <- leaf %>%
      leaflet::addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                                  redraw = "moveend", group = GRID_GROUP)
  }
  return(leaf)
  
}

# note all additional augment functions need leaf as first parameter.
add_measure <- function(leaf) {
  leaf %>%
    leaflet::addMeasure(
      position = "bottomright",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = "hectares",
      activeColor = "#3D535D",
      completedColor = "#e74c3c")
}

# the layer control need to wait home range, so not added here. id_pal is color pallete function from full data set. used different parameter name specifically to hint the difference. Always use id to hint the full context since id is a factor. leaflet need a factor function to apply on id column. In comparison, home ranges are added one by one and used plain color vector.
add_points <- function(leaf, dt, name_vec, id_pal) {
  # add each individual as a layer
  # for loop is better than lapply since we don't need to use <<-
  for (current_id in name_vec) {
    leaf <- leaf %>%
      leaflet::addCircles(data = dt[identity == current_id], group = current_id,
                          lng = ~longitude, lat = ~latitude,
                          radius = 0.3, weight = 2,
                          color = ~id_pal(id),
                          opacity = 0.4, fillOpacity = 0.05)
  }
  
  leaf %>%
    leaflet::addLegend(pal = id_pal, values = name_vec,
                       position = "topleft") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    # draw with measure, but it show measure on markers
    # addDrawToolbar(targetGroup = draw_group,
    #                editOptions = editToolbarOptions(
    #                  selectedPathOptions = selectedPathOptions())) %>%
    # addMeasurePathToolbar(options =
    #                         measurePathOptions(showOnHover = FALSE,
    #                                            minPixelDistance = 100))
    # simple measure
    add_measure()
}

#' Add layer control for leaflet map
add_control <- function(leaf, layer_vec,
                        tiles_info = TILES_INFO) {
  leaf %>% leaflet::addLayersControl(
    baseGroups = c(tiles_info$here, tiles_info$open),
    overlayGroups = c(GRID_GROUP, layer_vec),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}

# home range ----
add_home_range <- function(leaf, hrange, hr_levels, hr_color, hr_name){
  hrange_spdf <- sp::spTransform(
    ctmm::SpatialPolygonsDataFrame.UD(hrange, level.UD = hr_levels),
    sp::CRS("+proj=longlat +datum=WGS84"))
  est_indice <- seq(2, length(hrange_spdf), by = 3)
  hrange_spdf_est <- hrange_spdf[est_indice, ]
  hrange_spdf_other <- hrange_spdf[-est_indice, ]
  leaf %>%
    leaflet::addPolygons(data = hrange_spdf_est, weight = 2.2, opacity = 0.7,
                         fillOpacity = 0.05, color = hr_color, group = hr_name) %>%
    leaflet::addPolygons(data = hrange_spdf_other, weight = 1.2, opacity = 0.4,
                         fillOpacity = 0.05, color = hr_color, group = hr_name)
}
# given a map object, add layers and return the map object. use simple color_vec instead of pallete function so user can customize it easier.
add_home_range_list <- function(leaf, hrange_list, hr_levels,
                                hr_color_vec) {
  hr_name_vec <- names(hrange_list)
  for (i in seq_along(hrange_list)) {
    leaf <- leaf %>% add_home_range(hrange_list[[i]], hr_levels,
                                    hr_color_vec[i], hr_name_vec[i])
  }
  return(leaf)
}
# point map ----
# exported user friendly version ends with map and don't use verb in beginning. the usage in app is already abstract enough, nothing to wrap more. For package users, things can be improved: 1. name_vec came from dt, id_pal came from full dt, so only provide two dt? that will be difficult to customize color. show them the internal usage. so it's easy to get points map with two dt, that's good. next, home range is complex, need lots of parameters, just let user define the color is easier, and keep the separated functions, the add control need to be separated, but with more control.
# decided to wrap control code inside function, so user will use 3 different functions. no step by step building but it's easier. they can use internal functions if they need more flexibility.
# the internal shared part of point map
build_point_map <- function(dt_subset) {
  full_id_vec <- levels(dt_subset$id)
  selected_id_vec <- get_names(dt_subset)
  id_pal <- leaflet::colorFactor(scales::hue_pal()(length(full_id_vec)),
                                 full_id_vec, ordered = TRUE)
  base_map() %>% add_points(dt_subset, selected_id_vec, id_pal)
}

#' Build maps of animal locations
#'
#' An interactive map will shown in RStudio Viewer pane when running in
#' interactive session. You can also further augment it with
point_map <- function(loc_data_subset) {
  # full_id_vec <- levels(loc_data$id)
  selected_id_vec <- get_names(loc_data_subset)
  # id_pal <- leaflet::colorFactor(scales::hue_pal()(length(full_id_vec)),
  #                                full_id_vec, ordered = TRUE)
  # base_map() %>% add_points(loc_data, selected_id_vec, id_pal) %>%
  #   add_control(selected_id_vec)
  build_point_map(loc_data_subset) %>% add_control(selected_id_vec)
}

range_map <- function(hrange_list, hr_levels, hr_color_vec) {
  base_map() %>%
    add_home_range_list(hrange_list, hr_levels, hr_color_vec) %>%
    add_control(names(hrange_list))
}

point_range_map <- function(loc_data_subset, hrange_list,
                            hr_levels, hr_color_vec) {
  selected_id_vec <- get_names(loc_data_subset)
  build_point_map(loc_data_subset) %>%
    add_home_range_list(hrange_list, hr_levels, hr_color_vec) %>%
    add_control(c(selected_id_vec, names(hrange_list)))
}


# heat map ----
# base map layer control added here
add_heat <- function(leaf, loc_data, tiles_info = TILES_INFO) {
  leaf %>%
    leaflet.extras::addHeatmap(data = loc_data, lng = ~longitude, lat = ~latitude,
                               blur = 8, max = 1, radius = 5, group = "Heatmap") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    add_measure() %>%
    leaflet::addLayersControl(
      baseGroups = c(tiles_info$here, tiles_info$open),
      overlayGroups = c(GRID_GROUP, "Heatmap"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
}
#' @describeIn point_map Build heat map of animal locations
#'
#' @export
heat_map <- function(loc_data_subset) {
  base_map() %>% add_heat(loc_data_subset)
}
# utilities ----
get_names <- function(loc_data) {
  unique(loc_data, by = "identity")$identity
}
# check if a reactive value is valid yet
reactive_validated <- function(reactive_value) {
  res <- try(reactive_value, silent = TRUE)
  return(!("try-error" %in% class(res)))
}
# take and return rgb strings. given a base color, create variations in different values, ordered from bright to dark.
# the usual scales::gradient_n_pal take two ends then cut between, this is taking the 1 and 0.5 brightness variation of base color as two ends, otherwise the available color space could be very small when base color is quite dark/bright
vary_color <- function(base_color, count) {
  if (count == 1) {
    return(base_color)
  } else {
    hsv_vec <- grDevices::rgb2hsv(grDevices::col2rgb(base_color))[, 1]
    return(grDevices::hsv(hsv_vec[1], hsv_vec[2], seq(1, 0.5, length.out = count)))
  }
}
get_bounds <- function(dt) {
  return(list(lng1 = min(dt$longitude), lat1 = min(dt$latitude),
              lng2 = max(dt$longitude), lat2 = max(dt$latitude)))
}
apply_bounds <- function(leaf, bounds) {
  leaflet::fitBounds(leaf, bounds$east, bounds$north, bounds$west, bounds$south)
}

# home range ----
add_home_range <- function(leaf, hrange, hr_levels, hr_color, hr_name){
  hrange_spdf <- sp::spTransform(
    ctmm::SpatialPolygonsDataFrame.UD(hrange, level.UD = hr_levels),
    sp::CRS("+proj=longlat +datum=WGS84"))
  est_indice <- seq(2, length(hrange_spdf), by = 3)
  hrange_spdf_est <- hrange_spdf[est_indice, ]
  hrange_spdf_other <- hrange_spdf[-est_indice, ]
  leaf %>%
    leaflet::addPolygons(data = hrange_spdf_est, weight = 2.2, opacity = 0.7,
                         fillOpacity = 0.05, color = hr_color, group = hr_name) %>%
    leaflet::addPolygons(data = hrange_spdf_other, weight = 1.2, opacity = 0.4,
                         fillOpacity = 0.05, color = hr_color, group = hr_name)
}
# given a map object, add layers and return the map object. use simple color_vec instead of pallete function so user can customize it easier.
add_home_range_list <- function(leaf, hrange_list, hr_levels,
                                hr_color_vec) {
  hr_name_vec <- names(hrange_list)
  for (i in seq_along(hrange_list)) {
    leaf <- leaf %>% add_home_range(hrange_list[[i]], hr_levels,
                                    hr_color_vec[i], hr_name_vec[i])
  }
  return(leaf)
}
# 
# add_home_range(loc_data)
# ctmmweb::app()
}

# heat_map(loc_data)
library(mapview)
library(leafem)

here_app_id = 'ehftALetcOLjvopsXsZP'
here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'

maptitle2<-paste("Vulture Tracking Heatmap, Niassa ",today ,sep="")
maptitle2  

title <- tags$div(
  tag.map.title, HTML(maptitle2)
)


heatmap <- leaflet(last.loc) %>% 
  addMouseCoordinates()%>%
  addMapPane("polygons", zIndex = 250) %>%
  addProviderTiles("HERE.terrainDay", group="Terrain", options = leaflet::providerTileOptions(
                                detectRetina = TRUE,
                                app_id = here_app_id,
                                app_code = here_app_code))%>%
  addProviderTiles("HERE.satelliteDay", group="Satellite", options = leaflet::providerTileOptions(
    detectRetina = TRUE,
    app_id = here_app_id,
    app_code = here_app_code))%>%
  addScaleBar(position = c("bottomright"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
  leaflet.extras::addHeatmap(data = loc_data, lng = ~longitude, lat = ~latitude,
                             blur = 8, max = 1, radius = 5, group = "Heatmap")  %>% 
  addPolygons(data=NSR, color ="green", weight = 0.5, smoothFactor = 0.5, #~palsa(NAME)
              opacity = 0.8, fillOpacity = 0.1,fillColor ="green", popup=~NAME,  label=~NAME, group="NSR Boundaries",
              highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE),
              options = pathOptions(pane = "polygons")) %>%
addLayersControl(
  baseGroups = c("Terrain", "Satellite"),
  overlayGroups = c("NSR Boundaries", "Heatmap"),
  options = layersControlOptions(collapsed = F))   %>%
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

  
heatmap

# Export as HTML file
htmlwidgets::saveWidget(
  widget = heatmap,
  file = glue("{outdir}/{maptitle2}.html"),
  selfcontained = T)


# Write leaflet map -------------------------------------------------------

saveRDS(heatmap, glue("{outdir}/{maptitle2}.rds"))

  