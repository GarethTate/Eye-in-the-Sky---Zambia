# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
today <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/04_NSR_mapping_data.RData"))

library(KernSmooth)  

data<-as.data.table(data)

kde <- bkde2D(data[ , list(data$longitude, data$latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(5000,5000)) #bigger grid gives smoother kernels

CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
# leaflet(spgons) %>% addTiles() %>% 
#   addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

## Leaflet map with points and polygons
## Note, this shows some problems with the KDE, in my opinion...
## For example there seems to be a hot spot at the intersection of Mayfield and
## Fillmore, but it's not getting picked up.  Maybe a smaller bw is a good idea?
# leaflet(spgons) %>% addTiles() %>%
#   addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
#   addCircles(lng = data$longitude, lat = data$latitude,
#              radius = .5, opacity = .2, col = "blue")


## Leaflet map with polygons, using Spatial Data Frame
## Initially I thought that the data frame structure was necessary
## This seems to give the same results, but maybe there are some 
## advantages to using the data.frame, e.g. for adding more columns
spgonsdf = SpatialPolygonsDataFrame(Sr = spgons,
                                    data = data.frame(level = LEVS),
                                    match.ID = TRUE)
# leaflet() %>% addTiles() %>%
#   addPolygons(data = spgonsdf,
#               color = heat.colors(NLEV, NULL)[spgonsdf@data$level])


# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))


#can also convert the output of bkde2D into a raster rather than contour lines, using the fhat values as the raster cell values
#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)


## Leaflet map with raster
# leaflet() %>% addTiles() %>% 
#   addRasterImage(KernelDensityRaster, 
#                  colors = palRaster, 
#                  opacity = .8) %>%
#   addLegend(pal = palRaster, 
#             values = KernelDensityRaster@data@values, 
#             title = "Kernel Density of Points")
# 
#set low density cells as NA so we can make them transparent with the colorNumeric function
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

# ## Redraw the map
# leaflet() %>% addTiles() %>%
#   addRasterImage(KernelDensityRaster,
#                  colors = palRaster,
#                  opacity = .8) %>%
#   addLegend(pal = palRaster,
#             values = KernelDensityRaster@data@values,
#             title = "Kernel Density of Points")

palRaster2 <- colorBin("Spectral", bins = 8, domain = KernelDensityRaster@data@values, na.color = "transparent")

## Leaflet map with raster
area_use=leaflet() %>% addTiles()%>%
  addTiles(group = "OSM (default)") %>% #Esri.WorldImagery
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # addCircles(data=data, ~longitude, ~latitude,color = ~pal(id),
  #            popup = ~as.character(timestamp()), label = ~as.character(timestamp), group="raptor Fixes")%>%
  addRasterImage(KernelDensityRaster,
                 colors = palRaster,
                 opacity = .8, group="Kernel Densities") %>%
  addLegend(pal = palRaster,
            values = KernelDensityRaster@data@values,
            title = "Kernel Density of Points")%>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("raptor Fixes", "Kernel Densities"),
    options = layersControlOptions(collapsed = FALSE))

area_use

#now run occurrence range code
#--------------------------------------------------------------------------------------------------------------
prj <- "+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
prjCRS <- CRS(prj)
wgs <- "+proj=longlat +datum=WGS84"
wgsCRS <- CRS("+proj=longlat +datum=WGS84")
utm.prj <- "+proj=utm +zone=36 + ellps=WGS84"

#--------------------------------------------------------------------------------------------------------------
# study_area  <- spTransform(readOGR("C:/Users/GarethT/OneDrive - EWT/BOPP/GIS/Eskom power lines/Eskom_Transmission_Lines.shp"), wgsCRS)
# HR<-spTransform(readOGR("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/raptors/homerangeME.shp"), wgsCRS)
# linesraster <- raster("C:/Users/GarethT/OneDrive - EWT/BOPP/GIS/lines_Raster_1.tif")

NSR_admin <- spTransform(readOGR("data input/NSR_shapefiles/Niassa Special Reserve/Buffer_Zone.shp"), wgs)
names(NSR_admin)
head(NSR)
NSR_admin$NAME <- as.factor(NSR_admin$NAME)
NSR$NAME<-as.factor(NSR$NAME)

data=dataNSR
ddply(data, c("id"), summarise,n=length(id))
ddply(dataNSR, c("id"), summarise,n=length(id))
ddply(data, c("age"), summarise,n=length(id))
paste("The dataset contains: " , nrow(data), " locations",sep="")
paste("The dataset contains: ", length(unique(data$id)), " individual/s",sep="")

library(lubridate)
library(sp)
library(tidyverse)
#library(maps)
#library(mapdata)
library(plyr)
library(data.table)
library(adehabitatHR)
library(raster)
library(sf)
library(rgdal)
library(leaflet)
library(magrittr)
library(sp)
library(rgdal)
library(mapdata)
library(mapview)

ddply(data, c("id"), summarise,n=length(id))

data$id<-as.factor(data$id)

prj <- '+init=epsg:26911'

spdf <- SpatialPointsDataFrame(coordinates(cbind(data$longitude, data$latitude)),
                               data = data, proj4string = CRS(prj))

unique(spdf$id)

# head(spdf)
# crs(spdf) 
# plot(spdf, col=as.factor(spdf$id))
# map.axes()

#quick view plot
{
  # datamap <-data
  # datamap$id<-as.factor(datamap$id)
  # coordinates (datamap)= c("longitude","latitude")
  # #map('world2Hires', xlim=c(-20, 45), ylim=c(-38,48), col = "gray40")
  # maps::map("worldHires","South Africa", xlim=c(15,35),ylim=c(-35,-20), lwd=0.5)
  # map("worldHires","Botswana",lwd=0.5, add=T)
  # map("worldHires","Mozambique", add=T)
  # map("worldHires","Namibia", add=T)
  # map("worldHires","Angola", add=T)
  # sp::plot(datamap, pch =19, add=TRUE, cex=0.5, col=datamap$id)
  # #lines(datamap$x, datamap$y, col="grey", lty=10)
  # map.axes()
}

#kernel densities
# # convert data to a SpatialPointsDataFrame object
# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(data$longitude, -data$latitude), proj4string = CRS("+proj=longlat"))
cord.dec
# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748 + zone=36"))
cord.UTM

# Plotting points
#par(mfrow = c(1, 2))
#plot(cord.dec, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
#plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)  

data$id<-as.factor(data$id)

xy.dec <- SpatialPointsDataFrame(coordinates(cbind(data$longitude, data$latitude)),data = data, proj4string = CRS("+proj=longlat +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
xy.dec

xy.UTM <- spTransform(xy.dec, CRS("+init=epsg:32748 + zone=36"))
xy.UTM

# Plotting points
#par(mfrow = c(1, 2))
#plot(xy.dec, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
#plot(xy.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)  

#run quick kernel density:
library(adehabitatHR)
require(ggmap)

#set grid:
#options(warn=-1) #turn off warning messages so they're not printed in the pdf
xy.UTM$id<-as.character(xy.UTM$id)
head(xy.UTM)
class(xy.UTM$id)

unique(xy.UTM$id)
xy.UTM[,1]

xy.dec$id<-as.character(xy.dec$id)

kernel.all=kernelUD(xy.UTM[,1], grid=500, h="href") #use xy.dec to plot as latlong use xy.UTM for accurate HR

#image(kernel.all)
kernel.all95 <- getverticeshr(kernel.all, percent = 95, unout = c("km2"))
kernel.all75 <- getverticeshr(kernel.all, percent = 75, unout = c("km2"))
kernel.all50 <- getverticeshr(kernel.all, percent = 50, unout = c("km2"))
kernel.all10 <- getverticeshr(kernel.all, percent = 10, unout = c("km2"))

crs(kernel.all95)
class(kernel.all95)

#kernel.all95sp <- spTransform(kernel.all95, CRS("+proj=longlat"))
#crs(kernel.all95sp)


kernel.all95<- spTransform(kernel.all95, CRS("+proj=longlat"))
kernel.all50<- spTransform(kernel.all50, CRS("+proj=longlat"))
kernel.all10<- spTransform(kernel.all10, CRS("+proj=longlat"))


# kernel.all75 
# kernel.all50
# kernel.all10
# 
# as.data.frame(kernel.all95)
# as.data.frame(kernel.all10)

KUD95<-as.data.frame(kernel.all95)
KUD95$KUD95.area= KUD95$area
KUD95[c("id","KUD95.area")]

KUD75<-as.data.frame(kernel.all75)
KUD75$KUD75.area= KUD75$area

KUD50<-as.data.frame(kernel.all50)
KUD50$KUD50.area= KUD50$area

KUD10<-as.data.frame(kernel.all10)
KUD10$KUD10.area= KUD10$area


ME_Homeranges<-cbind(KUD95[c("id","KUD95.area")],KUD75[3], KUD50[3], KUD10[3])
ME_Homeranges$id<-as.factor(ME_Homeranges$id)

ME_Homeranges<-as.data.table(ME_Homeranges)
ME_Homeranges
names(ME_Homeranges)

ME_Homeranges
class(ME_Homeranges)

#convert to spatialpoints

ddply(data, c("id"), summarise,n=length(id))
ddply(ME_Homeranges, c("id"), summarise,n=length(id))


m1 <-
  data %>% 
  group_by(id) %>% 
  filter(row_number()==1)

m1<-as.data.frame(m1)


m1 <- m1[order(m1$id),]
m1

ME_Homeranges <- ME_Homeranges[order(ME_Homeranges$id),]

ME_Homeranges$namesexage=ME_Homeranges$id

ME_Homeranges_2<-cbind(ME_Homeranges,m1[2], m1[29],m1[30])
glimpse(ME_Homeranges_2)

#adding days monitored:
yearago=as.POSIXct(paste(Sys.Date()-365, "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
maxdate=as.POSIXct(paste(Sys.Date(), "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")

ME_Homeranges_2$daystagged<-round(as.POSIXct(paste(Sys.Date(), "00:00:01"), format="%Y-%m-%d %H:%M:%S", tz="UTC")-as.POSIXct(paste(ME_Homeranges_2$timestamp), format="%Y-%m-%d %H:%M:%S", tz="UTC"),0)
ME_Homeranges_2

fixes<-ddply(data, c("id", "sex", "age"), summarise,n=length(id))


ME_Homeranges_2<-cbind(ME_Homeranges_2,fixes)
ME_Homeranges_2

ME_Homeranges_2 <- ME_Homeranges_2[order(ME_Homeranges_2$timestamp),]
ME_Homeranges_2

ME_Homeranges_2$Date_Tagged<-ME_Homeranges_2$timestamp
names(ME_Homeranges_2)

ME_Homeranges_2<-subset(ME_Homeranges_2, select=c("id","sex", "age", "Date_Tagged","daystagged", "n" , "KUD95.area","KUD75.area" ,"KUD50.area" ,"KUD10.area" ))

# setwd("C:/Users/GarethT/OneDrive - EWT/BOPP/DATA/GPS/raptors/Martial raptor")
# write.csv(ME_Homeranges_2, file="ME_Home_ranges20220315.csv")
# writeOGR(kernel.all95, dsn = '.', layer = 'homerangeME_29Mar22', driver = "ESRI Shapefile")
KME_OR=as_tibble(ME_Homeranges_2)

KME_OR

KME_OR %>%
  write_csv(glue("{outdir}/04_NSR_Occurrence_Distribution_KDE_{format(today)}.csv"))

names(KME_OR)
KME_OR$KUD95.area<-round((KME_OR$KUD95.area),0)
KME_OR$KUD75.area<-round((KME_OR$KUD75.area),0)
KME_OR$KUD50.area<-round((KME_OR$KUD50.area),0)
KME_OR$KUD10.area<-round((KME_OR$KUD10.area),0)

library(gt)
tbl <- gt(data = KME_OR)
tbl

# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
tbl <-
  tbl %>%
  tab_header(
    title = md("NSR Raptor Tracking"),
    subtitle = paste("Occurrence_Distribution", today)
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

tbl

tbl <-
  tbl%>%
  cols_label(
    id = md("raptor Name"),
    KUD95.area = md("Current Home range (sq km)"),
    KUD75.area = md("75% area use (sq km)"),
    KUD50.area = md("50% core area use (sq km)"),
    KUD10.area = md("10% core area use (sq km)")
    # sex = md("sex "),
    # age = md("age Class"),
    #max_speed = md("Max Speed recorded (km/h)"),
    #totdistkms = md("Total distance traveled over month")
  )

# 
# tbl <-
#   tbl%>%
#   # fmt_number( # A column (numeric data)
#   #   columns = last_tag_voltage, # What column variable? BOD$Time
#   #   decimals = 1
#   # ) %>% 
#   fmt_number( # Another column (also numeric data)
#     columns = weeks_tagged, # What column variable? BOD$demand
#     decimals = 0 
#   )
# #%>% 
# # fmt_number( # Another column (also numeric data)
# #   columns = nest_location_x, # What column variable? BOD$demand
# #   decimals = 2 
# # )%>% 
# #   fmt_number( # Another column (also numeric data)
# #     columns = nest_location_y, # What column variable? BOD$demand
# #     decimals = 2
# #   )
# 
# tbl 

# tbl <-
#   tbl%>%
#   tab_style(
#     style = cell_text(color = "red", weight = "bold"),
#     locations = cells_body(
#       columns = Device_status,
#       rows = Device_status == "offline"
#     )
#   ) %>% 
#   tab_style(
#     style = cell_text(color = "green", weight = "bold"),
#     locations = cells_body(
#       columns = Device_status,
#       rows = Device_status == "online"
#     )
#   )
tbl 


title<- paste("NSR Raptors Occurrence Distribution Table", " ", today, ".png", sep="")
title

tbl %>%
  gtsave(
    title, expand = 1,vwidth = 2000, vheight = 1000,
    path = {outdir}
  )

# Metric summaries --------------------------------------------------------
summary_HRdataage <- KME_OR %>%
  group_by(id) %>%
  summarise(
    n = length(id),
    min_d = min(KUD95.area),
    max_d = max(KUD95.area),
    ave_d = mean(KUD95.area),
    sd_d = sd(KUD95.area),
    tot_d = sum(na.omit(KUD95.area))
  )

summary_HRdataage

# Metric summaries --------------------------------------------------------
# summary_HRdatasex <- KME_OR %>%
#   group_by(sex) %>%
#   summarise(
#     n = length(id),
#     min_d = min(KUD95.area),
#     max_d = max(KUD95.area),
#     ave_d = mean(KUD95.area),
#     sd_d = sd(KUD95.area),
#     tot_d = sum(na.omit(KUD95.area))
#   )
# 
# summary_HRdatasex


spdf

spdf$id<-as.character(spdf$id) 
names(spdf)
#kd <- kernelUD(spdf[, 1])#this is for id
kd <- kernelUD(spdf[, 1])#this is for id
kd=kernel.all

homerangeME<-getverticeshr(kd, percent = 95,  extent=0.8)

# creating SpatialPolygonsDataFrame
kd_names <- names(kd)
ud <- lapply(kd, function(x) try(getverticeshr(x, 90)))
# changing each polygons id to the species name for rbind call

sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- kd_names[i]
})

sdf_poly <- Reduce(rbind, ud)
#sp::plot(sdf_poly)

#Visualizing with ggplot

df <- fortify(sdf_poly)

g <- ggplot(df, aes(x = long, y = lat, fill = id, group = group)) +
  geom_polygon(alpha = .4) +
  ggthemes::scale_fill_gdocs() +
  coord_equal() +
  theme_void()
g

#maps::map('world2Hires', xlim=c(15, 34), ylim=c(-36,-19), col = "gray40")


#g + facet_wrap(~id)

# Visualizing in Leaflet
# first convert to longitude and latitude

library(htmltools)
library(htmlwidgets)


sdf_poly <- spTransform(sdf_poly, CRS('+init=epsg:4326'))
leaflet(sdf_poly) %>% addTiles() %>%
  addPolygons()
# # adding colors and legends
# 
sdf_poly$id <- kd_names
fct <- factor(kd_names)

#PLOTTING LAST LOCATIONS AND KERNEL DENSITIE
# 
# pal <- colorFactor(c("blue", "red", "green", "yellow", "purple", "brown", "grey", "pink", "orange", "black", ""),
#                    domain = unique(kernel.all95$id))
class(kernel.all95$id)
kernel.all95$id<-as.factor(kernel.all95$id)



pal <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "wheat3","forestgreen"),
                   domain = unique(kernel.all95$id))

pal2 <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "wheat3","forestgreen"),
                    domain = unique(kernel.all50$id))

pal3 <- colorFactor(c("blue", "red", "green", "yellow", "purple", "coral", "grey", "cadetblue1", "orange", "black", "wheat3","forestgreen"),
                    domain = unique(kernel.all10$id))
# 
# leaflet(data)%>%addTiles()%>%
#   addProviderTiles("Esri.WorldImagery")%>% #HERE.satelliteDay
#   addPolygons(data=vkde_points50, weight = 1, fillOpacity = 0.5, color = ~pal(id), group="vkde_points50")%>%
#   addPolygons(data=vkde_points75, weight = 1, fillOpacity = 0.5, color = ~pal(id), group="vkde_points75")%>%#this is from my homerange analysis from the ME script
#   addCircles(~longitude, ~latitude,color = ~pal(id), popup = ~as.character(timestamp()), label = ~timestamp)%>%
#   #addMarkers(clusterOptions = markerClusterOptions())
#   addLayersControl(overlayGroups = c("vkde_points50", "vkde_points75"), options = layersControlOptions(collapsed = T))
here_app_id = 'ehftALetcOLjvopsXsZP'
here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'


map=leaflet(data) %>%
  addMouseCoordinates() %>%
  #addTiles(group = "OSM (default)") %>% #Esri.WorldImagery
  addProviderTiles("HERE.terrainDay", group="Terrain", options = leaflet::providerTileOptions(
    detectRetina = TRUE,
    app_id = here_app_id,
    app_code = here_app_code))%>%
  addProviderTiles("HERE.satelliteDay", group="Satellite", options = leaflet::providerTileOptions(
    detectRetina = TRUE,
    app_id = here_app_id,
    app_code = here_app_code))%>%
  addLegend("topleft", pal = pal, values = ~kernel.all10$id, title = "Individual id", labFormat = labelFormat(prefix = " "), opacity = 1, group="LEGEND")%>% 
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions(maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE))%>%
  addMeasure(position = "bottomleft", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>% setView(0,0,3)%>%
  #     addDrawToolbar(
  #   targetGroup = "draw",
  #   editOptions = editToolbarOptions(
  #     selectedPathOptions = selectedPathOptions()
  #   )
  # )  %>%
  #addStyleEditor()%>% 
  addPolygons(data=NSR, color ="red", weight = 0.5, smoothFactor = 0.5, #~palsa(NAME)
              opacity = 0.8, fillOpacity = 0.05,fillColor ="green", popup=~NAME, group="NSR_area",highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE))%>%
  addRasterImage(KernelDensityRaster, colors = palRaster, opacity =0.8, group="Area usage") %>% #only add this once youve run the Kernel density analysis
    addPolygons(data=kernel.all95,
              color =pal(kernel.all95$id),# "purple4",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              label=~paste0("KDE95: ",area,"sqkm"),
              fillOpacity = 0.2, group=~as.character(id),
              popup = popupTable(kernel.all95,
                                 zcol = c("id", "area"),
                                 feature.id = FALSE,
                                 row.numbers = FALSE))%>%
  #HOW TO USE HTML TABLE:
  #https://cran.r-project.org/web/packages/htmlTable/vignettes/general.html
  # popup = ~paste0('<h3>HOMERANGE DETAILS</h3>','<table>',
  #                 '<b>raptor ID</b>: ', id, '<br>',
  #                 '<b>Homerange size (km^2)</b>: ', area), ) %>%
  #label = as.character(kernel.all95$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1)) %>%
  addPolygons(data=kernel.all50,
              color =pal2(kernel.all50$id),# "purple4",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              label=~paste0("KDE50: ",area,"sqkm"),
              fillOpacity = 0.5, group=~as.character(id))%>%
  #label = as.character(kernel.all50$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1)) %>%
  addPolygons(data=kernel.all10,
              color =pal3(kernel.all10$id),# "purple4",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              label=~paste0("KDE10: ",area,"sqkm"),
              fillOpacity = 1, group=~as.character(id))#%>%
#label = as.character(kernel.all10$id),labelOptions = leaflet::labelOptions(noHide = TRUE,direction = "bottom", textsize = "12px", textOnly = TRUE,offset = c(0, -5),  opacity = 1)) %>%
#add
#addCircleMarkers(data=last.loc, color = ~pal(id), group=~as.character(id), radius=5,stroke = FALSE, label=~timestamp, popup=~coords, fillOpacity = 1,  ~longitude, ~latitude)#%>
# addPolygons(data=kernel.all75,
#             color = "orange",
#             weight = 1,
#             smoothFactor = 0.5,
#             opacity = 0,
#             fillOpacity = 0.3) %>%
# addPolygons(data=kernel.all50,
#             color = "yellow",
#             weight = 1,
#             smoothFactor = 0.5,
#             opacity = 0,
#             fillOpacity = 0.3) %>%
#addScaleBar(options = list(imperial = FALSE)) 

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

today<-format(today, format="%d %B %Y")

maptitle<-paste("NSR Raptors Occurrence Range ", today ,sep="")
maptitle  

title <- tags$div(
  tag.map.title, HTML(maptitle)
)

HRmap<-map%>%
  # setView(22.65073,-28, zoom = 6) %>% 
  addLayersControl(
    baseGroups = c("Terrain", "Satellite"),
    overlayGroups = c("LEGEND",  "NSR_area", "Area usage", c(unique(as.character(kernel.all95$id)))),
    options = layersControlOptions(collapsed = F))%>%
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

HRmap

HRmap=HRmap%>% 
  hideGroup("LEGEND")%>% 
  hideGroup("Bateleur_8887")%>% 
  hideGroup("Bateleur_8889")%>% 
  hideGroup("White_headed_8888")%>% 
  hideGroup("WBV_NSR_4205")%>% 
  hideGroup("WBV_NSR_4245")%>% 
  hideGroup("WBV_NSR_4247")%>%
  hideGroup("WBV_NSR_4255")%>%
  hideGroup("WBV_NSR_4257")%>%
  hideGroup("WBV_NSR_4260")%>%
  hideGroup("WBV_NSR_4261")%>%
  hideGroup("WBV_NSR_4262")%>%
  hideGroup("WBV_NSR_4263")%>%
  hideGroup("WBV_NSR_4264")%>%
  hideGroup("WBV_NSR_4265")%>%
  hideGroup("WBV_NSR_4266")%>%
  hideGroup("WBV_NSR_4267")%>%
  hideGroup("WBV_NSR_4268")%>%
  hideGroup("WBV_NSR_4269")%>%
  hideGroup("WBV_NSR_4270")%>%
  hideGroup("WBV_NSR_4271")%>%
  hideGroup("WBV_NSR_4272")%>%
  hideGroup("WBV_NSR_4273")%>%
  hideGroup("WBV_NSR_4274")

HRmap

#Export as HTML file
htmlwidgets::saveWidget(
  widget = HRmap,
  file = glue("{outdir}/{maptitle}.html"),
  selfcontained = T)


# Write leaflet map -------------------------------------------------------

saveRDS(HRmap, glue("{outdir}/{maptitle}.rds"))

# Save workspaces ---------------------------------------------------------
save.image(glue("{outdir}/06_NSR_OcurrenceRange_data.RData"))

save(file = glue("{outdir}/07_NSR_OR_shiny_global_data.RData"),
     list = c("ME_Homeranges_2","HRmap","area_use","NSR","kernel.all", "kernel.all95", "kernel.all50", "kernel.all10"))

# END ---------------------------------------------------------------------


