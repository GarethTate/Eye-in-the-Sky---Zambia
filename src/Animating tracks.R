#AniMOVE and create animation
# usethis::create_github_token()
# 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
# 
# usethis::edit_r_environ()



# install.packages("gdalUtils")
# remotes::install_github("AniMoveCourse/animove")
# library(data.table)
library(moveVis)
library(move)
# library(raster)
# library(lubridate)
library(ggplot2)
library(animove)
library(lubridate)
library(sf)
library(glue)
# remotes::install_github("gearslaboratory/gdalUtils")
# library(gdalUtils)

sessionInfo()
# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/03_NSR_tracking_data_activity.RData"))
data <- dataNSR

  
detachAllPackages <- function() {
    basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
    package.list <- setdiff(package.list,basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  }


detachAllPackages()

library(moveVis)
library(move)
# library(raster)
# library(lubridate)
library(ggplot2)
library(animove)
library(lubridate)
library(sf)
library(glue)


df<-data
  # class(df)
  # df<-as.data.table(df)
  range(df$latitude)
  range(df$longitude)
  range(df$timestamp)
  
df<-subset(df, latitude < -5)
  
  # df<-subset(df, df$longitude<40)
  # df<-subset(df, df$latitude<30)
  
  #Converting and aligning trajectories
  m1 <- move(x = df[["longitude"]], y = df[["latitude"]],
             time = df[["timestamp"]], animal = df[["id"]],
             proj = "+proj=longlat +datum=WGS84 +no_defs",
             removeDuplicatedTimestamps = TRUE) 
  
  #check both time stamps and sampling rates(temporal resolution) of the trajectories 
  lag <- unlist(timeLag(m1, unit = "mins"))
  median(lag) #This shows each trajectory sampling rate 
  sd(lag) #which is differing over time with a standard deviation of 36.86 minutes
  plot(m1)  
  
  dev.off()
  #function to align trajectories using linear interpolation, named
  
  m <- align_move(m1, res = 60, digit = 0, unit = "mins")#resolution of 180 minutes (3 hours) 
  plot(m, col="red")  
  
  # use a extent object as your AOI
  ext <- extent(m)
  ext
  ext@xmin <- ext@xmin - (ext@xmin*0.02)
  ext@xmax <- ext@xmax + (ext@xmax*0.02)
  ext@ymin <- ext@ymin - (ext@xmin*0.02)
  ext@ymax <- ext@ymax + (ext@xmax*0.02)
  ext
  
  # frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor", alpha = 0.5, ext=ext)
  #   
  # length(unique(timestamps(m)))  
  # frames[[85]]
  
  # #old code starts here
  # ext <- extent(m) * 3
  # # ext@xmin <- ext@xmin * 2
  # # ext@xmax <- ext@xmax * 2
  # ext
  
  # #ext <- extent(-20, 50, -38, 35)
  # ext <- extent(-22, 33, -26, 29)
  # class(ext)
  
  memory.limit()
  memory.limit(size=500000000000)  
  
  
  
  # framesME1 <- frames_spatial(m = m, ext=ext,
  #                             #path_colours = id,
  #                             path_size = 2, # default is 3
  #                             map_service = "carto",
  #                             map_type = "voyager_no_labels",
  #                             path_legend = FALSE) %>%
  #   add_labels(x = "Longitude", y = "Latitude",
  #              title = "Martial Eagle Movements",
  #              subtitle = subtitle) %>%
  #   add_text(labels = lbl.tstamp, x = 15, y = 30, colour = "black",
  #            size = 4, type = "label") %>%
  #   add_text(labels = "G. Tate, EWT", x = 40, y = -35, colour = "black",
  #            size = 3, type = "text") %>%
  #   add_progress()
  # 
  # length(framesME1)
  # 
  # framesME1[[43]]
  
  
  frames.ts <- sort(unique(timestamps(m)))
  # lbl.tstamp <- paste("Year:",  toupper(year(frames.ts)),
  #                     "| Month:", toupper(month(frames.ts)),
  #                     "| Day:", yday(frames.ts), sep = " ")
  
  lbl.tstamp <- paste("DATE:",  toupper(date(frames.ts)))
  
  firstdate<-df[1,]$date
  todaysdate<-now()
  #todaysdate<-format(today, format="%Y %M %d")
  
  subtitle<-paste(firstdate," to ", todaysdate, sep="")
  
  #get_maptypes()
  class(m)
  ext
  
  # data1 = data.frame(x= 30.0963, y=-25.438)
  # 
  # data2 <- data.frame(y = c(-26.34392, -26.34392),
  #                     x = c(27.93119, 32.38477))
  
  
  
  #For a polygon:
  # data <- data.frame(x = c(8.917, 8.924, 8.924, 8.916, 8.917),
  #                    y = c(47.7678, 47.7675, 47.764, 47.7646, 47.7678))
  
  #http://movevis.org/articles/example-1.html
  # $osm
  # [1] "streets"      "streets_de"   "streets_fr"   "humanitarian" "topographic"  "roads"       
  # [7] "hydda"        "hydda_base"   "hike"         "grayscale"    "no_labels"    "watercolor"  
  # [13] "toner"        "toner_bg"     "toner_lite"   "terrain"      "terrain_bg"   "mtb" 
  # 
  
framesfinal <- frames_spatial(m,  map_service = "osm", map_type = "streets_de", #map_service = "esri", map_type = "world_imagery",
                                #map_token = "sk.eyJ1IjoiZ2FyZXRoanRhdGUiLCJhIjoiY2ttdndxNHFwMDlkODJ2dXR6OTQ2aHVqdSJ9.GXoaQdV3-9QmkDLkqmH-ZQ",
                                ext = ext,
                                path_colours = m$id,
                                trace_show = TRUE,
                                path_size = 2, # default is 3
                                path_legend =F) %>%
    add_northarrow(colour = "black") %>%
    add_progress() %>%
    add_scalebar(colour = "black", x =  36.7, y = -13.25) %>%
    add_text(labels= "MARIRI CAMP", x =  38.096081, y = -12.1749, # x = 30.0963, y = -25.438
             colour = "black", size = 2)%>%
    add_text(labels= "CHUILEXI CAMP", x =  38.411763, y = -11.925, # x = 30.0963, y = -25.438 -24.616235, 30.885521
             colour = "black", size = 2)%>%
    # geom_path(aes(x = x, y = y), data = data,
    #             colour = "red", linetype = "dashed")%>%
    add_labels(x = "Longitude", y = "Latitude",
               title = "GPS tracked Raptors Niassa",
               subtitle = subtitle) %>%
    add_text(labels = lbl.tstamp, x = 38.41, y = -5.5, colour = "black", #, x = 23.5, y = -31.4 x = 29.2, y = -23.6
             size = 4, type = "label")
  
length(framesfinal)
framesfinal[[10]]
  
library(sf)
NSR     <- st_read("C:/Users/GarethT/OneDrive - EWT/BOPP/R/EITS_NIASSA/data output/NSR_shapefiles/Niassa Special Reserve/NSR_Zonning_Detailed_in_Community_Tourism_Units.shp")
wgs <- "+proj=longlat +datum=WGS84"
NSR=st_transform(NSR, wgs)
crs(NSR)
NSR$NAME<- as.factor(NSR$NAME)
  
  # frames3 = add_gg(framesfinal, gg = expr(geom_sf(data = sa_PAs, aes(group = "ID"),show.legend="ID" ,
  #                                             colour =  "black")) , data = sa_PAs) 
  
  frames4 = add_gg(framesfinal, gg = expr(geom_sf(data = NSR, aes(group = "NAME"),show.legend="NAME",fill="green",
                                                  colour =  "black", alpha = 0.1)) , data = NSR)  %>%
    add_text(labels= "MARIRI CAMP", x =  38.096081, y = -12.1749, # x = 30.0963, y = -25.438
             colour = "black", size = 2)%>%
    add_text(labels= "CHUILEXI CAMP", x =  38.411763, y = -11.925, # x = 30.0963, y = -25.438 -24.616235, 30.885521
             colour = "black", size = 2)
  
  
  # framesfinal2[[10]]
  #frames3[[10]]
frames4[[10]]
  
  # setwd("C:/Users/GarethT/OneDrive - The EWT/BOPP/DATA/GPS/Eagles")
  # animate_frames(frames2, out_file = "ME 307 2019-2020.gif", overwrite = TRUE,fps = 5) 
  # ?animate_frames
  #locator(1)
  #cropbox1 <- drawExtent()
  
animate_frames(frames4, out_file = "C:/Users/GarethT/OneDrive - EWT/BOPP/R/Raptors_Niassa_animation_Aug22.mp4", overwrite = T) #.mov
  
}
