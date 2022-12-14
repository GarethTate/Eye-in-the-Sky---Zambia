# if (!require("remotes")) install.packages("remotes")

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

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/04_NSR_mapping_data.RData"))

# LEAFLET MAP 1 -----------------------------------------------------------
#source("src/Helper Functions.R")

head(data$timestamp)
range(data$timestamp)
data <- subset(data, timestamp > "2022-06-01")

data$id<-as.character(data$id)

data_tel <- as.telemetry(data, timezone="SAST",  projection=NULL,drop = FALSE)
#data_tel <- as.telemetry(data,timezone="UTC", projection=NULL,   drop = FALSE)

class(data_tel)
names(data_tel)
plot(data_tel, col=rainbow(length(data_tel)))
head(data_tel)

#conflicted::conflict_prefer("collect", "ctmmweb")
#conflicted::conflict_prefer("collect", "dplyr")



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
# range_map(loc_data)

range(loc_data$timestamp)

# sampling time
plot_time(loc_data)

#HEAT Map -  manually 
#https://rdrr.io/github/ctmm-initiative/ctmmweb/src/R/10_map.R

# CONFIG ----
# the layer name of graticule, it need to be consistent across functions. so making it parameter will not guarantee consistency. It's less important so using global is OK.
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


heat_map(data)
# point_map(data) 
range_map(data)


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


heatmap=heat_map(loc_data)
heatmap




















# 
# p2<-plot_time(loc_data)
# 
# knitr::kable(head(loc_data))
# 
# class(data)
# data=as.data.table(data)
# 
# assign_distance(loc_data, data)
# knitr::kable(head(loc_data))
# 
# assign_speed(loc_data, data)
# knitr::kable(head(loc_data))
# 
# # Or you can use pipe operator
# class(data)
# loc_data %>% assign_distance(data) %>% ctmmweb::assign_speed(data)
# knitr::kable(head(loc_data))
# knitr::kable(info)
# range(loc_data$distance_center_raw)
# 
# #Selecting a subset
# # select by identity column
# loc_data_sub1 <- loc_data[identity %in% c("ME_314", "ME_307")]
# # select by id factor column value
# loc_data[as.numeric(id) %in% c(1, 3)]
# #
# # plot animal locations
# plot_loc(loc_data)
# 
# # take the ggplot2 object to further customize it
# plot_loc(loc_data) +
#   ggplot2::ggtitle("Locations of Eagles") +
#   # override the default left alignment of title and make it bigger
#   ctmmweb:::CENTER_TITLE
# 
# 
# vario_list <- lapply(data, ctmm::variogram)
# # names of vario_list are needed for figure titles
# names(vario_list) <- names(data)
# plot_vario(vario_list)
# 
# 
# # sometimes the default figure settings doesn't work in some systems, you can clear the plot device then use a smaller title size
# # dev.off()
# # plot_vario(vario_list, cex = 0.55)
# 
# guess_list <- lapply(data,
#                      function(tele)
#                        ctmm::ctmm.guess(tele, interactive = FALSE))
# plot_vario(vario_list, guess_list)
# 
# 
# 
# # calculate home range with ctmm::akde. 
# tele_list_sub2 <- data[id$identity]
