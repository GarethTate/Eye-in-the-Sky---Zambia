# # Libraries
# library(conflicted)
# library(maps)
# conflicted::conflict_prefer("map", "purrr")
# library(mapdata)
# library(raster)
# library(tidyverse)
# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("lag", "dplyr")
# conflicted::conflict_prefer("select", "dplyr")
# conflicted::conflict_prefer("summarise", "dplyr")
# library(lubridate)
# library(sp)
# library(rgdal)
# library(geosphere)
# library(dismo)
# library(rgeos)
# library(sf)
# library(recurse)
# library(plotly)
# library(glue)
# conflicted::conflict_prefer("layout", "plotly")
# conflicted::conflict_prefer("collapse", "dplyr")
# library(leaflet)
# # Check
# conflicted::conflict_scout()
conflicted::conflict_prefer("summarize", "plyr")
conflicted::conflict_prefer("summarize", "dplyr")

## ________________________________________________________________________

# Read in data ------------------------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")
todaysdate <- format(todaysdate, format = "%Y-%m")
outdir <- glue("data output/{todaysdate}")

load(glue("{outdir}/03_NSR_tracking_data_activity.RData"))

# Filter data by movement -------------------------------------------------
data <- dataNSR %>%
  filter(movement == "moving") %>%
  arrange(id, timestamp)

range(data$timestamp)

# Create timeframe data subsets -------------------------------------------
todaysdate <- now(tzone = "Africa/Johannesburg")

## Function to split data
create_timeframe <- function(x) {
  data %>%
    filter(timestamp >= ymd_hms(todaysdate) - days(x)) %>%
    arrange(id, timestamp)
}

## Vector of timeframe splits
days_vec <- c(7, 10, 14, 15, 31, 180, 365)

## Map function
timeframe_data <- map(
  .x = days_vec,
  .f = create_timeframe
)

## Name elements in list
names(timeframe_data) <- glue::glue("dataNSR_{days_vec}days")

## Write each element as an object in R session
for (i in seq_along(timeframe_data)) {
  assign(names(timeframe_data)[i], timeframe_data[[i]])
}


# Filter day fixes --------------------------------------------------------
data <- data %>%
  filter(dayNight == "day") %>%
  arrange(id, timestamp)


# Filter most recent 7 days of data ---------------------------------------
obs_days <- 8
todaysdate <- now(tzone = "Africa/Johannesburg")
oneweek <- ymd_hms(todaysdate) - days(obs_days)

# NOTE From here onward the data_week object is used instead of data object
data_week <- data %>%
  filter(timestamp >= oneweek)

## Checks
head(data_week$timestamp)
range(data_week$timestamp)
range(dataNSR$timestamp)

## Create a week data SpatialPointsDataFrame object
xy_week <- SpatialPointsDataFrame(coordinates(
  cbind(data_week$longitude, data_week$latitude)
),
data = data_week,
proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
)

## Ungroup
data_week <- data_week %>%
  ungroup

# NOTE From here onward the xy_week object is used instead of xy object
xy_week

## Checks
head(xy_week)
raster::extent(xy_week)

# Clustering for latest week points ---------------------------------------

## Use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy_week)

## Cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method = "complete")

## Define the distance threshold in meter
d <- 800

## Define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy_week$clust <- cutree(hc, h = d)

## Checks
names(xy_week)
head(as.data.frame(xy_week))
range(xy_week$clust)
xy_week

# Plot latest week clusters -----------------------------------------------

## Expand the extent of plotting frame
xy_week@bbox[] <- as.matrix(extend(extent(xy_week), 0.001))

## Get the centroid coords for each cluster
cent <- matrix(ncol = 2, nrow = max(xy_week$clust))
for (i in 1:max(xy_week$clust)) {
  # gCentroid from the rgeos package
  cent[i, ] <- gCentroid(subset(xy_week, clust == i))@coords
}

## Compute circles around the centroid coords using a 500m radius
ci <- circles(cent, d = d, lonlat = T)

## Plot
jpeg(glue("{outdir}/01_week_clusters_{format(todaysdate, '%Y-%m-%d')}.jpg"),
     width = 1600, height = 900
)
maps::map("worldHires", "South Africa", lwd = 0.5)
plot(ci@polygons, axes = T, add = T)
plot(xy_week, col = rainbow(500, alpha = 0.9)[factor(xy_week$clust)], add = T, pch = 1)
dev.off()

# Extract clusters with more than 5 points -------------------------------

# NOTE From here onward the week_data object is used instead of data.df object
week_data <- as_tibble(xy_week)


## Create cluster code
week_data$clust_code <- paste("id", week_data$clust, sep = "_")
glimpse(week_data)

## Check the number of clusters within each code
week_data %>%
  group_by(clust_code) %>%
  tally() %>%
  print(n = 50)

## Create vector of clust_codes that have more than 5 instances
# library(plyr)
splst <- week_data %>%
  group_by(clust_code) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  filter(n >= 4) %>% #changed from 5
  pull(clust_code) # NB indicate the numbers of fixes you want at each cluster!!

## Subset the week data by splst variable
week_data_sub <- week_data %>%
  filter(clust_code %in% splst) %>%
  droplevels()

## Check the number of points in each cluster
week_data_sub %>%
  group_by(clust_code) %>%
  tally() %>%
  print(n = 50)

# Filter week data by elevation and distance ------------------------------

## Select change in elevation that indicate feeding events (i.e.< 5)
week_data_sub <- week_data_sub %>%
  filter(change_in_ele <= 10) %>% #changed from 5
  arrange(id, timestamp)

## Extract low distances travelled = feeding/roosting
week_data_sub <- week_data_sub %>%
  filter(dist_move_m <= 500) #changed from 180

## Checks
range(week_data_sub$speed_km_h)
range(week_data_sub$change_in_ele)
range(week_data_sub$dist_move_m)
range(week_data_sub$GPSSpeed)
plyr::ddply(week_data_sub, c("clust"), summarise, n = length(clust))

# Add row variable and unique cluster ID ----------------------------------
week_data_sub <- week_data_sub %>%
  mutate(rows = 1:nrow(.))

week_data_sub$UniqueClusterID <- paste("CLUSTID", week_data_sub$clust, sep = "_") 

# week_data_sub%>%
#   mutate(UniqueClusterID = paste("CLUSTID", 1:nrow(.), sep = "_")) %>%
#   mutate(UniqueClusterID = gsub("-", "", UniqueClusterID))

## Checks
plyr::ddply(week_data_sub, c("UniqueClusterID"), summarise, n = length(UniqueClusterID))

# Create a subset week data SpatialPointsDataFrame object -----------------

# NOTE The xy_week data have been subset which is why we need new xy SPDF
# NOTE From here onward the xy_week_sub object is used instead of x_y object

xy_week_sub <- SpatialPointsDataFrame(coordinates(cbind(week_data_sub$longitude,
                                                        week_data_sub$latitude)),
                                      data = week_data_sub,
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
)
xy_week_sub
extent(xy_week_sub)

# Clustering of latest week subset data -----------------------------------

## Use the distm function to generate a geodesic distance matrix in meters
mdist_sub <- distm(xy_week_sub)

## Cluster all points using a hierarchical clustering approach
hc_sub <- hclust(as.dist(mdist_sub), method = "complete")

## Define the distance threshold in m
d <- 800

## Define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy_week_sub$clust <- cutree(hc_sub, h = d)

## Checks
names(xy_week_sub)
head(as.data.frame(xy_week_sub))
range(xy_week_sub$clust)
xy_week_sub

# expand the extent of plotting frame
xy_week_sub@bbox[] <- as.matrix(extend(extent(xy_week_sub), 0.001))

# get the centroid coords for each cluster
cent_sub <- matrix(ncol = 2, nrow = max(xy_week_sub$clust))
for (i in 1:max(xy_week_sub$clust)) {
  # gCentroid from the rgeos package
  cent_sub[i, ] <- gCentroid(subset(xy_week_sub, clust == i))@coords
}

## Compute circles around the centroid coords using a 500m radius
ci_sub <- circles(cent_sub, d = d, lonlat = TRUE)

## Plot
jpeg(glue("{outdir}/02_week_clusters_subset_{format(todaysdate, '%Y-%m-%d')}.jpg"),
     width = 1600, height = 900
)
maps::map("worldHires", "South Africa", lwd = 0.5)
plot(ci_sub@polygons, axes = T, add = T)
plot(xy_week_sub, col = rainbow(500, alpha = 0.9)[factor(xy_week_sub$clust)], add = T, pch = 1)
dev.off()

#  Assign feeding clusters and create centroid buffers --------------------
feeding_clusters <- ci_sub@polygons

## Creating new polygons around centroid points
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
utm36S <- CRS("+proj=utm +zone=36 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

centroid_sf <- st_as_sf(as.data.frame(cent_sub),
                        coords = c("V1", "V2"),
                        crs = 4326
)

## Buffer circles by 500m
circles_sf <- st_buffer(centroid_sf, dist = 500)
crs(circles_sf)

## Check plot
plot(circles_sf, axes = T)
extent(circles_sf)

# Add attributes to feeding clusters --------------------------------------

## Convert to multiple part polygons to add additional attributes
feeding_cluster_sf <- st_cast(circles_sf, "POLYGON") %>%
  mutate(UniqueClusterID = paste("CLUSTID", 1:nrow(.), sep = "_")) %>%
  mutate(UniqueClusterID = gsub("-", "", UniqueClusterID))

plyr::ddply(feeding_cluster_sf, c("UniqueClusterID"), summarise, n = length(UniqueClusterID))
plyr::ddply(week_data_sub, c("UniqueClusterID"), summarise, n = length(UniqueClusterID))
# plyr::ddply(clus_output_7days, c("UniqueClusterID"), summarise, n = length(UniqueClusterID))

feeding_cluster_sf <- st_transform(feeding_cluster_sf, wgs84)

# Leaflet plot to check clusters ------------------------------------------
names(providers)

map_1 <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group="SATELLITE") %>%
  addProviderTiles("Stamen.TerrainBackground", group="Stamen") %>%
  addProviderTiles("OpenStreetMap.France"  , group="VLS") %>%#"CartoDB.Voyager" "CartoDB.VoyagerLabelsUnder" "Stadia.Outdoors" "OpenStreetMap.France"
  addPolygons(
    data = feeding_clusters, color = "red",
    weight = 15, smoothFactor = 0.5, opacity = 1.0,
    fillOpacity = 0.1, group = "OLD POLYS"
  ) %>%
  addPolygons(
    data = feeding_cluster_sf, color = "blue", weight = 10,
    smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1,
    popup = ~UniqueClusterID, group = "NEW POLYS",
    label = ~UniqueClusterID,
    labelOptions = leaflet::labelOptions(
      noHide = TRUE, direction = "bottom",
      textsize = "10px", textOnly = TRUE,
      offset = c(0, -5), opacity = 1,
      style = list("color" = "black")
    )
  ) %>%
  addCircleMarkers(
    data = week_data_sub, lng = ~longitude, lat = ~latitude,
    radius = week_data_sub$clust / 500, color = "red",
    popup = ~id, label = ~ paste0(
      "Feeding Event: ",
      timestamp, ": Co-ordinates  ",
      coords, UniqueClusterID
    )
  ) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Stamen", "SATELLITE", "VLS"),
    overlayGroups = c("NEW POLYS", "OLD POLYS"), # "Known carcasses"
    options = layersControlOptions(collapsed = T)
  )

map_1
# 
# head(as.data.frame(feeding_cluster_sf))
# 
# 
# xy_week_sub <- xy_week_sub %over% feeding_cluster_sf

# Calculating re-visitation to clusters ------------------------------------

now <- now(tzone = "Africa/Johannesburg")

polysf <- as(feeding_cluster_sf, "sf")
polysf <- st_cast(polysf, "POLYGON")
polysf$UniqueClusterID <- as.factor(polysf$UniqueClusterID)

poly_sp <- as_Spatial(polysf)

## Create list of cluster polygon IDs
xlist <- unique(poly_sp$UniqueClusterID)

# Function to calculate revisitation to each cluster polygon over different timeframes
cluster_fn <- function(x, time_data) {
  time_data <- subset(time_data, select = c("longitude", "latitude", "timestamp", "id"))
  time_data <- time_data[order(time_data$id, time_data$timestamp), ]
  
  uni_clus <- subset(poly_sp, UniqueClusterID == x)
  clusterrevisits <- getRecursionsInPolygon(as.data.frame(time_data), uni_clus)
  clusterstats <- clusterrevisits$revisitStats
  clusterstats$UniqueClusterID <- as.character(x)
  return(clusterstats)
}

# BUG In code line 1084 clus_output for 7days function is actually specified using the data_10days dataframe.

# clus_output_7days <- map2_df(
#   .x = xlist,
#   .y = list(dataNSR_10days),
#   .f = cluster_fn
# ) %>%
#   mutate(data_range = "7days")

# FIXME Use 7 day data frame and Fill in error rows (clusters with no revisits) with NAs /0


## Using 7 day data causes error - see below
## NO REVISITS IN CLUSTID 14 CAUSES ERROR
# Suggests that the points don't fall within any cluster
# https://github.com/cbracis/recurse/issues/1

conflicted::conflict_prefer("compact", "plyr")
conflicted::conflict_prefer("compact", "purrr")

clus_output_7days <- map2(
  .x = xlist,
  .y = list(dataNSR_7days),
  .f = safely(cluster_fn)
)

## Manually check and print the clusters with errors
clus_output_7days %>%
  map("error") %>%
  purrr::compact()

null_rows <- clus_output_7days %>%
  map("error")

names(null_rows) <- seq_along(null_rows)

null_rows <- null_rows %>%
  compact

null_ref <- as.numeric(names(null_rows))

as.character(xlist[null_ref])

## Add error clusters and set all values to NA
clus_output_7days <- clus_output_7days %>%
  map("result") %>%
  bind_rows %>%
  mutate(data_range = "7days")

clus_output_7days <- clus_output_7days %>%
  add_row(UniqueClusterID = as.character(xlist[null_ref]),
          data_range = "7days")

#need to do the same for 31 day data, to prevent error: 'tim' is not character or numeric
# clus_output_31days <- map2_df(
#   .x = xlist,
#   .y = list(dataNSR_31days),
#   .f = cluster_fn
# ) %>%
#   mutate(data_range = "31days")
conflicted::conflict_prefer("compact", "plyr")
conflicted::conflict_prefer("compact", "purrr")


clus_output_31days <- map2(
  .x = xlist,
  .y = list(dataNSR_31days),
  .f = safely(cluster_fn)
)


## Manually check and print the clusters with errors
clus_output_31days %>%
  map("error") %>%
  purrr::compact()

null_rows <- clus_output_31days %>%
  map("error")

names(null_rows) <- seq_along(null_rows)

null_rows <- null_rows %>%
  compact

null_ref <- as.numeric(names(null_rows))

as.character(xlist[null_ref])

## Add error clusters and set all values to NA
clus_output_31days <- clus_output_31days %>%
  map("result") %>%
  bind_rows %>%
  mutate(data_range = "31days")

clus_output_31days <- clus_output_31days %>%
  add_row(UniqueClusterID = as.character(xlist[null_ref]),
          data_range = "31days")

#also have to do this for 180 day data

# clus_output_180days <- map2_df(
#   .x = xlist,
#   .y = list(dataNSR_180days),
#   .f = cluster_fn
# ) %>%
#   mutate(data_range = "180days")

clus_output_180days <- map2(
  .x = xlist,
  .y = list(dataNSR_180days),
  .f = safely(cluster_fn)
)

## Manually check and print the clusters with errors
clus_output_180days %>%
  map("error") %>%
  purrr::compact()

null_rows <- clus_output_180days %>%
  map("error")

names(null_rows) <- seq_along(null_rows)

null_rows <- null_rows %>%
  compact

null_ref <- as.numeric(names(null_rows))

as.character(xlist[null_ref])

## Add error clusters and set all values to NA
clus_output_180days <- clus_output_180days %>%
  map("result") %>%
  bind_rows %>%
  mutate(data_range = "180days")

clus_output_180days <- clus_output_180days %>%
  add_row(UniqueClusterID = as.character(xlist[null_ref]),
          data_range = "180days")


# clus_output_365days <- map2_df(
#   .x = xlist,
#   .y = list(dataNSR_365days),
#   .f = cluster_fn
# ) %>%
#   mutate(data_range = "365days")

clus_output_365days <- map2(
  .x = xlist,
  .y = list(dataNSR_365days),
  .f = safely(cluster_fn)
)

## Manually check and print the clusters with errors
clus_output_365days %>%
  map("error") %>%
  purrr::compact()

null_rows <- clus_output_365days %>%
  map("error")

names(null_rows) <- seq_along(null_rows)

null_rows <- null_rows %>%
  compact

null_ref <- as.numeric(names(null_rows))

as.character(xlist[null_ref])

## Add error clusters and set all values to NA
clus_output_365days <- clus_output_365days %>%
  map("result") %>%
  bind_rows %>%
  mutate(data_range = "365days")

clus_output_365days <- clus_output_365days %>%
  add_row(UniqueClusterID = as.character(xlist[null_ref]),
          data_range = "365days")

# Combine revisit stats dataframes ----------------------------------------
revisit_data_clusters <- bind_rows(
  clus_output_7days,
  clus_output_31days,
  clus_output_180days,
  clus_output_365days
) %>%
  as_tibble()

revisit_data_clusters

revisit_data_clusters$timeInside <- substr(revisit_data_clusters$timeInside, 1, 5)
revisit_data_clusters$timeInside <- as.numeric(revisit_data_clusters$timeInside)
range(revisit_data_clusters$timeInside, na.rm = TRUE)

## Write reference file
revisit_data_clusters %>%
  write_csv(glue("{outdir}/03_clusters_revisit_{format(todaysdate, '%Y-%m-%d')}.csv"))

# Cluster summaries -------------------------------------------------------
summarydata <- revisit_data_clusters %>%
  group_by(data_range, UniqueClusterID) %>%
  summarise(
    tot_revisits_in_clust = sum(na.omit(coordIdx)),
    tot_time_in_clust = sum(na.omit(timeInside)),
    ave_time_in_clust = mean(timeInside),
    individIDs_in_clust = (test <- paste(unique(id), collapse = ", ")),
    total_individs_in_clus = (count <- length(unique(id)))
  ) %>%
  ungroup()

summarydata %>%
  write_csv(glue("{outdir}/04_clusters_summary_{format(todaysdate, '%Y-%m-%d')}.csv"))


# Pivot cluster summary data -------------------------------------------------
glimpse(summarydata)

clust_data_final <- summarydata %>%
  pivot_wider(
    id_cols = NULL, names_from = data_range,
    names_glue = "{.value}__{data_range}",
    values_from = c(tot_revisits_in_clust:total_individs_in_clus)
  )

clust_data_final <- clust_data_final %>%
  rename_with(., ~ str_replace(., "days", ""))

names(clust_data_final)

## Important - have to deduct columns to get true values for each timeframe:
clust_data_final$totrev7 <- clust_data_final$tot_revisits_in_clust__7

clust_data_final$totrev31 <- (clust_data_final$tot_revisits_in_clust__31 -
                                clust_data_final$tot_revisits_in_clust__7)

clust_data_final$totrev180 <- (clust_data_final$tot_revisits_in_clust__180 -
                                 clust_data_final$tot_revisits_in_clust__31)

clust_data_final$totrev365 <- (clust_data_final$tot_revisits_in_clust__365 -
                                 clust_data_final$tot_revisits_in_clust__180)

# Plot cluster revisit frequency ------------------------------------------

## Original counts
fig1 <- plot_ly(clust_data_final,
                x = ~UniqueClusterID,
                y = ~tot_revisits_in_clust__7,
                type = "bar", name = "7 days"
) %>%
  add_trace(y = ~tot_revisits_in_clust__31, name = "31 days") %>%
  add_trace(y = ~tot_revisits_in_clust__180, name = "180 days") %>%
  add_trace(y = ~tot_revisits_in_clust__365, name = "365 days") %>%
  plotly::layout(yaxis = list(title = "Count"), barmode = "group", title = "Original counts")
fig1

## Write to file
htmlwidgets::saveWidget(
  as_widget(fig1),
  glue("{outdir}/01_clusters_freqency_original_{format(todaysdate, '%Y-%m-%d')}.html")
)

## True counts
fig2 <- plot_ly(clust_data_final, x = ~UniqueClusterID,
                y = ~totrev7, type = "bar", name = "7 days") %>%
  add_trace(y = ~totrev31, name = "31 days") %>%
  add_trace(y = ~totrev180, name = "180 days") %>%
  add_trace(y = ~totrev365, name = "365 days") %>%
  plotly::layout(yaxis = list(title = "Count"), barmode = "group", title = "True counts")
fig2
## Write to file
htmlwidgets::saveWidget(
  as_widget(fig2),
  glue("{outdir}/02_clusters_freqency_true_{format(todaysdate, '%Y-%m-%d')}.html")
)


# Assign revisitation classes ---------------------------------------------

## NOTE In original code this refers to line: 1369
clust_data_final <- clust_data_final %>% mutate(
  presentin7 =
    case_when(
      totrev7 <= 0 ~ "",
      totrev7 > 0 ~ "a"
    )
)

clust_data_final <- clust_data_final %>% mutate(
  presentin31 =
    case_when(
      totrev31 <= 0 ~ "",
      totrev31 > 0 ~ "b"
    )
)

clust_data_final <- clust_data_final %>% mutate(
  presentin180 =
    case_when(
      totrev180 <= 0 ~ "",
      totrev180 > 0 ~ "c"
    )
)

clust_data_final <- clust_data_final %>% mutate(
  presentin365 =
    case_when(
      totrev365 <= 0 ~ "",
      totrev365 > 0 ~ "d"
    )
)



clust_data_final$score <- paste(clust_data_final$presentin7,
                                clust_data_final$presentin31,
                                clust_data_final$presentin180,
                                clust_data_final$presentin365,
                                sep = ""
)

clust_data_final %>%
  select(score, UniqueClusterID)

#unique(clust_data_final$score)

# Assign cluster types based on revisitation classes ----------------------

## Important - specifying cluster type

# QUESTION This is where the training data comes into play?

clust_data_final <- clust_data_final %>%
  mutate(
    Cluster_Type =
      case_when(
        tot_revisits_in_clust__365 <= 2 ~ "Feeding event detected",
        tot_revisits_in_clust__365 > 2 & tot_revisits_in_clust__365 <= 50 ~ "Nest/Roost/Water",
        tot_revisits_in_clust__365 >= 50 ~ "Nesting site"
      ))

clust_data_final %>%
  select(score, UniqueClusterID, Cluster_Type)

clust_data_final$predicted_activity <- clust_data_final$score

clust_data_final$predicted_activity <- as.factor(clust_data_final$predicted_activity)

# code	predicted activity
# a	Feeding detected
# b	Feeding detected
# c	Feeding detected
# d	Feeding detected
# ab	Feeding detected
# ac	Roost/Water/Supplementary Feeding site
# ad	Roost/Water/Supplementary Feeding site
# bc	Roost/Water/Supplementary Feeding site
# bd	Roost/Water/Supplementary Feeding site
# cd	Roost/Water/Supplementary Feeding site
# abc	Nest/Roost/Water/Supplementary Feeding site
# acd	Nest/Roost/Water/Supplementary Feeding site
# abd	Nest/Roost/Water/Supplementary Feeding site
# bcd	Nest/Roost/Water/Supplementary Feeding site
# abcd	Breeding site

# library("combinat")
#
# x <- c("a","b","c","d")
# t(combn(c(x,x), 2))

clust_data_final$predicted_activity <- gsub(
  "\\ba\\b",
  "Feeding detected",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bb\\b",
  "Feeding detected",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bc\\b",
  "Feeding detected",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bd\\b",
  "Feeding detected",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bab\\b",
  "Feeding detected",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\babc\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\babcd\\b",
  "Breeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bac\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bacd\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bad\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\babd\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bbcd\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\bbc\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final$predicted_activity <- gsub(
  "\\babd\\b",
  "Nest/Roost/Water/Supplementary Feeding site",
  clust_data_final$predicted_activity
)

clust_data_final %>%
  select(score, UniqueClusterID, Cluster_Type, predicted_activity)

unique(clust_data_final$score)
unique(clust_data_final$predicted_activity)


## Checks
plyr::ddply(clust_data_final, c("Cluster_Type"), summarise, n = length(Cluster_Type))
plyr::ddply(clust_data_final, c("score"), summarise, n = length(score))
plyr::ddply(clust_data_final, c("predicted_activity"), summarise, n = length(predicted_activity))


# Calculating points in clusters ------------------------------------------

# NOTE Line ref 1448 in original code

point_data <- dataNSR
range(point_data$timestamp)

## Order data
point_data <- point_data[order(point_data$id, point_data$timestamp), ]

## Specify fixed date thresholds
todaysdate <- now(tzone = "Africa/Johannesburg")

date_7days <- ymd_hms(todaysdate) - days(7)
date_active0days <- ymd_hms(todaysdate) - days(10)
date_31days <- ymd_hms(todaysdate) - days(31)
date_active80days <- ymd_hms(todaysdate) - days(180)
date_365days <- ymd_hms(todaysdate) - days(365)

data_active_7days <- subset(point_data, timestamp >= date_7days)
data_active_7days <- data_active_7days[order(data_active_7days$id, data_active_7days$timestamp), ]
range(data_active_7days$timestamp)

data_active_10days <- subset(point_data, timestamp >= date_active0days)
data_active_10days <- data_active_10days[order(data_active_10days$id, data_active_10days$timestamp), ]
range(data_active_10days$timestamp)

data_active_31days <- subset(point_data, timestamp < date_7days & timestamp >= date_31days)
data_active_31days <- data_active_31days[order(data_active_31days$id, data_active_31days$timestamp), ]

data_active_180days <- subset(point_data, timestamp < date_31days & timestamp >= date_active80days)
data_active_180days <- data_active_180days[order(data_active_180days$id, data_active_180days$timestamp), ]
range(data_active_180days$timestamp)

data_active_365days <- subset(point_data, timestamp < date_active80days)
data_active_365days <- data_active_365days[order(data_active_365days$id, data_active_365days$timestamp), ]
range(data_active_365days$timestamp)#this wont be valid until more tracking days are obtained

# Calculate number of fixes in each cluster -------------------------------

## Used as a proxy for revisitation
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

feeding_cluster_sf

# x <- data_active_7days
# timeframe <- "7days"

## Function to calculate summaries for each cluster and timeframe
cluster_summary_calc <- function(x, timeframe) {
  
  xy_points <- x %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    )
  
  xy_feeding_int <- sp::over(x = as(xy_points, "Spatial"), y = as(feeding_cluster_sf, "Spatial"))
  
  xy_int <- x %>%
    ungroup() %>%
    mutate(UniqueClusterID = xy_feeding_int$UniqueClusterID) %>%
    filter(!is.na(UniqueClusterID)) %>%
    mutate(n = 1)
  
 df <- xy_int %>%
    group_by(UniqueClusterID) %>%
    summarise(
      tot_points_in_clust = sum(na.omit(n)),
      individIDs_in_clust = (test <- paste(unique(id), collapse = ", ")),
      total_individs_in_clus = (count <- length(unique(id)))
    ) %>%
    mutate(data_range = timeframe)
  
  return(df)
}

data_sets <- list(
  data_active_7days, data_active_10days, data_active_31days,
  data_active_180days#, data_active_365days
)

date_labels <- c("7days", "10days", "31days", "180days")#, "365days" reove for now

revisit_data_clusters <- map2_df(
  .x = data_sets,
  .y = date_labels,
  .f = cluster_summary_calc
)

## Write file
revisit_data_clusters %>%
  write_csv(glue("{outdir}/05_revisit_data_clusters_{format(todaysdate, '%Y-%m-%d')}.csv"))

# Pivot cluster timeframe summary data ------------------------------------

# QUESTION How is this cluster data different from the cluster summary 04_clusters_summary?
pts_in_clust_final <- revisit_data_clusters %>%
  pivot_wider(
    id_cols = UniqueClusterID, names_from = data_range,
    names_glue = "{.value}__{data_range}",
    values_from = c(tot_points_in_clust:total_individs_in_clus)
  )

pts_in_clust_final <- pts_in_clust_final %>%
  rename_with(., ~ str_replace(., "days", ""))

names(pts_in_clust_final)

## Write file
pts_in_clust_final %>%
  write_csv(glue("{outdir}/06_pts_in_clust_final_{format(todaysdate, '%Y-%m-%d')}.csv"))

range(pts_in_clust_final$tot_points_in_clust__7, na.rm = TRUE)
range(pts_in_clust_final$tot_points_in_clust__31, na.rm = TRUE)
# range(pts_in_clust_final$tot_points_in_clust__180, na.rm = TRUE)
# range(pts_in_clust_final$tot_points_in_clust__365, na.rm = TRUE)

## Plot check
fig <- plot_ly(pts_in_clust_final,
               x = ~UniqueClusterID,
               y = ~tot_points_in_clust__7, type = "bar", name = "7 days"
) %>%
  add_trace(y = ~tot_points_in_clust__31, name = "31 days") %>%
  #add_trace(y = ~tot_points_in_clust__180, name = "180 days") %>%
  #add_trace(y = ~tot_points_in_clust__365, name = "365 days") %>%
  layout(yaxis = list(title = "Count"), barmode = "group")

fig

# Assign cluster type ----------------------------------------------------- #change all to tot_points_in_clust__365 when data comes through

pts_in_clust_final <- pts_in_clust_final %>%
  mutate(
    Cluster_Type =
      case_when(
        tot_points_in_clust__31 <= 1 ~ "Feeding event detected",
        tot_points_in_clust__31 > 1 &
          tot_points_in_clust__31 <= 50 ~ "Nest/Roost/Water/Supplementary Feeding",
        tot_points_in_clust__31 > 50 ~ "Nesting site"
      )
  )


head(pts_in_clust_final)

plyr::ddply(pts_in_clust_final, c("Cluster_Type"), summarise, n = length(Cluster_Type))
unique(pts_in_clust_final$Cluster_Type)

# Add points to revisits --------------------------------------------------

# NOTE clust_data_final includes revisit counts, individuals, cluter types, time in clusters, individuals in clusters

clust_data_final <- clust_data_final %>%
  left_join(pts_in_clust_final %>%
              select(UniqueClusterID, tot_points_in_clust__7,
                     tot_points_in_clust__31),#, tot_points_in_clust__180,
                     #tot_points_in_clust__365),
            by = "UniqueClusterID"
  )

names(clust_data_final)
glimpse(clust_data_final)

# Add cluster attribute data to cluster polygons --------------------------
get_centroids <- as.data.frame(gCentroid(poly_sp, byid = TRUE)) %>%
  mutate(UniqueClusterID = poly_sp$UniqueClusterID)

Feeding_clus_poly_final <- as(poly_sp, "sf") %>%
  left_join(clust_data_final,by = "UniqueClusterID") %>%
  left_join(get_centroids, by = "UniqueClusterID") %>%
  mutate(
    coords = paste(y, x, sep = ", "),
    clust_coords = paste(y, x, sep = ", ")
  ) # FIXME Duplication neccessary?

glimpse(Feeding_clus_poly_final)
plot(Feeding_clus_poly_final$geometry)

# Process weekdata to add event_id ----------------------------------------

# week_data <- week_data[order(week_data$timestamp), ]
# todaysdate <- now(tzone = "Africa/Johannesburg")
# week_data$days_lapsed_since_feeding_event <- difftime(todaysdate,
#                                                       week_data$timestamp, units = "days")
#
# week_data$event_id <- cumsum(!duplicated(week_data[1:2]))
# week_data$event_id <- paste("eventid", week_data$event_id,
#                             week_data$date, sep = "_")

# NOTE replaced week_data with week_data_sub
week_data_sub <- week_data_sub[order(week_data_sub$timestamp), ]
todaysdate <- now(tzone = "Africa/Johannesburg")
week_data_sub$days_lapsed_since_feeding_event <- difftime(todaysdate,
                                                          week_data_sub$timestamp, units = "days")

#glimpse(week_data_sub)
week_data_sub<-as_tibble(week_data_sub)

#add unique ID to each point of interest
week_data_sub <-week_data_sub%>% 
  mutate(event_id=paste("", 1:nrow(.), sep="_"))%>% 
  mutate(event_id= gsub("_", "", event_id))

# week_data_sub <-week_data_sub%>% 
#   mutate(event_id=paste("",1:nrow(.), sep="_"))%>% 
#   mutate(event_id= gsub("-", "", event_id))

# glimpse(week_data_sub)
# duplicated(week_data_sub$UniquepointID)
# sum(duplicated(week_data_sub$UniquepointID))

# week_data_sub$event_id<-week_data_sub$UniquepointID
# 
# week_data_sub$event_id <- cumsum(!duplicated(week_data_sub[1:2]))
# head(week_data_sub[44])


week_data_sub$event_id <- paste("eventid", week_data_sub$event_id,
                                week_data_sub$year, week_data_sub$month, week_data_sub$day, week_data_sub$hour,
                                sep = "_")

duplicated(week_data_sub$event_id)
sum(duplicated(week_data_sub$event_id))

# #convert to multiple part polygons to add additional attributes
# FeedingCluster.sf <-st_cast(dat_circles, "POLYGON")%>% #from sf # converted single part polygon (1 attribute), into mutli part polygon
#   mutate(UniqueClusterID=paste("CLUSTID", 1:nrow(.), sep="_"))%>% #add unique ID to each cluster
#   mutate(UniqueClusterID= gsub("-", "", UniqueClusterID))

glimpse(week_data_sub)
# wgs <- "+proj=longlat +datum=WGS84"
# wgsCRS <- CRS("+proj=longlat +datum=WGS84")
# week_pts <- SpatialPointsDataFrame(coordinates(cbind(week_data$longitude,
#                                                      week_data$latitude)),
#                                    data = week_data, proj4string = CRS(wgs)
# )


wgs <- "+proj=longlat +datum=WGS84"
wgsCRS <- CRS("+proj=longlat +datum=WGS84")
week_pts <- SpatialPointsDataFrame(coordinates(cbind(week_data_sub$longitude,
                                                     week_data_sub$latitude)),
                                   data = week_data_sub, proj4string = CRS(wgs)
)


# Read in South Africa shapefiles ---------------------------------------------
prj <- "+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
prjCRS <- CRS(prj)

NSR<- spTransform(readOGR("data input/NSR_shapefiles/Niassa Special Reserve/NSR_Zonning_Detailed_in_Community_Tourism_Units.shp"), wgs)


NSR_admin <- spTransform(readOGR("data input/NSR_shapefiles/Niassa Special Reserve/Buffer_Zone.shp"), wgs)
names(NSR_admin)
head(NSR)
NSR_admin$NAME <- as.factor(NSR_admin$NAME)
NSR$NAME<-as.factor(NSR$NAME)

## Extract CH_NO and place for each point

# week_pts_admin = week_data.area2
# week_pts_pa = week_data.area
week_pts_pa <- week_pts %over% NSR
week_pts_admin <- week_pts %over% NSR_admin
# week_pts_KNPsections <- week_pts %over% KNP_sections

# week_data <- cbind(week_data, week_pts_pa, week_pts_admin)
# week_data <- week_data[order(week_data$timestamp), ]

week_data_sub <- cbind(week_data_sub, week_pts_pa, week_pts_admin)
week_data_sub <- week_data_sub[order(week_data_sub$timestamp), ]
# unique(week_data_sub$SECTION)

Activity_clusters <- subset(Feeding_clus_poly_final,!predicted_activity %in% c("Feeding detected"))
unique(Activity_clusters$predicted_activity)

# Subset by feeding detected ----------------------------------------------
Feeding_clusters_map <- subset(
  Feeding_clus_poly_final,
  Feeding_clus_poly_final$predicted_activity %in% c("Feeding detected")
)
unique(Feeding_clusters_map$predicted_activity)

todaysdate <- now(tzone = "Africa/Johannesburg")
lastseven <- ymd_hms(todaysdate) - days(obs_days)
lastseven <- subset(dataNSR, timestamp >= lastseven)
lastseven <- as_tibble(lastseven)

# Create leaflet tracks timeline slider -----------------------------------
power <- lastseven
power$start <- power$timestamp
power <- as_tibble(power) %>%
  mutate(start = timestamp) %>%
  mutate(end = lead(timestamp))

summary(power$end)

power_geo <- geojsonio::geojson_json(power, lat = "latitude", lon = "longitude")

# Save and write to RData -------------------------------------------------
save.image(glue("{outdir}/04_NSR_mapping_data.RData"))

