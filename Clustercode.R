library(spatialEco)
library(raster)
library(sp)
library(rgdal)
library(RColorBrewer)
library(sf)
library(rgeos)
library(ggplot2)
library(git2r)

###zonal stats requires input object SpatialPolygonDataFrame, but gUnaryUnion returns a SpatialPolygon from a SpatialPolygonDataFrame.
rm(list = ls())
setwd("C:/Ian/Tati/Code")
#Read the csv with long/lat data and ClusterID
clusters <- read.csv("Clusters.csv")

#subset data before running large operations. REMOVE THIS BEFORE FINAL RUN
clusters <- clusters[1:200,]

#separate data and coordinates, create spatial points data frame
spdf = SpatialPointsDataFrame(clusters[0:2], clusters[3])
#project in WGS 84 (where data was collected)
proj4string(spdf) = CRS("+proj=longlat +datum=WGS84")

#convert to file readable by package sf with Canada Albers Equal Area Conic
cluster_points <- st_as_sf(spdf, "sf") %>%
  st_transform(., crs = "++proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

#double check the units are meters before buffering
st_crs(cluster_points)$units

#make sure I didn't screw anything up
plot(cluster_points, col = cluster_points$ClusterSP)
#make sure specific values are same in the final product
plot(cluster_points[cluster_points$ClusterSP == 74,])

#buffer the poinst by 100 metres
cluster_Poly <- st_buffer(cluster_points, dist = 100)

#spatial eco only works with class SpatialPolygon. Converting from sf to sp (class SpatialPolygons) did not preserve attribute data, so I write to shapefile. There is undoubtedly a better way to do this without creating a temporary shapefile.
tempShp <- tempfile(pattern = "", fileext = ".shp")
st_write(cluster_Poly, tempShp)
cluster_Poly <- readOGR(tempShp)

#Merge polygons based on ClusterSP. Will only run on polygons
cluster_Poly1 <- gUnaryUnion(cluster_Poly, id = cluster_Poly$ClusterSP)

#get data to add to SpatialPolygonsDataFrame
newrows <- row.names(cluster_Poly1)

#### To convert back to SpatialPolygonsDataFrame
cluster_sf <- st_as_sf(cluster_Poly1, "sf")
tempShp1 <- tempfile(pattern = "", fileext = ".shp")
st_write(cluster_sf, tempShp1)
cluster_shape <- readOGR(tempShp1)
cluster_shape@data$clustervalue <- newrows

#plot, making sure the subset matches
plot(cluster_shape, border = cluster_shape@data$clustervalue, lwd = 10)
plot(cluster_shape[cluster_shape@data$clustervalue == 74,])

#Create test raster that simulates Landsat derived data. Substitute for the 3 LCC rasters
newExtent <- extent(cluster_Poly1)
ras = raster(ext = newExtent, resolution = 30)
ras <- setValues(ras, 10)

#Make function for zonal stats. Code only runs if function defined.
#We can customize function depending on what data we want from raster (e.g. proportion of specific values
newfunc <- function(x) {
  zone = mean(x)
  return(zone)
}

#Run zonal stats. This uses for loop. Extract would be faster but it returns cell values rather than custom function output. set plot to TRUE to confirm visually
output_data <- spatialEco::zonal.stats(x = cluster_shape, y = ras, stat = newfunc, plot = FALSE)

#Output the buffers if necessary
output <- st_as_sf(cluster_shape, "sf")
st_write(output, "final_cluster_out.shp")
