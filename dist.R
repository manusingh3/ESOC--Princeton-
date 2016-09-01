library(doParallel)
library(foreach)
library(raster)
library(sp)
library(rgdal)
library(ggmap)
library(plotly)
library(sp)
library(rgdal)

setwd("C:/Users/ms52/Downloads/nightlights/2008")

# The folder contains two files - one with corrections to exclude environmental conditions 
#like clouds and gas flares from the calculations. The other has all sources of light - we will process both in our calculations 
# and compare results. The files which end in rad_9 contain the average radiance values. The other file contains the average number of cloud free days 
# used to make that calculation. 

####### Note - VCMCFG containing data that excludes any stray light.
####### Note - The spatial projection used for this is WGS84. 


# load the required file for analysis- and set the projection
file =  file.choose()

#tifs = list.files(file,pattern = "\\.tif")
rast <- raster(file)

wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(rast) <- CRS(wgs84)

# use the district shapefiles - 

#distshape <- file.choose()

map <- readOGR(dsn="C:\\Users\\ms52\\Downloads\\nightlights\\april", layer="district398")

# setting the projection of both the files as same 
projection(map) <- CRS(wgs84)

map_data = read.csv(file.choose())

dist = list()
dist = unlist(map_data$DISTID)


total = data.frame()

#dist = unlist(dist)
#i = 1208
for(i in dist)
  {

print(i)
polygon <- map[map$DISTID==i,] #extract one polygon
extent <- extent(polygon) #extract the polygon extent 

#Raster extract
outer <- crop(rast, extent) #extract raster by polygon extent
inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon

#Convert cropped raster into a vector
#Specify coordinates
coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                      seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
#Convert raster into vector
data <- as.vector(inner)

#package data in neat dataframe
data <- cbind(as.character(i),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad = ifelse(is.na(data$avg_rad) , 0, data$avg_rad) 
#data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2013"

total = rbind(total,data)

}
write.csv(total, file = "dist_NL_2013.csv")
b = read.csv(file.choose())
b = aggregate(total$avg_rad~total$GEOID, FUN = "sum")
colnames(b) = c("distid","total rad")
b
######################Normalize by population#####################################

pop = read.csv(file.choose())
norm = data.frame()
norm = merge(b, pop, by = "distid")
norm = norm[,-3]
colnames(norm) = c("DISTID","tot_rad","pop")

#####to avoid computational problems all the 0 values are replaced by 0.01

norm$tot_rad =ifelse(norm$tot_rad==0, 0.01, norm$tot_rad)

norm$norm_rad = norm$tot_rad/norm$pop*1000000

norm = merge(norm, map_data, by = "DISTID")

 head(norm)
# colnames(norm) = c("DISTID","tot_rad","pop", "norm_rad","prov_name","dist_name","prov_id")
write.csv(norm, file = "per_capita_dist_nightlight_2013.csv")


#########################Plotting the data on the map####################################
install.packages("maptools")
library(maptools)
library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rgdal)
norm = read.csv(file.choose())
#map <-readShapeSpatial  ("C:\\Users\\ms52\\Downloads\\nightlights\\april\\district398.shp")
map <- readOGR(dsn="C:\\Users\\ms52\\Downloads\\nightlights\\april", layer="district398")


#map_new <- gSimplify(map, tol = 0.00001)

map_df = fortify(map)

colnames(norm) = c("DISTID", "tot_rad","pop","norm_rad", "id","OBJECTID","PROV_34_NA" ,"DIST_34_NA" ,"PROVID" )
map_data = merge(map_df, norm, by = "id", all.x=TRUE)
final.plot<-map_data[order(map_data$order), ] 
###############################plotting the map##################################
centroids <- setNames(do.call("rbind.data.frame", by(final.plot, final.plot$group, 
                                                     function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
centroids$label <- final.plot$DIST_34_NA[match(rownames(centroids), final.plot$group)]


ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group =group , fill = log(tot_rad)), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_distiller(name="nightlight/population", palette = "RdBu",direction = -1 , breaks = pretty_breaks(n = 5))+
  theme_nothing(legend = TRUE)+
  labs(title="Log of per capita illumination- per district, AF")+
  with(centroids, annotate(geom="text", x = long, y=lat, label = label, size = 1.75)) 

