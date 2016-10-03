#population cluster analysis
#population cluster analysis
library(doParallel)
library(foreach)
library(raster)
library(sp)
library(rgdal)
library(ggmap)
library(plotly)
library(sp)
library(rgdal)

setwd("C:/Users/ms52/Downloads/population")

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

# setting the projection of both the files as same 
projection(map) <- CRS(wgs84)

map_data = read.csv(file.choose())

dist = list()
dist = unlist(map_data$DISTID)


total = data.frame()


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
  colnames(data)<-c("DISTID","lon","lat","avg_rad") #note that 
  #data$avg_rad = ifelse(is.na(data$avg_rad) , 0, data$avg_rad) 
  data <- data[!is.na(data$avg_rad),] #keep non-NA values only
  data$year = "07/01/2014"
  
  total = rbind(total,data)
  
}

save(total,file="population.Rdata")
