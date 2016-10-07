#population cluster analysis
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
library(snow)
library(osc)
setwd("C:/Users/ms52/Downloads/population")

# load the required file for analysis- and set the projection
file =  file.choose()

#tifs = list.files(file,pattern = "\\.tif")
rast <- raster(file)

wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(rast) <- CRS(wgs84)

# use the district shapefiles - 
#trial files\
 file1 = file.choose()
 r = raster(file1)
 total = read.csv(file.choose())
 copy1 = total
 total = total[,c(3,4,5)]
 total$avg_rad = as.numeric(total$avg_rad)

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

####Clustering algo from OSC - orthodrinic spatial clustering 

trial = cca(total, s=1, mode=3, count.cells=FALSE,
    count.max=ncol(data)*3,
    res.x=NULL, res.y=NULL, cell.class=1,
    unit="", compare="")
cols <- c("white",rep(rainbow(10), length.out=length(table(trial))) )

image(trial, col=cols, xlab="", ylab="")
##### raster clustering

urban <- cca(r, cell.class=1,s=2000, unit="m")
str(urban)
# plot the result
# result <- landcover*NA
# result[cellFromXY(result,urban$cluster[,c("long","lat")])]<-urban$cluster[,"cluster_id"]*(-1)
# plot(result, col=rainbow(9))


# replace all NA values from raster dataset with 0 for computation 

r[is.na(r[])] <- 0 
# wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# projection(r) <- CRS(wgs84)


###Clustering agaon with total dataset excluding the 0 points
row_sub = apply(total, 1, function(row) all(row !=0 ))
total = total[row_sub,]

#new cluster
# trial = cca(total, s=5000, mode=1, count.cells=FALSE,
#             res.x=NULL, res.y=NULL,
#             unit="", compare="")
urban <- cca(rast, cell.class=1,s=2000, unit="m")
trial2 = cca(rast, s=50000, count.cells=FALSE,cell.class=1,unit="m", compare="10")

result = total*NA
result[cellFromXY(result,trial$cluster[,c("long","lat")])]<-trial$cluster[,"cluster_id"]*(-1)
plot(result, col=rainbow(9))


 test = trial$cluster
 test = test[ !(test$cluster_id ==1), ]
 colnames(test) = c("lon","lat","cluster_id")
 
 
 #plotting on the actual map 
 af <- get_map(location = "afghanistan", zoom = 15)
 
 af  <- qmap("afghanistan", zoom = 6, legend = "topleft")
 
  af+ geom_point(aes(x = lon, y = lat,
                    size = cluster_id),
                data = test)
  
  
  
 
