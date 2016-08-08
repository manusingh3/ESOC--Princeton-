library(doParallel)
library(foreach)
library(raster)
library(sp)
library(rgdal)
library(ggmap)
library(plotly)
library(sp)
library(rgdal)

setwd("C:/Users/ms52/Downloads/nightlights/2013")

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




##Loop through data

par(mai=c(0.25,0.25,0.25,0.25),bg='#001a4d',mfrow = c(2,1))
cities <- c("Kandahar", "Sar-e Pol")
coords <- data.frame()
for(i in 1:length(cities)){
  
  ##Coords
  temp_coord <- geocode(cities[i], source = "google")
  coords <- rbind(coords,temp_coord)
  
  e <- extent(temp_coord$lon - 1, temp_coord$lon + 1,
              temp_coord$lat - 0.25, temp_coord$lat + 0.25)
  rc <- crop(rast, e)    
  
  ##Rescale brackets
  sampled <- as.vector(rc)
  clusters <- 4
  clust <- kmeans(sampled,clusters)$cluster
  combined <- as.data.frame(cbind(sampled,clust))
  brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
  
  #Plots
  plot(rc, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters), 
       legend=T,frame = F, asp=1.5)
  
  text(temp_coord$lon ,temp_coord$lat + 0.15,cities[i], 
       col="white", cex=1.25)
  
  
  rm(combined)
}


######Saving data for these cities ( i is referecing OBJECTID from the map shapefile)
#Thus Kandhar i = 46, Sari-Pul  i = 71

i = 46
polygon <- map[i,] #extract one polygon
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
data <- cbind(as.character(map$OBJECTID[i]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2013"
write.csv(data, file = "kandhar2013.csv")

i = 71
polygon <- map[i,] #extract one polygon
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
data <- cbind(as.character(map$OBJECTID[i]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2013"
write.csv(data, file = "sari2013.csv")


# read consolidated data file
library(lattice)
library(ggplot2)

data = read.csv(file.choose())

kandhar = data[data$GEOID==46,]
kandhar$year = as.Date(kandhar$year, format='%Y')
sari$year = as.Date(sari$year, format='%Y')
sari = data[data$GEOID==71,]


# data1 <- data[data$year %in% c("2008", "2013"), ]
# data1$year = as.factor(data1$year)
# data1$GEOID  = as.factor(data1$GEOID )


# ggplot(data1, aes(x=avg_rad, colour=year)) + geom_density()++     facet_grid(avg_rad~GEOID) + opts(aspect.ratio = 1)
# 
# 
# ggplot(kandhar, aes(x=(avg_rad), colour=year)) + geom_freqpoly(stat = "bin", aes(size = 2))
# 
# xyplot(lat ~ lon | cut(avg_rad, 2), data = kandhar)

##################Plots for Kandhar############################
ggplot(kandhar, aes( year, avg_rad)) + geom_smooth(method = "glm")+ggtitle("Time series using logistic regression- KANDHAR")

  ggplot(kandhar, aes( year, avg_rad)) + geom_smooth(method = "loess", se=TRUE)+
  geom_jitter(alpha=2/10, shape=21, fill="pink", colour="black", size=5) +
    ggtitle("Time series using locally weighted regression- KANDHAR")
  ##################Plots for Saripul############################  
  ggplot(sari, aes( year, avg_rad)) + geom_smooth(method = "glm")+
    ggtitle("Time series using logistic regression- SAR-E-PUL")
  
  ggplot(sari, aes( year, avg_rad)) + geom_smooth(method = "loess", se=TRUE)+
    geom_jitter(alpha=2/10, shape=21, fill="pink", colour="black", size=5) +
    ggtitle("Time series using locally weighted regression- SAR-E-PUL")
######################################Plots for faceted data for comparision #########################  
 
  data$year = as.Date(data$year, format = "%Y")

  ggplot(data, aes( year, avg_rad)) + geom_smooth(method = "loess", se=TRUE)+
    geom_jitter(alpha=0.5/10, shape=21, fill="pink", colour="black", size=5) +
    ggtitle("Comparison KANDHAR-46 SAR-E-PUL-71")+facet_grid(. ~ GEOID)  

# geom_line()densityplot(GEOID~avg_rad|factor(year),data, groups = GEOID, plot.points = FALSE, auto.key = TRUE)
# 
# 
# xyplot(lat ~ lon | cut(avg_rad, 6),,pch = 20,
#        groups=factor(year, labels = c("2008","2009","2010","2011","2012","2013")),
#        auto.key = list(columns = 2),type= c("p","g"),layout = c(3,1), data = kandhar)












