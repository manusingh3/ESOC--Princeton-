a = map$OBJECTID[map$DISTID%in%capital]
a
#i = 46
#a = a[-10]
#i = 89
a1 = a[1:11]
a1 =a1[-10]
a2 = a[12:20]
a3 = a[21:30]
a4 = a[31:34]

#a3 = a3[-6]
total = data.frame()
new1 = data.frame()
for(i in (a1)){
  
  print(i)
  data = data.frame()
  
  polygon <- map[map$OBJECTID ==i,] #extract one polygon
  polygon
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
  data$year = "2009"
  new1 = rbind(data, new1)
  table(new1$GEOID)
  
}
total = rbind(total,new1)





new1 = data.frame()
for(i in (a2)){
  
  print(i)
  data = data.frame()
  
  polygon <- map[map$OBJECTID ==i,] #extract one polygon
  polygon
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
  data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
  colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
  data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
  data$year = "2009"
  new1 = rbind(data, new1)
  table(new1$GEOID)
  
}

total = rbind(total,new1)



















new1 = data.frame()
for(i in (a3)){
  
  print(i)
  data = data.frame()
  
  polygon <- map[map$OBJECTID ==i,] #extract one polygon
  polygon
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
  data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
  colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
  data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
  data$year = "2009"
  new1 = rbind(data, new1)
  table(new1$GEOID)
  
}

i = 294

print(i)
data = data.frame()

polygon <- map[map$OBJECTID ==i,] #extract one polygon
polygon
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
data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2009"
new1 = rbind(data, new1)
table(new1$GEOID)


i = 302
print(i)
data = data.frame()

polygon <- map[map$OBJECTID ==i,] #extract one polygon
polygon
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
data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2009"
new1 = rbind(data, new1)
table(new1$GEOID)

i = 310
print(i)
data = data.frame()

polygon <- map[map$OBJECTID ==i,] #extract one polygon
polygon
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
data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2009"
new1 = rbind(data, new1)
table(new1$GEOID)

i = 323
print(i)
data = data.frame()

polygon <- map[map$OBJECTID ==i,] #extract one polygon
polygon
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
data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2009"
new1 = rbind(data, new1)
table(new1$GEOID)

i = 336
print(i)
data = data.frame()

polygon <- map[map$OBJECTID ==i,] #extract one polygon
polygon
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
data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
data$year = "2009"
new1 = rbind(data, new1)
table(new1$GEOID)

total = rbind(total, new1)





new1 = data.frame()
for(i in (a4)){
  
  print(i)
  data = data.frame()
  
  polygon <- map[map$OBJECTID ==i,] #extract one polygon
  polygon
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
  data <- cbind(as.character(map$OBJECTID[i-1]),coords, data) 
  colnames(data)<-c("GEOID","lon","lat","avg_rad") #note that 
  data$avg_rad[data$avg_rad==0] <- NA ;data <- data[!is.na(data$avg_rad),] #keep non-NA values only
  data$year = "2009"
  new1 = rbind(data, new1)
  table(new1$GEOID)
  
}

total = rbind(total,new1)

write.csv(total, file = "nightlight_dist_cap_2009.csv")
getwd()





















