
#load the file 

file = file.choose()
df = read.csv(file)

# aggregate the shape length as per district

df$Shape_Leng = as.numeric(as.character(df$Shape_Leng))

a = by(df[, 17], df$PROV_34_NA, sum)

a = tapply(df$Shape_Leng, df$PROV_34_NA, sum)
a = as.data.frame(a)
colnames(a) = c("total_road_len")
write.csv(a, file = "province_add.csv")

b = tapply(df$Shape_Leng, df$DISTID, sum)
b = as.data.frame(b)
b

colnames(b) = c("total_road_len")

write.csv(b, file = "district_agg.csv")
# aggregate length as per province 