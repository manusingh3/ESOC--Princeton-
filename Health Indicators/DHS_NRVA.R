####Check correlations between DHS and NRVA 2012 datsets 

nrva = read.csv("C:\\Users\\ms52\\Desktop\\ESOC Temp files\\NRVA_all.csv")

nrva$id = rownames(nrva)

#survey design 

df.nrva <-
	svydesign(
		ids = nrva$id,
		data = nrva,
		weights = ~Household.weight ,
	)

temp =as.data.frame(  svyby(~female.born+~male.born+~male.dead+~female.dead+~total.dead+~total.children+~no.anc.visit+~anc.doctor+~anc.midwife+~anc.nurse+~anc.chw
                            +~anc.tba+~anc.other+~anc.noone+~BA.doctor+~BA.midwife+~BA.chw+~BA.tba+~BA.noone+~BA.other+~tt.injection+~no.tt.intections+~fever+
                              cough+breathing.difficulty+diarrhoea
                            ,~Province,design=df.nrva,svytotal,na.rm=TRUE))



temp =as.data.frame(  svyby(~female.born+male.born+male.dead+female.dead+total.dead+total.children+no.anc.visit+anc.doctor+anc.midwife+anc.nurse+anc.chw
                            +anc.tba+anc.other+anc.noone+BA.doctor+fever+cough+breathing.difficulty+diarrhoea
                            ,~Province,design=df.nrva,svytotal,na.rm=TRUE))
temp = temp[, -(21:39)]
write.csv(temp, "temp.nrva.csv")

b = as.data.frame(svyby(~no.tt.intections, ~Province, df.nrva, svytotal,na.rm =  TRUE,  keep.var=TRUE))

temp = merge(temp, b, by = "Province")
svyby(~BA.nurse, ~Province, df.nrva, svytotal,na.rm =  TRUE,  keep.var=TRUE)



drops <- c("se.x","se.y")
temp = temp[ , !(names(temp) %in% drops)]

write.csv(temp, "temp.nrva.csv")
temp = read.csv(file.choose())

###############Load dhs data

dhs = read.csv("C:\\Users\\ms52\\Desktop\\ESOC Temp files\\DHS_MC.csv")
head(dhs)

dhs$id = rownames(dhs)
dhs$sample.weight..6.decimals. = dhs$sample.weight..6.decimals./1000000
df.dhs <- 
  svydesign( 
    ids = ~id , 
    data = dhs , 
    weights = ~sample.weight..6.decimals.
  )

temp.1 =as.data.frame(svyby(~age+~female.born+~male.born+~male.dead+~female.dead+total.dead+~stillbirth+total.children+no.anc.visit+~anc.doctor+~anc.midwife+~anc.nurse+~anc.chw
                            +~anc.tba+~anc.other+~anc.noone+~BA.doctor+~BA.midwife+~BA.chw+~BA.tba+~BA.noone+~BA.other+~tt.injection+~no.tt.intections
                            ,~province,design=df.dhs,svytotal,na.rm=TRUE))

temp.1 =as.data.frame(svyby(~age+female.born+male.born+male.dead+female.dead+total.dead+stillbirth+total.children+no.anc.visit+anc.doctor+tt.injection
                            +no.tt.intections
                            ,~province,design=df.dhs,svytotal,na.rm=TRUE))

temp.1 = temp.1[,-(14:25)]

temp_dhs
write.csv(temp_dhs, "temp_dhs.csv")
#temp.2 = read.csv("C:\\Users\\ms52\\Desktop\\ESOC Temp files\\temp_dhs.csv")


b = as.data.frame(svyby(~BA.nurse, ~province, df.dhs, svytotal,na.rm =  TRUE,  keep.var=TRUE))
temp.1 = merge(temp.1, b, by = "province")
temp.1

drops <- c("se.x","se.y")
temp.1 = temp.1[ , !(names(temp.1) %in% drops)]

#total = merge(temp_dhs, temp.1, by = "ProvName")
write.csv(total, "total_dhs.csv")



map$Province_Name = tolower(map$Province_Name)
table(map$Province_Name)
table(total$province)


total = read.csv("C:\\Users\\ms52\\Desktop\\ESOC Temp files\\NRVA\\total_dhs.csv")


head(total)

####Merging the two datasets together

map = subset(map,!duplicated(map$Province_Name))

colnames(temp)[2] = "ProvName"
colnames(temp)[26] = "province"
temp = merge(temp, map , by ="ProvName", all = T )

head(temp)
colnames(temp)

final = merge(total, temp, by = "province")

rownames(final) = final$province

final = final[,-1]
head(final)
library(corrplot)
M <- cor(final)
corrplot(M, method="circle")

library(Hmisc)

res2<-rcorr(as.matrix(final[]))
            
write.csv(final, "comparison_final_NRVA_DHS.csv") 

final = final[, -22]

final = final = 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

colnames(final)
a = as.data.frame(flattenCorrMatrix(res2$r, res2$P))
