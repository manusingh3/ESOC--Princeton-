---
title: "Child Mortality 2005-2012"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This is a prelimnary draft examining child mortality in Afghnistan using the NRVA (National Risk and Vulnerability Assessment) data from the years 2005 to 2012. Afghanistan has very high fertility level (CITE AAAA) underlying a young population composition. this implies higer risk for maternal and child mortality escpecially in areas which have poor access to health care in the country. The NRVA does not provide sufficent data to track maternal mortalut rate over the years but under 5 child mortalty rate can be traced from 2005 to 2012 and this is showing a downward trend (REF later section XX)

The Under 5 Mortality rate (U5MR) is defined as the number of death per 1000 liver births

```{r}
cm2012 = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/cm2012.csv")
cm2007 = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/cm2007.csv")
cm2005 = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/cm2005.csv")
```
## Survey Weighted aggregation

```{r}
library(survey)

cm2012$cm = (cm2012$Number.of.boys.born + cm2012$Number.of.girls.born - cm2012$Number.of.boys.alive- cm2012$Number.of.girls.alive  )/ (cm2012$Number.of.boys.born+ cm2012$Number.of.girls.born)

cm2012$cm = (cm2012$cm *cm2012$Household.weight)/1000

cm2012$id = rownames(cm2012)
head(cm2012)
 cm2012 = na.omit(cm2012)
 
 a = as.data.frame(tapply(cm2012$Number.of.boys.born, cm2012$Province.1, sum))
 a$province = rownames(a)
 
 b = as.data.frame(tapply(cm2012$Number.of.girls.born, cm2012$Province.1, sum))
 b$province = rownames(b)
 
 c = as.data.frame(tapply(cm2012$Number.of.boys.alive, cm2012$Province.1, sum))
 c$province = rownames(c)
 
 d = as.data.frame(tapply(cm2012$Number.of.girls.alive, cm2012$Province.1, sum))
 d$province = rownames(d)
 
 total = merge (a,b)
 
 total = merge (total, c)
 
 total = merge (total, d)
 
 colnames(total) = c("province", "boys_born","girls_born", "boys_alive", "girls_alive")
 
 total$cm_2012 = (total$boys_born+total$girls_born-total$boys_alive-total$girls_alive)/(total$boys_born+total$girls_born)*1000
 
 mean(total$cm_2012)

# #cm2012$cm = cm2012$cm*1000
# cm <- 
# 	svydesign( 
# 		ids = cm2012$id , 
# 		data = cm2012 , 
# 		weights = ~Household.weight , 
# 	)
# 
# aggregate(cm2012$Number.of.boys.born, by = list(Province.1), FUN = sum)
# 
# svymean(~cm, cm)
# 
# svytotal(~cm, cm)

a = sum(((cm2012$Number.of.boys.born + cm2012$Number.of.girls.born - cm2012$Number.of.boys.alive- cm2012$Number.of.girls.alive)))

b = sum(cm2012$Number.of.boys.born + cm2012$Number.of.girls.born)

a/b*1000

total
```
In the section above we see the child mortality  aggregated by provinces. The overall child mortality for this period is estimated to be 94.8 deaths per 1000 liver births.

To cross validate this measure we have the child morta;lity levels from the DHS survey. Although the DHS survey was published in 2010 and the NRVA data we have is for 2011-2012. The available DHS survey is the closest and still useful for cross validating our results. Note- @ Radha - Although the levels at the province level do not correlate with the the NRVA survey (they seem to be normalised). So interdistrict variance is reduced I suppose by virtue of weight. But the country level value is 132.9 which is inbetween the 2007 value of 153.8 (published value for that year was 161, data gives ~154) and 94.5 in 2012 ( published value of ~98). 

```{r}
#comparison to DHS data

dhs = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/DHS Child_mortality.csv")
dhs$sample.weight..6.decimals. = dhs$sample.weight..6.decimals./1000000

cm.dhs <- 
	svydesign( 
		ids = ~X , 
		data = dhs , 
		weights = ~sample.weight..6.decimals.
	)

# a = svytotal(~total.children.ever.born, cm.dhs)
# b = svytotal(~dead_children, cm.dhs)

a =as.data.frame(   svyby(~total.children.ever.born, ~province, cm.dhs, svytotal))
b =as.data.frame(   svyby(~dead_children, ~province, cm.dhs, svytotal))

cross = merge(a,b, by = "province")
cross$cm = (cross$dead_children/ cross$total.children.ever.born)*1000

drops <- c("se.x","se.y")
cross = cross[ , !(names(cross) %in% drops)]

sum(cross$dead_children)/sum(cross$total.children.ever.born)
cross = 

total$province = tolower(total$province)

cross = merge(total, cross , by = "province")

cross$ch_born_2012 = cross$boys_born+cross$girls_born

cross$ch_dead_2012 = (-cross$boys_alive-cross$girls_alive+cross$boys_born+cross$girls_born)

cross = cross[, -c(2,3,4,5)]

rownames(cross) = cross$province

cross = cross[, -c(1)]
cross

# library(corrplot)
# M <- cor(cross)
# corrplot(M, method="circle")

```




##Similar analysis done for child mortality for the rest of the years

```{r}

cm2007 = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/cm2007.csv")

head(cm2007)


cm2007 = cm2007[,-(15)]



cm2007$Q_17_9_Total = (ifelse(is.na(cm2007$Q_17_9_Total), 0, cm2007$Q_17_9_Total))

cm2007 = na.omit(cm2007)


cm07 <-
	svydesign(
		ids = ~X ,
		data = cm2007 ,
		weights = ~(HH_Weight))

a = as.data.frame(  svyby(~Q_17_9_Total,~Province_Name,  cm07, svytotal))
b = as.data.frame(  svyby(~Q_17_10_Total,~Province_Name,  cm07, svytotal))

temp = merge (a, b , by = "Province_Name")

drops <- c("se.x","se.y")
temp = temp[ , !(names(temp) %in% drops)]
temp$Province_Name = tolower(temp$Province_Name)

temp$cm_2007 = temp$Q_17_9_Total/temp$Q_17_10_Total

mean(temp$cm_2007)

 temp.1 = as.data.frame(tapply(temp$cm_2007, temp$Province_Name, mean))
 temp.1$province = rownames(temp.1)
 colnames
 # 
 #  a = as.data.frame(tapply(temp$Q_17_9_Total,temp$Province_Name,  sum))
 # a$province = rownames(a)

write.csv(temp, file = "tempcm2007.csv")
temp = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/tempcm2007.csv")

temp 
total
total = total[, -c(2,3,4,5)]
colnames(total) = c("Province_Name", "cm_2012")

total = merge(total, temp, by = "Province_Name", all = T)
total

```


is the overall child mortality rate. 153.59


```{r}

#calculating the cm per district

a = as.data.frame(svyby(~Q_17_9_Total, ~Province_Name,cm07, svymean))

b = as.data.frame(svyby(~Q_17_10_Total, ~Province_Name,cm07, svymean))

temp = merge(a,b, by ="Province_Name" )

temp$cm = temp$Q_17_9_Total/temp$Q_17_10_Total*1000

head(total)

```

##analysing data for 2005

```{r}
####Writing previous data file 

write.csv(total, file= "province_cm.csv")

cm2005[is.na(cm2005)] <- 0

cm2005$total_child = (cm2005$first_women_no_birth_16_10+cm2005$third_women_no_birth_16_10+cm2005$second_women_no_birth_16_10)


cm2005$dead_child = (cm2005$first_women_boy_died_16_9+cm2005$first_women_daughter_died_16_9+cm2005$second_women_boy_died_16_9+cm2005$second_women_daughter_died_16_9+cm2005$third_women_daughter_died_16_9+cm2005$third_women_boy_died_16_9)

head(cm2005)

sum(cm2005$dead_child)

sum(cm2005$total_child)


cm05 <-
	svydesign(
		ids = ~hh_id ,
		data = cm2005 ,
		weights = ~HH_weight)


a =as.data.frame(   svyby(~total_child, ~ProvName, cm05, svytotal))
b =as.data.frame(   svyby(~dead_child, ~ProvName, cm05, svytotal))

temp = merge (a, b , by = "ProvName")
temp$cm_2005 = temp$dead_child/ temp$total_child

map = read.csv(file.choose())

map = map[, -c(1,2,4,5)]
colnames(map) = c("Province_Name","ProvName")

head(temp)

temp = merge(temp, map, by = "ProvName")

head(temp)

temp = temp[, -c(1,3,5)]

temp$cm05 = temp$dead_child/temp$total_child

test = as.data.frame(tapply(temp$cm05, temp$Province_Name, mean))
test$ProvName = rownames(test)
test

colnames(test) = c("cm_2005", "Province_Name")

test$cm_2005 = test$cm_2005*1000
test$Province_Name = tolower(test$Province_Name)

test

write.csv(test, file = "tempcm2005.csv")
total = read.csv("C:/Users/ms52/Desktop/ESOC Temp files/NRVA/province_cm.csv")

temp = merge(total, test, by = "Province_Name")

write.csv(test, file = "province_cm.csv")
# 
# test = as.data.frame(tapply(cm2005$total_child, cm2005$ProvName, sum))
# test$ProvName = rownames(a)
# test = merge(temp, test, by = "ProvName")
# 
# cor(test$total_child, test$`tapply(cm2005$total_child, cm2005$ProvName, sum)`)
# 
#  
# 
# mean(temp$cm_2005, na.rm = T)
```



## Exploratory Analysis



