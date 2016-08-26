# Health indicators from DHS files
library(foreign)

file = file.choose()
#file = "C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Raw Data\\DHS\\afch66fl\\UNDER5.DAT"
under5 = read.dta(file.choose())

a = attr(under5,"var.labels")
colnames(under5) = a
head(under5)
write.csv(under5, file = "dealth_other.csv")

b = (attr(under5, "label"))
b = as.data.frame(unlist(b))

                  
######Calculate maternal mortality rate use data women.csv##################                 
df = read.csv(file.choose())
# head(df)
# 
# a = colnames(df)
# sibs = df[,1149:1689]
# id = df[,1:40]
# 
sibs <- sibs[,colSums(is.na(sibs))<nrow(sibs)] ##### remove columns where all the values are NA
#df <- df[,colSums(is.na(df))<nrow(df)] # remove all NAs from the full dataset.

# sibs = cbind(id, sibs)

#test set##
test = svydesign(ids = sibs$cluster.number, data = sibs, weights = sibs$sample.weight..6.decimals.)
sibs$sex.of.sibling



#clean dataset is sibs
#remove data for all male siblings and recode female siblings as 1 and male siblings are recoded as 0. 
df = df[!(df$sex.of.sibling == 1),]

df$sex.of.sibling = ifelse((df$sex.of.sibling == 1), 0, 1)

for (i in 1:20){
  
  b = paste("sex.of.sibling.",i, sep = "")
  
  df[,b] = ifelse((df[,b] == 1), 0, 1)
  
  
  
}

# now we have only female siblings - consider only those female siblings which are not alive 

df$sibling.currently.alive = ifelse(df$sibling.currently.alive ==1, 0, 1)

for (i in 1:20){
  
  b = paste("sibling.currently.alive.",i, sep = "")
  
  df[,b] = ifelse((df[,b] == 1), 0, 1)
  
}

#  siblings in the age group of 15-49 are given the code 1 rest are coded as 0

df$age.of.sibling = ifelse((df$age.of.sibling > 14 & df$age.of.sibling < 49 ), 1, 0)
for (i in 1:20){
  
  b = paste("age.of.sibling.",i, sep = "")
  
  df[,b] = ifelse((df[,b] > 14 & df[,b] < 49 ), 1, 0)
  
}


# is the death due to maternity related causes 
df$died.while.pregnant = ifelse((df$died.while.pregnant ==1 ),1,0)

for (i in 1:11){
  
  b = paste("died.while.pregnant.",i, sep = "")
  
  df[,b] = ifelse((df[,b] ==1 ), 1, 0)
  
}

#######die during childbirth
df$diedduringchildbirth = ifelse((df$diedduringchildbirth ==1),1,0)
for (i in 1:11){
  
  b = paste("diedduringchildbirth",i, sep = "")
  
  df[,b] = ifelse((df[,b] ==1 ), 1, 0)
  
}



########died within two months of delivery 
df$diedwithin2monthsofdelivery = ifelse((df$diedwithin2monthsofdelivery ==1), 1, 0)
for (i in 1:11){
  
  b = paste("diedwithin2monthsofdelivery",i, sep = "")
  
  df[,b] = ifelse((df[,b] ==1 ), 1, 0)
  
}


############trying to reshape the data ##############
test = melt(df, id = c("cluster.number", "sample.weight..6.decimals.", "region","province","locality","urban.rural",
                       "sex.of.sibling", "sibling.currently.alive", "age.of.sibling", "died.while.pregnant"))
trial = cast(test,sex.of.sibling~ sibling.currently.alive)


# trying to create new datasets

# a = colnames(df)
# a = gsub("\\.","",a)
# colnames(df) = a
# test = reshape(df, varying = c(7:26), timevar = "femalesibling", direction = "long", sep = "", id = "X")
# row.names(test) = NULL
# test = reshape(test, varying = c(8:27), timevar = "sibling alive", direction = "long", sep = "")
# 
# test = subset(df, (sexofsibling ==1 & siblingcurrentlyalive ==1 &diedwhilepregnant ==1  ))
#                 (diedwhilepregnant ==1 | diedduringchildbirth ==1 | diedwithin2monthsofdelivery ==1))

total = data.frame()
###############new data set with all the women who died while pregnant ################### 
test = subset(df, df$died.while.pregnant ==1 )

final = test[, c("cluster.number","sample.weight..6.decimals.", "province", b )]

colnames(final) = c("c.number", "sample.weight", "province", "mat.death")


for (i in 1:11) {
  
  b = paste("died.while.pregnant.",i, sep = "")
test = subset( df, df[, b] ==1)

final = test[, c("cluster.number","sample.weight..6.decimals.", "province", b )]

colnames(final) = c("c.number", "sample.weight", "province", "mat.death")

total = rbind(final, total)
 }

#####################women who died during childbirth#######################
test = subset(df, df$died.during.childbirth ==1 )

final = test[, c("cluster.number","sample.weight..6.decimals.", "province", "died.during.childbirth" )]

colnames(final) = c("c.number", "sample.weight", "province", "mat.death")

total = rbind(final, total)

for (i in 1:11) {
  
  b = paste("died.during.childbirth.",i, sep = "")
  test = subset( df, df[, b] ==1)
  
  final = test[, c("cluster.number","sample.weight..6.decimals.", "province", b )]
  
  colnames(final) = c("c.number", "sample.weight", "province", "mat.death")
  
  total = rbind(final, total)
}

###############died two months after childbirth#############################

test = subset(df, df$died.within.2.months.of.delivery ==1 )

final = test[, c("cluster.number","sample.weight..6.decimals.", "province", "died.within.2.months.of.delivery" )]

colnames(final) = c("c.number", "sample.weight", "province", "mat.death")

total = rbind(final, total)


for (i in 1:11) {
  
  b = paste("died.within.2.months.of.delivery.",i, sep = "")
  test = subset( df, df[, b] ==1)
  
  final = test[, c("cluster.number","sample.weight..6.decimals.", "province", b )]
  
  colnames(final) = c("c.number", "sample.weight", "province", "mat.death")
  
  total = rbind(final, total)
}



####################creating new colum which  divides the deaths by total number of women in that province 

d = read.csv(file.choose())
d$count = 1
pop = tapply(d$count, df$province, sum)
pop = as.data.frame(pop)
pop$province = row.names(pop)
total = merge(total, pop, by = "province")
write.csv(total, file = "total.csv")


#####row transformations before calculating probabilities##########

total$mat.death = total$mat.death/total$pop
total$sample.weight = total$sample.weight/1000000


############survey design \

survey = svydesign(ids = total$c.number, data = total, weights = total$sample.weight)

a = as.data.frame(prop.table(svytable(~province, design = survey)))

a = as.data.frame(svytotal(~province , survey))
b = rownames(a)
b = gsub("province","",b)
a$province = b
head(a)
write.csv(a , file = "total_maternal_mortality_province.csv")

#######Deaths of women weighed by live births #####
d = read.csv(file.choose())
child = d[,112:135]

child$sum = rowSums(child[,1:23])
child$child.born.alive.or.dead = ifelse(child$child.born.alive.or.dead =="alive", 1,0)

for (i in 1:23){
  
  b = paste("child.born.alive.or.dead.",i, sep = "")
  
  child[,b] = ifelse((child[,b] =="alive" ), 1, 0)
  
}

child$sum = rowSums(child[1:20])
child$sum = rowSums(child, na.rm = TRUE)

prov = d$province
child = cbind(child, prov)
head(child)

alive.ch = as.data.frame(tapply(child$sum, child$prov, sum))

alive.ch$province = rownames(alive.ch)
head(alive.ch)
colnames(alive.ch) = c("alive.child", "province")
total = merge(total, alive.ch, by = "province" )
head(total)
total$mat.death = total$mat.death*total$pop
total$mat.death = total$mat.death/total$alive.child

a = (tapply(total$mat.death, total$province, sum))*100000
write.csv(a , file = "maternal death per 100000 live births.csv")


#######running correlation between the two ############

library(corrplot)
a = as.data.frame(a)
a$province = rownames(a)

d = read.csv(file.choose())
a = read.csv(file.choose()) # use a aftre running t apply with row names and only numberic data
head(a)
colnames(d) = c("province","X")
rownames(d) = d$province
d = merge(a, d , by = "province")
cor(d[,unlist(lapply(d, is.numeric))])
