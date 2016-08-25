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
for (i in 1:20) {
  
  b = paste("died.while.pregnant.",i, sep = "")
test = subset(df, df$died.while.pregnant ==1 )

final = test[, c("cluster.number","sample.weight..6.decimals.", "province", b )]

colnames(final) = c("c.number", "sample.weight", "province", "mat.death")

 }

total = rbind(final, total)
