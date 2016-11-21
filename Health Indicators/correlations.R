###checking correlations between ANQAR, NRVA files, reshaping data

file = file.choose()

df = read.csv(file)

a = tapply(df$qnew325, df$province, mean)

a = df$qnew325

df$qnew325 = factor(df$qnew325,levels=c("very bad", "bad", "fair", "good", "very good"),ordered=TRUE)

df$qnew325 = as.numeric(df$qnew325)


a = tapply(df$qnew325, df$province, mean, na.rm =T)

cm_file = file.choose()
cm   = read.csv(cm_file)
cm = cm[1:34, ]

###survey design for anqar
df$id = rownames(df)

library(survey)

df = df[!is.na(df$qnew456),]

df.anqar <-
  svydesign(
    ids = df$id,
    data = df,
    weights = ~qnew456 ,
  )

b = as.data.frame(svyby(~qnew325, ~province, df.anqar, svymean,na.rm =  TRUE,  keep.var=TRUE))

a = as.data.frame(a)
a = a[-1,]
a$province = rownames(a)
total = merge(a, b, by = "province")


library(corrplot)

rownames(total) = total$province
total = total[,-1]
total
m = cor(total)
corrplot(m, method = "number")


#####checking against child mortality 

colnames(cm)[1] = "province"
total = merge (total, cm , by = "province")


#########checking correlation between years of child mortality 

library(reshape)
library(reshape2)

cm = cm[,-2]
test <- melt(cm, id.vars = c("Province_Name"), variable_name = "year",value.name = "child_mort")
head(test)
t = dcast(cm, Province_Name~year, value.var = "cm")
