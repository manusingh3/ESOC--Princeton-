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
b = as.data.frame(unlist(b)

                  
######Calculate maternal mortality rate use data women.csv##################                 
df = read.csv(file.choose())
head(df)
a = colnames(df)
sibs = df[,1149:1689]
id = df[,1:40]
sibs <- sibs[,colSums(is.na(sibs))<nrow(sibs)] ##### remove columns where all the values are NA
