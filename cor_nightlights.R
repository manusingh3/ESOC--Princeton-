b = aggregate(d$MISTI_W1_D9~ d$name, FUN = "sum")

d = read.csv(file.choose())

nrva = read.csv(file.choose())
nrva = na.omit(nrva)

colnames(b ) = c("PROV_34_NA", "misti")
temp = merge(d, b , by = "PROV_34_NA")

trial = aggregate(temp$PROV_34_NA~ temp$misti, FUN = "mean")

nrva = read.csv(file.choose())
c = aggregate(nrva$nrva~ nrva$Province_Name, FUN = "sum")
colnames(c) = c("PROV_34_NA", "nrva")

temp = merge(d , c, by = "PROV_34_NA")

b = aggregate(temp$PROV_34_NA~temp$tot_rad, FUN = "sum")
temp$PROV_34_NA = as.factor(temp$PROV_34_NA)

b = aggregate(d$norm_rad~d$PROV_34_NA, FUN = "mean"); colnames(b) = c("prov", "rad")
c = aggregate(nrva$nrva~nrva$Province_Name, FUN = "sum" ) ;colnames(c) = c("prov", "nrva")



# c$prov = ifelse(c$prov == "Sar-e-Pul", "Sari Pul", c$prov)
# c$prov = ifelse(c$prov == "Sar-e-Pul", "Sari Pul", c$prov)

c$prov = as.character(c$prov)

c$prov[c$prov =="Sar-e-Pul"] = "Sari Pul"
c$prov[c$prov =="Urozgan"] = "Uruzgan"
c$prov[c$prov =="Helmand"] = "Hilmand"
c$prov[c$prov =="Herat"] = "Hirat"
c$prov[c$prov =="Kunarha"] = "Kunar"




table(c$prov);table(b$prov)



total = merge(b, c , by = "prov")

cor(total$rad, total$nrva)

misti = read.csv(file.choose())

total = merge(total, misti, by = "prov")

dhs = read.csv(file.choose())
head(dhs)
colnames(dhs) = c("wi_country", "q_country", "wi_urban", "q_urban","wi_rural", "q_rural", "prov")

colnames(dhs) = c("prov", "dhs")
table(b$prov);table(dhs$prov)

test$prov = as.character(test$prov)

test$prov[test$prov =="helmand"] = "hilmand"
test$prov[test$prov =="herat"] = "hirat"
test$prov[test$prov =="urozgan"] = "uruzgan"


test = aggregate(df$wi_country~df$prov, FUN = "mean")
test.1 = aggregate(df$wi_urban~df$prov, FUN = "mean")

test = merge( test.1,test, by = "df$prov", all = TRUE)
colnames(test) = c("prov", "urban","rural", "country")
table(b$prov);table(test$prov)
dhs.cor = merge(test, b, by = "prov")
write.csv(dhs.cor, file = "dhs_cor.csv")

#######DHS 

rownames(dhs.cor) = dhs.cor$prov
 dhs.cor = dhs.cor[, -1]
dhs.cor

dhs.cor = data.matrix(dhs.cor)

dhs <- heatmap(dhs.cor, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

m = cor(dhs.cor, use="pairwise.complete.obs")

library(corrplot)
corrplot(m, method="pie")



####################New changes 21 Sep #######
## Understanding residuals wrt to violence 
#####check if relationships exits between violence and nightlights
# load preprocessed data for sigacts 2011

df = read.csv(file.choose())




rownames(df) = df$X
df = df[,-1]
par(mar=c(8,8,8,8))

m = cor (df, use = "pairwise.complete.obs")
corrplot(m, method = "number")

corrplot.mixed(m)


#####correlation combined with significance test
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}



res1 = cor.mtest(m , 0.95)

corrplot(m, p.mat = res1[[1]], sig.level=0.5)



corrplot(m, p.mat = res1[[1]], insig = "p-value")
#### centering data

df$sig_scale = scale(df$sigacts, center = TRUE, scale = FALSE)

#####scatter plot and linear regression
library(ggplot2)
p <- ggplot(df, aes(log_sig_pc, resid1))
p+geom_point()
p+geom_smooth(method = "lm")+geom_point()
       

lm1 = lm(log_rad_pc~country, data = df)
resid1 = resid(lm1)


plot(df$log_sig_pc, resid1, 
          ylab="Residuals", xlab="Violent events", 
           main="Residuals of violence- Normalised") 
abline(lm(df$log_sig_pc~resid1))

#####Normalised by population

p <- ggplot(df, aes(sig_scale, log(norm_rad)))
p+geom_point()
p+geom_smooth(method = "lm")+geom_point()


lm2 = lm(norm_rad~sig_scale, data = df)
resid2 = resid(lm2)


plot(df$sigacts, resid2, 
     ylab="Residuals", xlab="Violence", 
     main="Residuals of violence- Normalised") 
abline(0, 0)
