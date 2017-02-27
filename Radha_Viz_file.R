file = file.choose()

file = "C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Clean Data\\Nightlights_Eco_Ind\\consolidated_DMSP_2008-2013_bcc.csv"

df = read.csv(file)
reg = read.csv("C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Raw Data\\GIS\\district398\\district398\\reg_cmd.csv")
#anq = read.csv("C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Clean Data\\ANQAR\\ANQAR_FULL_.csv")

df$time = as.character(df$time)
# df$Month = as.character(df$Month)
 df = merge(df, reg, by = c("DISTID", "PROVID"))

df$date <- as.Date(paste("01", "01", df$time, sep="_"),  format = "%m_%d_%Y")

head(df)

df = df[!(df$tot_rad <0.2 ),]
tapply(df$tot_rad, df$time, mean)
df$deviation = (df$tot_rad - 722.4628)
# a = as.data.frame(table(df$date, df$DISTID))
# colnames(a) = c("Time", "DISTID", "Count_mil_troop")
colnames(df)[10] = "Regional Command"
# kan = df[df$PROVID == 3, ]
# 
# 
# kan = aggregate(kan$tot_rad, by =list(kan$time), FUN= mean)
# #kabul = aggregate(kabul$tot_rad, by =list(kabul$time), FUN= mean)
# kan$date <- as.Date(paste("01", "01", kan$Group.1, sep="_"),  format = "%m_%d_%Y")
# colnames(kan)[2] = c( "tot_rad")
# 
# 
# ggplot(df, aes(y = (tot_rad), x= date))+geom_smooth(aes( color = `Regional Command`), se = FALSE)+theme_bw()+ggtitle("Variation in Nightlights with Time")+
#   xlab("Time")+ylab("Total Radiance")+scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))
#  +theme(plot.title = element_text(size = 25, face = "bold"))+scale_color_brewer(palette = "Pastel1")
#  
#   
#    geom_text(data = NULL, x = 2010-01-01, y = 1500, label = "plot mpg vs. wt")
#   
#   
#   geom_point(data = kabul, x = date, y= kabul$tot_rad)
# 
# annotate("text", x = as.Date("2010-01-01"), y = 1580, label = "Parwan")+theme(plot.title = element_text(size = 25, face = "bold"))+scale_color_brewer(palette = "Pastel1")
# 
# 
# 
# 
# ggplot()+geom_smooth(df, aes(y = tot_rad, x= date, color = `Regional Command`), se = FALSE)+theme_classic()+
#   xlab("Time")+ylab("Total Radiance")+scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))
# 
# 
# 
#   geom_point(data = kabul, x = date, y= kabul$tot_rad)
# 
# 
# 
# 
# 
# ggplot(df, aes(y = (deviation), x= date))+geom_smooth( size = 1.1, alpha = 0.2)+geom_hline(aes(yintercept=0), color = "red", size = 1.2)+facet_wrap(~`Regional Command`) +theme_bw()
# +geom_hline(aes(yintercept = 0)))



ggplot(df, aes(y = (deviation), x= date))+geom_smooth( size = 1.1, alpha = 0.2)+
  geom_hline(aes(yintercept=0), color = "red", size = 1.2) +theme_bw()+annotate("text", x = as.Date("2010-01-01"), y =250, label = "National Average in 2008")+
  scale_color_brewer(palette = "Pastel1")+ggtitle("National Nightlight Trend with Time")+
  xlab("Time")+ylab("Total Radiance")+scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))+facet_wrap(~`Regional Command`)


test = df[!(df$PROVID ==1 ),]
tapply(test$tot_rad, test$time, mean)
test$deviation = (test$tot_rad - 571)
g2 = ggplot(test, aes(y = (deviation), x= date))+geom_smooth( size = 1.1, alpha = 0.2)+
  geom_hline(aes(yintercept=0), color = "red", size = 1.2) +theme_bw()+annotate("text", x = as.Date("2010-01-01"), y = 100, label = "National Average in 2008 without Kabul")+
  scale_color_brewer(palette = "Pastel1")+ggtitle("National Nightlight Trend with Time")+
  xlab("Time")+ylab("Total Radiance")+scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))+facet_wrap(~`Regional Command`)

multiplot(g1, g2, cols=1)





###### health Stuff


hea = read.csv("C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Clean Data\\ANQAR\\final_anqar_q325.csv")
colnames(hea)[5] = "DISTID"
test = merge(hea, reg, by = "DISTID")
head(test)

total = aggregate(test$qnew325, by = list(test$REG_CMD, test$yq), FUN = sum, na.rm = TRUE)
library(zoo)
test$date <- as.Date(as.yearqtr(test$yq, format = "%Yq%q"))
table( test$qnew325, test$REG_CMD, test$yq)

ggplot(test, aes(y =(qnew325), x= (date), color = qnew325, group = 1))+geom_line()+facet_wrap(~REG_CMD) +theme_bw()
 
a = as.data.frame(table(test$qnew325, test$REG_CMD, test$yq))
a$date <- as.Date(as.yearqtr(a$Var3, format = "%Yq%q"))
a = a[!(a$Var1 ==""),]
a = a[!(a$Freq ==0),]
a = a[!(a$Var1 =="dk\\refused"),]
head(a)
a$Var1 = as.character(a$Var1)
a$Var1 = ifelse(a$Var1 =="very bad", "bad", a$Var1)
a$Var1 = ifelse(a$Var1 =="very good", "good", a$Var1)
b = as.data.frame(aggregate(a$Freq, by = list(a$Var2, a$Var3) , FUN = sum))

write.csv(a, file = "temp.csv")

a$Var1 = ordered(a$Var1, levels = c("bad", "fair", "good"))
colnames(a) = c("Status", "Region", "Time", "Freq", "Date")
colnames(b) = c("Region", "Time", "tot")
test = merge(a, b , by = c("Region", "Time"))
head(test)
test$Percentage = test$Freq/test$tot

test2 = test[!(test$Status=="fair"),]

 ggplot(test, aes(y =(Percentage), x= (Date),color = Status)) +theme_bw()+
   geom_smooth(size = 1.2,se = FALSE, span = 1.5)+scale_fill_brewer(palette = "Set1")+
 scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))

 
 ggplot(test, aes(y =(Percentage), x= (Date))) +theme_bw()+scale_colour_brewer(palette = "Pastel1")+
   geom_smooth(data = test, aes(x= Date, y = Percentage), size = 1.2, color = "firebrick4", se= FALSE, span = 1.5)+
   geom_smooth(aes(color = `Region`),se = FALSE, span = 1.5)+facet_wrap(~Status)+
   scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))
 
   
   
   
   
   
   
   ggplot(df, aes(y = (tot_rad), x= date))+geom_smooth(aes( color = `Regional Command`), se = FALSE)+theme_bw()+ggtitle("Variation in Nightlights with Time")+
      xlab("Time")+ylab("Total Radiance")+scale_x_date( labels = date_format("%Y"), breaks =date_breaks("year"))
     +theme(plot.title = element_text(size = 25, face = "bold"))+scale_color_brewer(palette = "Pastel1")
# +facet_wrap(~REG_CMD)
# 
# 
# g + geom_point()+facet_wrap(~PROV_34_NA)+scale_x_date(format = "%b-%Y")
# 
# geom_smooth(method = "lm", formula = y~x)+facet_wrap(~year)

###########################################################################################
#top 10 most violent dist compo to least violent dist

most = c(2308,2302,101,2312,2415,2408,2401,2304)
most_data = df[df$DISTID %in% most, ]
head(most_data)
least = c(1121,1313,1117,1110,1215,1120,3406,1205)
least_data = df[df$DISTID %in% least, ]

ggplot(most_data, aes(y = tot_rad, x= date))+geom_point()+facet_wrap(~DIST_34_NA, scales = "free")+geom_smooth()

ggplot(least_data, aes(y = tot_rad, x= date))+geom_point()+facet_wrap(~DIST_34_NA, scales = "free")+geom_smooth()


####################################################
#QQ of violence and nightlights
library(gtools)
library(data.table)
file2 = file.choose()
violence = read.csv(file2)
#violence$quantile = quantcut(violence$acts, q=seq(0,1,by=0.25))
head(violence)

violence <- within(violence, quartile <- as.integer(cut(violence$acts, 
                                                        quantile(violence$acts, probs=0:5/5), include.lowest=TRUE)))

violence= violence[,-3]
colnames(df) = c("DISTID", "tot_rad", "PROV_34_NA", "DIST_34_NA","PROVID", "Month", "Year", "date")
colnames(violence)[1] = c("DISTID")

total = merge(df, violence, by = "DISTID")
head(total)

ggplot(total, aes(y = tot_rad, x= date,  color = factor(quartile)))+geom_smooth(size = 1)+ggtitle("Nightlight levels split by quintiles of Violence")

#### controlling for mil presence 
file1 = file.choose()
orbat = read.csv(file1)
head(orbat)
colnames(orbat)[2] = "date"
total = merge(total, orbat, by = c("DISTID", "date"))
write.csv(total, file = "int_file_viz.csv")

lm1 = lm(tot_rad~Count_mil_troop + acts, data = total )
summary(lm1)


ggplot(total, aes(x=acts, y=tot_rad)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(lm(tot_rad ~Count_mil_troop + acts, data = total) )



############plotting regression results
ggplotRegression <- function (fit) {
  
  library(ggplot)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

lm1 <- lm(tot_rad~acts+Count_mil_troop , data = total)
ggplotRegression(lm1)





########################Nightlghts reference


