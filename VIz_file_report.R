file = file.choose()

file = "C:\\Users\\ms52\\Dropbox (ESOC - Princeton)\\ESOC\\Afghanistan\\OAPA Lessons Learned Study\\Clean Data\\Nightlights_Eco_Ind\\consolidated_VIIRS_2014-2016_bcc.csv"

df = read.csv(file)

df$Year = as.character(df$Year)
df$Month = as.character(df$Month)

df$date <- as.Date(paste(df$Month, "01", df$Year, sep="_"),  format = "%m_%d_%Y")

head(df)
a = as.data.frame(table(df$date, df$DISTID))
colnames(a) = c("Time", "DISTID", "Count_mil_troop")


ggplot(df, aes(y = tot_rad, x= date))+geom_line()+facet_wrap(~PROV_34_NA)+scale_x_date(format = "%b-%Y")
g + geom_point()+facet_wrap(~PROV_34_NA)
  
  geom_smooth(method = "lm", formula = y~x)+facet_wrap(~year)
  
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
