df = read.csv(file.choose())


g = ggplot(df, aes(x = (v), y= log(spend_capita)))

g + geom_point(alpha = 0.8, colour = "red", size = 4, shape = 17)+ geom_smooth(method = "lm", formula = y~x)+
                 geom_text(aes(y = log(spend_capita)+0.08, label = df$province))+
  xlab("Violence per capita")+ylab("Log of spend per capita")+theme_minimal()



g = ggplot(b, aes(x = (variable), y= log(value)))

g + geom_point(alpha = 0.8, colour = "red")+ facet_wrap(~province)

a = as.data.frame(tapply(df$acts, list(df$province,df$year), sum))
b = tapply(df$acts, list(df$province,df$year), sum)

a$province = rownames(a)

b = melt(a, id ="province" )

b$variable = as.Date(b$variable, format = "%Y%")


#############stephen Graph

library(zoo)
df = read.csv(file.choose())
 a = df
 
 
 
 a$dd <- as.yearmon(paste(df$year, df$month, sep = "-"))

 g = ggplot(a, aes(x = (dd), y= acts))
 
 g + geom_line(alpha = 0.8, colour = "red")+ facet_wrap(~district) +geom_smooth(method = "loess")
 
 g + geom_line (aes(colour = a$district), size = 1)
 
 
 
