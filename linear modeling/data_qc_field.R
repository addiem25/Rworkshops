setwd("../Desktop/Rworkshop/linear modeling")

# read in data and examine
agdata<-read.csv("Combined_Data_15_02_19.csv",head=T)
str(agdata)
# In Excel's numeric dates, January 1, 1900 is valued as 1
dayzero<-as.Date("1900-01-01") - 1
# dates read as factors; must change this!
agdata$Date_of_Anthesis<-as.integer(as.character(agdata$Date_of_Anthesis))
agdata$Date_of_Germination<-as.integer(as.character(agdata$Date_of_Germination))
agdata$Date_of_Silking<-as.integer(as.character(agdata$Date_of_Silking))
str(agdata)

#let's check a few traits
summary(agdata$Date_of_Germination)
plot(agdata$Date_of_Germination)
plot(agdata[which(agdata$Year==2017 & agdata$Date=="Date1"),"Date_of_Germination"])

summary(agdata$Date_of_Anthesis)
#These trials should range 2017-2018, so 42734-43463
subset(agdata,Date_of_Anthesis<42734)
dayzero + 39226 #this is in 2007, should be 2017
as.Date("2017-05-25") - dayzero
agdata[which(agdata$Date_of_Anthesis==39226),"Date_of_Anthesis"]<-42879
dayzero + 39229 #also in 2007, should be 2018
as.Date("2018-05-28") - dayzero
agdata[which(agdata$Date_of_Anthesis==39229),"Date_of_Anthesis"]<-43247
summary(agdata$Date_of_Anthesis)
subset(agdata,Date_of_Anthesis>43463)
dayzero + 46523 #this is in 2027! beware of data from the future! should be 2018
as.Date("2018-05-17") - dayzero
agdata[which(agdata$Date_of_Anthesis==46523),"Date_of_Anthesis"]<-43236
summary(agdata$Date_of_Anthesis)
plot(agdata$Date_of_Anthesis)
plot(agdata[which(agdata$Year==2017),"Date_of_Anthesis"],ylab="Anthesis Date in 2017")
subset(agdata,Year==2017 & Date_of_Anthesis>42920)
#can also use identify()
identify(agdata[which(agdata$Year==2017),"Date_of_Anthesis"])
#must press Esc when done interacting with the graph
agdata[c(44,122,531,1417,1531,1576,1624),]

summary(agdata$Date_of_Silking) #again, should be within 42734-43463
subset(agdata,Date_of_Silking<42734)
dayzero + 39240 #2007 should be 2018
as.Date("2018-06-08") - dayzero
agdata[which(agdata$Date_of_Silking==39240),"Date_of_Silking"]<-43258
summary(agdata$Date_of_Silking)
plot(agdata$Date_of_Silking)
plot(agdata[which(agdata$Year==2018),"Date_of_Silking"])
identify(agdata[which(agdata$Year==2018),"Date_of_Silking"])
agdata[c(4,25,46,560,1181,1417),]

agdata$ASI<-agdata$Date_of_Anthesis-agdata$Date_of_Silking
summary(agdata$ASI)
plot(agdata$ASI)
largeASI<-subset(agdata,abs(agdata$ASI)>10,
                 select=c(1,grep("Date_",colnames(agdata)),18))
largeASI$Date_of_Germination<-dayzero + largeASI$Date_of_Germination
largeASI$Date_of_Anthesis<-dayzero + largeASI$Date_of_Anthesis
largeASI$Date_of_Silking<-dayzero + largeASI$Date_of_Silking
largeASI
#In this case, entries need to be flagged to compare to collected data.
#Sometimes we don't have original data to compare and must decide a threshold to remove -
#this should be based on a cutoff of some kind and must be documented/reproducible!

summary(agdata$Plant_Height)
plot(agdata$Plant_Height)
#values may be accurate, but there is a pattern, let's try to identify it
plot(agdata$Plant_Height,col=agdata$Trial)
#trials were not randomized, therefore we cannot use normal row-range effects.
#With field layouts, could use repeated checks only to model spatial variation
#across the field, then use that as a covariate to adjust values
#for other (non-check) varieties.
#Otherwise, this all becomes part of the error and reduces heritability
#as well as power to do genetic mapping

