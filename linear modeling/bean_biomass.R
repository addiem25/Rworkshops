#set a working directory
setwd("C:/Users/addie/Desktop/Rworkshop/linear modeling")
#read in data
qcdat<-read.csv("bean_biomass.csv",head=T)
#check structure
str(qcdat)
#change some columns to be read as factors not numbers
qcdat$pass<-as.factor(qcdat$pass)
qcdat$range<-as.factor(qcdat$range)
qcdat$nitrogen<-as.factor(qcdat$nitrogen)
qcdat$rep<-as.factor(qcdat$rep)
#do the values make sense? let's look at H5_wetbundle
hist(qcdat$H5_wetbundle,xlab="grams",main="Harvest 5 wet bundle")
plot(qcdat$H5_wetbundle,ylab="grams",main="Harvest 5 wet bundle")
#ack! there is a weird pattern in the data, what explains this?
plot(qcdat$H5_wetbundle,ylab="grams",main="Harvest 5 wet bundle",
     col=qcdat$nitrogen)
plot(qcdat$H5_wetbundle,ylab="grams",main="Harvest 5 wet bundle",
     col=qcdat$pedigree1)
#is this the same for dry weight?
plot(qcdat$H5_stemdry+qcdat$H5_leafdry,ylab="grams",
     main="Harvest 5 dry weight",col=qcdat$pedigree1)
#or height?
plot(qcdat$height,col=qcdat$pedigree1)
#interesting, this appears to be affected by N; let's quantify

#build a linear model for height
htmod<-lm(qcdat$height~qcdat$nitrogen+qcdat$pedigree1+
            qcdat$nitrogen:qcdat$pedigree1)
#let's check residuals
summary(htmod)
plot(fitted(htmod),resid(htmod))
plot(htmod)
#do anova
anova(htmod)
#yes, genotype and nitrogen and their interaction are both significant
#how are traits related? let's look at correlations
heatmap(cor(qcdat[,c(11:32)],use="pairwise.complete.obs"),
        symm=T,cexRow=.5,cexCol=.5)
#interesting. stems and bundle go together, but number of plants
#does not seem to be all that related to the biomass collected in
#a unit of area; let's test this on harvest 5
anova(lm(qcdat$H5_stemdry~qcdat$H5_nplants))
#nope, number of plants did not affect the dry weight; what about height?
anova(lm(qcdat$height~qcdat$H5_nplants))
#Ooh, so having more plant density leads to taller plants; how much?
summary(lm(qcdat$height~qcdat$H5_nplants))
plot(qcdat$height,qcdat$H5_nplants)
#but this isn't true at all timepoints, so it may be erroneous
summary(lm(qcdat$height~qcdat$H4_nplants))

#next steps:
#what other QC should be done?
#write a function to analyze all traits and output summary results
#what else?
#discuss real applications of data

