setwd("../Desktop/Rworkshop/linear modeling")

#load data; needs to have columns called "Genotype", "Row", "Range", "Rep"
leaf6data<-read.csv("leaf6area.csv",head=T)

#make factors
leaf6data$Rep<-as.factor(leaf6data$Rep)
leaf6data$Row<-as.factor(leaf6data$Row)
leaf6data$Range<-as.factor(leaf6data$Range)

#remove plots with leaves that are too different
leaf6data<-leaf6data[which(leaf6data$QC=="keep"),]

#make linear model to predict trait based on Geno + Rep
repmod<-lm(leaf6data$leafave~
             leaf6data$Genotype+
             leaf6data$Rep)
anova(repmod)

#make linear model to predict trait based on Geno + Row + Range
rrmod<-lm(leaf6data$leafave~
            leaf6data$Genotype+
            leaf6data$Row+
            leaf6data$Range)
anova(rrmod)

#in this case, none of Rep, Row, Range are significant, only Genotype
#so we should proceed with only Genotype in the model

gmod<-lm(leaf6data$leafave~
           leaf6data$Genotype)
anova(gmod)

#show simple calculation of heritability
anova(gmod)$'Mean Sq'[1]/sum(anova(gmod)$'Mean Sq')

#how to access fitted values?
gmod$fitted.values #this will contain one value per (non-NA) row
#or do we want coefficients?
gmod$coefficients #this will contain one value per level of factors
#but really they tell us the same thing
gmod$fitted.values[leaf6data$Genotype=="PI601572"]
gmod$coefficients[names(gmod$coefficients)==
                    "leaf6data$GenotypePI601572"]+
  gmod$coefficients["(Intercept)"]
#if we had more than one factor (e.g. row, range, etc.) we would need
#to add all of the respective factors' coefficients to get the fitted values
#Sometimes we may only be interested in the genetic coefficients and we could
#do downstream analysis directly on these

