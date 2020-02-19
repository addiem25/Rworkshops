# Set working directory (change to where your file is located)
setwd("../Desktop/Rworkshop/linear modeling")

# Read in dataset (I added the abs2-abs1 value and changed "%" to "Percent_",
# but otherwise it's the same as what you sent)
mapdat = read.csv("mapping_pop_data.csv", header=T)

# Examine imported data
str(mapdat)
head(mapdat)
tail(mapdat)

# Attach dataset
attach(mapdat)

# Examine distribution of response data and its change by variables
hist(Percent_Digested)
hist(Abs2_1)
plot(Percent_Digested~Abs2_1)
plot(Percent_Digested~Absorbance2)
plot(Abs2_1~Absorbance2)
# I saw what you were talking about in the above plots; seems to indicate Abs2_1
# is indeed better than Percent_Digested so that's what I use for the remainder
# of the script but this could be altered.
boxplot(Abs2_1~Row)
boxplot(Abs2_1~Column)
boxplot(Abs2_1~Date)
boxplot(Abs2_1~Plate)

# Rename variables for ease of use
# Could use Percent_Digested instead of Ab2_1
DIGESTIBILITY = as.numeric(Abs2_1)
LINE=as.factor(Individual)
ROW=as.factor(Row)
COL=as.factor(Column)
DATE=as.factor(Date)
PLATE=as.factor(Plate)

# Do basic ANOVA to look at significance of factors
anova(lm(DIGESTIBILITY~LINE+ROW+COL+PLATE))

# Row is not significant; will exclude this from future calculations

# Calculate variance components
# requires lme4 package -- go to menu Packages->Install Packages...
# to install, and then load using:
library(lme4)

# Linear Model with random effects for variance components
digvarcomp<-lmer(DIGESTIBILITY~(1|LINE)+(1|COL)+(1|PLATE)+(1|LINE:COL)+(1|LINE:PLATE))
# Extract variance components
summary(digvarcomp)

# If you re-run with different models and maximize
# either the REML criterion or the heritability estimates,
# the best results are to use only LINE as a predictor,
# ie just use line means instead of BLUPs. I will continue
# with the BLUP calculations using COL and PLATE since they
# were significant, but this is something to think about.
# I have also included calculation of line means and the
# comparison of the top/bottom line selections using
# BLUP vs line mean. Which you use is ultimately up to you!

# BLUPs
# fit the model
digmodel=lmer(DIGESTIBILITY~(1|LINE)+(1|COL)+(1|PLATE)+(1|LINE:COL)+(1|LINE:PLATE))

# pred BLUPS
digblup=ranef(digmodel)
# look at output structure
str(digblup)
# extract blup for line
diglineblup=digblup$LINE
# see the structure of the blup for each line
str(diglineblup)
# save the output
write.csv(diglineblup, file="Digestibility_Line_BLUPS.csv")

# Creating plots with the BLUPs
# Create a numeric vector with the BLUP for each line
LINEBLUP = diglineblup[,1]
# Create a histogram with the BLUP for each line
hist(LINEBLUP)

# Compare BLUP to line means on a scatterplot
lmean=tapply(DIGESTIBILITY,LINE,mean)
plot(LINEBLUP,lmean)

# Compare top/bottom selections made by BLUP to those of line means
lmean<-lmean[order(lmean)]
diglineblup<-diglineblup[order(diglineblup[1]),,drop=FALSE]
botmean<-head(lmean,100)
topmean<-tail(lmean,100)
botblup<-head(diglineblup,100)
topblup<-tail(diglineblup,100)
length(intersect(rownames(topmean),rownames(topblup)))
length(intersect(rownames(botmean),rownames(botblup)))

# Can do the same for top/bot 20 instead of top/bot 100
bot20mean<-head(lmean,20)
top20mean<-tail(lmean,20)
bot20blup<-head(diglineblup,20)
top20blup<-tail(diglineblup,20)
length(intersect(rownames(top20mean),rownames(top20blup)))
length(intersect(rownames(bot20mean),rownames(bot20blup)))

# Can write any of these datasets to a file using this or similar:
write.csv(topblup,file="Top 100 BLUP.csv")
write.csv(botblup,file="Bot 100 BLUP.csv")
write.csv(topmean,file="Top 100 lmeans.csv")
write.csv(botmean,file="Bot 100 lmeans.csv")


