if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("multtest")
install.packages("gplots")
install.packages("LDheatmap")
install.packages("genetics")
install.packages("ape")
install.packages("EMMREML")
install.packages("scatterplot3d")
library(multtest)
library(gplots)
library(LDheatmap)
library(genetics)
library(ape)
library(EMMREML)
library(compiler) #this library is already installed in R
library("scatterplot3d")
source("http://zzlab.net/GAPIT/gapit_functions.txt")
source("http://zzlab.net/GAPIT/emma.txt")
geno_demo <- read.delim("mdp_genotype_test.hmp.txt",header = FALSE)
dim(geno_demo)
View(geno_demo[1:10,1:20])
pheno_demo <- read.delim("mdp_traits.txt",header = TRUE)
dim(pheno_demo)
View(pheno_demo[1:15,])
sum(pheno_demo$Taxa%in%geno_demo[1,12:292])



#run GAPIT
myGAPIT <- GAPIT(
  Y=pheno_demo,
  G=geno_demo,
  PCA.total = 3,
  model = c("GLM","MLM","MLMM","FarmCPU")
)


library(rMVP)
#phenotype histogram
MVP.Hist(phe=pheno_demo,file.type = "jpg",breakNum = 10,dpi = 300)
#SNP density
geno_mvp <- read.delim("mdp_genotype_test.hmp.txt",header = TRUE)
MVP.Report(geno_mvp[, c(1,3,4)], plot.type="d", col=c("darkgreen", "yellow", "red"), file.type="jpg", dpi=300)
#Manhattan plot and QQ plot
Dia_MLMM <- read.csv("GAPIT.MLMM.EarDia.GWAS.Results.csv",header = TRUE)
head(Dia_MLMM)
colnames(Dia_MLMM)[4] <- c("Dia_MLM")
Dia_FarmCPU <- read.csv("GAPIT.FarmCPU.EarDia.GWAS.Results.csv",header = TRUE)
head(Dia_MLMM)
colnames(Dia_FarmCPU)[4] <- c("Dia_FarmCPU")

Dia <- merge(Dia_MLMM[,1:4],Dia_FarmCPU[,c(1,4)],by="SNP")%>%arrange(Chromosome)
head(Dia)
View(Dia)
MVP.Report(Dia, plot.type="m", multracks=TRUE, threshold=c(1e-6,1e-4),threshold.lty=c(1,2), 
           threshold.lwd=c(1,1), threshold.col=c("black","grey"), amplify=TRUE,bin.size=1e6,
           chr.den.col=c("darkgreen", "yellow", "red"), signal.col=c("red","green"),signal.cex=c(1,1),
           file.type="jpg",memo="",dpi=300)

MVP.Report(Dia,plot.type="q",col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1"),threshold=1e6,
           signal.pch=19,signal.cex=1.5,signal.col="red",box=FALSE,multracks=
             TRUE,file.type="jpg",memo="",dpi=300)










