install.packages("qtl2", repos="https://rqtl.org/qtl2cran")
library(qtl2)

#Load data on gravitropism in a set of Arabidopsis recombinant inbred lines 
#from Moore et al. (2013) Genetics 195:1077-1086
grav2 <- read_cross2("https://kbroman.org/qtl2/assets/sampledata/grav2/grav2.zip")

#insert pseudomarkers into genetic map
map <- insert_pseudomarkers(grav2$gmap, step=1)

#calculate genotype probabilities
pr <- calc_genoprob(grav2, map, error_prob=0.002)

#can also use allele probabilities
apr <- genoprob_to_alleleprob(pr)

#calculate kinship matrix to use as covariate to account for
#relationships among individuals
kinship <- calc_kinship(pr)

#alternatively, calculate kinship matrix using only gridded markers
#to eliminate effects of uneven marker distribution
grid <- calc_grid(grav2$gmap, step=1)
pr_grid <- probs_to_grid(pr, grid)
kinship_grid <- calc_kinship(pr_grid)

#another alternative, calculate kinship matrix for each chromosome
#using data from all other chromosomes ("Leave One Chromosome Out")
kinship_loco <- calc_kinship(pr, "loco")

#perform genome scan
out <- scan1(pr, grav2$pheno)

#LOD plot of one timepoint
par(mar=c(5.1, 4.1, 1.1, 1.1))
ymx <- maxlod(out) # overall maximum LOD score
plot(out, map, lodcolumn=119, col="slateblue", ylim=c(0, ymx*1.02))

#find all peaks
find_peaks(out, map, threshold=4, drop=1.5)

#get bayes credible interval
bayes_int(out, map, lodcolumn=119, chr=3, prob=0.95)
#or LOD support interval
lod_int(out, map, lodcolumn=119, chr=3, peakdrop=1.8)

#run using linear mixed model and account for kinship
out_pg <- scan1(pr, grav2$pheno, kinship)
out_pg_loco <- scan1(pr, grav2$pheno, kinship_loco)

#compare no kinship to regular to loco
color <- c("slateblue", "violetred", "green3")
par(mar=c(4.1, 4.1, 1.6, 1.1))
ymx <- max(maxlod(out), maxlod(out_pg), maxlod(out_pg_loco))
plot(out, map, lodcolumn=119, col=color[1], 
     main=colnames(grav2$pheno)[119], ylim=c(0, ymx*1.02))
plot(out_pg, map, lodcolumn=119, col=color[2], add=TRUE)
plot(out_pg_loco, map, lodcolumn=119, col=color[3], add=TRUE, lty=2)
legend("topleft", lwd=2, col=color, c("H-K", "LMM", "LOCO"), 
       bg="gray90", lty=c(1,1,2))

#perform permutations to test significance
operm <- scan1perm(pr, grav2$pheno[,"T236"], kinship_loco, n_perm=1000)
summary(operm)

#plot phenotypes vs genotypes at specific loci
g <- maxmarg(pr, map, chr=3, pos=14, return_char=TRUE)
par(mar=c(4.1, 4.1, 0.6, 0.6))
plot_pxg(g, grav2$pheno[,"T236"], ylab="Phenotype")

#include +/-2 SE intervals
par(mar=c(4.1, 4.1, 0.6, 0.6))
plot_pxg(g, grav2$pheno[,"T236"], SEmult=2, swap_axes=TRUE, xlab="Phenotype")
