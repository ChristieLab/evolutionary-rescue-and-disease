#=============================================================================================================#
# Script created by Mark Christie, all rights reserved, contact at markchristie1500@gmail.com
# Script created in version R 4.0.0 on xx/xx/2020
# This script:
# Usage notes: 
#============================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
#library("")

#source("~/Drive/scripts.R", sep = ''))

setwd("C:/Users/Mark Christie/Dropbox/manuscripts/In_Review/evolutionary_rescue_review/model/chytrid2.8/output")
list.files()

dat <- read.table("Output.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
#dat <- read.table("Output_stochastic.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("i", "n.gen", "plength", "pwidth", "n.species.set", "n.loci.set", "n.allele.set", "n.genotypes.set", "n.adults.set", "n.tads.set", "k.adults", "k.tads", "n.eggs", "adult.survival.var", "tadpole.survival.var", "proportion.metamorph", "r", "days", "threshold", "host.scepts.min", "host.scepts.max", "n.generations", "n.replicates", "n.gtypes", "heterozygosity.expected", "heterozygosity.observed","p.polymorphic", "suscepts", "n.individs", "n.species", "n.tads", "n.tads.inf", "n.tads.uninf", "n.adults", "n.adults.inf", "n.adults.uninf", "n.inf", "n.uninf")
head(dat)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

length(dat[, 1])
plot(-10, -10, xlim = c(0, 300), ylim = c(0, max(dat$n.adults) +10))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 300) {points(length(dat2[, 1]), lyr$n.adults, pch=19, col="red", cex =2)}
  if(length(dat2[, 1]) == 300 & lyr$n.inf > 0) {points(length(dat2[, 1]), lyr$n.adults, pch=19, col="purple", cex =2)}  
  if(length(dat2[, 1]) == 300 & lyr$n.inf == 0) {points(length(dat2[, 1]), lyr$n.adults, pch=19, col="blue", cex =2)}
}

#=======================================#
length(dat[, 1])
plot(-10, -10, xlim = c(0, 150), ylim = c(0, max(dat$n.adults) +10), xlab = "Years", ylab = "Adult population size")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="red", lwd =2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {lines(1:length(dat2[, 1]), dat2$n.adults, col="purple", lwd =2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="blue", lwd =2)}
}









