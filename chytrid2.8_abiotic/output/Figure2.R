#=============================================================================================================#
# Script created by Mark Christie, all rights reserved, contact at markchristie1500@gmail.com
# Script created in version R 4.0.0 on xx/xx/2020
# This script:
# Usage notes: 
#============================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
#library("")

#source("~/Drive/scripts.R", sep = ''))

setwd("C:/Users/Mark Christie/Dropbox/manuscripts/In_Review/evolutionary_rescue_review/model/chytrid2.8_abiotic/output")
list.files()

dat <- read.table("Output_stochastic.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("i", "n.gen", "plength", "pwidth", "n.species.set", "n.loci.set", "n.allele.set", "n.genotypes.set", "n.adults.set", "n.tads.set", "k.adults", "k.tads", "n.eggs", "adult.survival.var", "tadpole.survival.var", "proportion.metamorph", "r", "days", "threshold", "host.scepts.min", "host.scepts.max", "n.generations", "n.replicates", "n.gtypes", "heterozygosity.expected", "heterozygosity.observed","p.polymorphic", "suscepts", "n.individs", "n.species", "n.tads", "n.tads.inf", "n.tads.uninf", "n.adults", "n.adults.inf", "n.adults.uninf", "n.inf", "n.uninf")
head(dat)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

# full =======================================#
#par(mfrow=c(1,3))

length(dat[, 1])
plot(-10, -10, xlim = c(0, 150), ylim = c(0, 240), xlab = "Years", ylab = "Adult population size")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")


OUT <- NULL
n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  #lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="orange3", lwd =2); out <- cbind("extinct", dat2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {lines(1:length(dat2[, 1]), dat2$n.adults, col="gray60", lwd =2); out <- cbind("coexist", dat2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="lightskyblue3", lwd =2); out <- cbind("clear", dat2)}
  colnames(out)[1] <- "result"
  OUT <- rbind(OUT, out)
}

# extinct =======================================================================================#
table(OUT[, 1])
extinct <- OUT[OUT[, 1] == "extinct", ]
(table(extinct[, 2]))
n.extinct <- length(table(extinct[, 2]))
ival <- unique(extinct[, 2])
ival

OUT2 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- rbind(dat2, 0)
  lines(1:length(dat2[, 1]), dat2$n.adults, col="orange3", lty=1, xlim = c(0, 150))
  dat2 <- dat2$n.adults
  if(length(dat2 < 150)){n.zeros <- 150-length(dat2); dat2 <- c(dat2, rep(0, n.zeros))}
  OUT2 <- cbind(OUT2, dat2)
  }

temp <- rowMeans(OUT2)
temp <- apply(OUT2, 1, median)
lines(1:68, temp[1:68], col="orange", lwd = 4)


# coexist =======================================================================================#
coexist <- OUT[OUT[, 1] == "coexist", ]
(table(coexist[, 2]))
n.coexist <- length(table(coexist[, 2]))
ival <- unique(coexist[, 2])
ival


OUT2 <- NULL
  for(i in ival){
    dat2 <- dat[dat[, 1] == i, ]
    dat2 <- dat2$n.adults
    OUT2 <- cbind(OUT2, dat2)
  }


temp <- rowMeans(OUT2)
#temp <- apply(OUT2, 1, median)
lines(1:150, temp, col="black", lwd = 4)


# clear =======================================================================================#
clear <- OUT[OUT[, 1] == "clear", ]
(table(clear[, 2]))
n.clear <- length(table(clear[, 2]))
ival <- unique(clear[, 2])
ival

OUT2 <- NULL
#for(n in 1:150){
#OUT2 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- dat2$n.adults
  OUT2 <- cbind(OUT2, dat2)
}


temp <- rowMeans(OUT2)
#temp <- apply(OUT2, 1, median)
#lines(1:150, temp, col="blue", lwd = 4)






#==Deterministic===========================================================================================#


#=============================================================================================================#
# Script created by Mark Christie, all rights reserved, contact at markchristie1500@gmail.com
# Script created in version R 4.0.0 on xx/xx/2020
# This script:
# Usage notes: 
#============================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
#library("")

#source("~/Drive/scripts.R", sep = ''))

setwd("C:/Users/Mark Christie/Dropbox/manuscripts/In_Review/evolutionary_rescue_review/model/chytrid2.8_abiotic/output")
list.files()

dat <- read.table("Output_deterministic.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("i", "n.gen", "plength", "pwidth", "n.species.set", "n.loci.set", "n.allele.set", "n.genotypes.set", "n.adults.set", "n.tads.set", "k.adults", "k.tads", "n.eggs", "adult.survival.var", "tadpole.survival.var", "proportion.metamorph", "r", "days", "threshold", "host.scepts.min", "host.scepts.max", "n.generations", "n.replicates", "n.gtypes", "heterozygosity.expected", "heterozygosity.observed","p.polymorphic", "suscepts", "n.individs", "n.species", "n.tads", "n.tads.inf", "n.tads.uninf", "n.adults", "n.adults.inf", "n.adults.uninf", "n.inf", "n.uninf")
head(dat)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

# full =======================================#

length(dat[, 1])
plot(-10, -10, xlim = c(0, 150), ylim = c(0, 240), xlab = "Years", ylab = "Adult population size")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")


OUT <- NULL
n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  #lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="orange3", lwd =2); out <- cbind("extinct", dat2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {lines(1:length(dat2[, 1]), dat2$n.adults, col="gray60", lwd =2); out <- cbind("coexist", dat2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="lightskyblue3", lwd =2); out <- cbind("clear", dat2)}
  colnames(out)[1] <- "result"
  OUT <- rbind(OUT, out)
}

# extinct =======================================================================================#
table(OUT[, 1])
extinct <- OUT[OUT[, 1] == "extinct", ]
(table(extinct[, 2]))
n.extinct <- length(table(extinct[, 2]))  # none went exting
ival <- unique(extinct[, 2])
ival

OUT2 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- rbind(dat2, 0)
  lines(1:length(dat2[, 1]), dat2$n.adults, col="orange3", lty=1, xlim = c(0, 150))
  dat2 <- dat2$n.adults
  if(length(dat2 < 150)){n.zeros <- 150-length(dat2); dat2 <- c(dat2, rep(0, n.zeros))}
  OUT2 <- cbind(OUT2, dat2)
  }

temp <- rowMeans(OUT2)
temp <- apply(OUT2, 1, median)
lines(1:62, temp[1:62], col="orange", lwd = 4)


# coexist =======================================================================================#
coexist <- OUT[OUT[, 1] == "coexist", ]
(table(coexist[, 2]))
n.coexist <- length(table(coexist[, 2]))
ival <- unique(coexist[, 2])
ival


OUT2 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- dat2$n.adults
  OUT2 <- cbind(OUT2, dat2)
}


temp <- rowMeans(OUT2)
#temp <- apply(OUT2, 1, median)
lines(1:150, temp, col="black", lwd = 4)


# clear =======================================================================================#
clear <- OUT[OUT[, 1] == "clear", ]
(table(clear[, 2]))
n.clear <- length(table(clear[, 2]))
ival <- unique(clear[, 2])
ival

OUT2 <- NULL
#for(n in 1:150){
#OUT2 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- dat2$n.adults
  OUT2 <- cbind(OUT2, dat2)
}


temp <- rowMeans(OUT2)
#temp <- apply(OUT2, 1, median)
#lines(1:150, temp, col="blue", lwd = 4)








# not used
# histogram=====================================================================================#
barplot(c(n.extinct, n.coexist, n.clear), col=c("red", "purple", "blue"))
barplot(c(n.extinct/50, n.coexist/50, n.clear/50), col=c("red", "purple", "blue"), ylab = "Frequency", names.arg = c("Extinct", "Coexist", "Clear"))
abline(h=0)
mtext()

nlines =5
(n.extinct/50) *nlines
(n.coexist/50) *nlines
(n.clear/50) *nlines

plot(c(1,2,3), c(n.extinct, n.coexist, n.clear), ylim = c(0, 50), xlim = c(0.5, 3.5))

