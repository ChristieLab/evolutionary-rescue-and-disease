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


# extinct =======================================#

length(dat[, 1])
plot(-10, -10, xlim = c(0, 150), ylim = c(0, max(dat$n.adults) +10), xlab = "Years", ylab = "Adult population size")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")


OUT <- NULL
n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="red", lwd =2); out <- cbind("extinct", dat2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {lines(1:length(dat2[, 1]), dat2$n.adults, col="purple", lwd =2); out <- cbind("coexist", dat2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="blue", lwd =2); out <- cbind("clear", dat2)}
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

for(i in ival){
dat2 <- dat[dat[, 1] == i, ]
plot(-100, -100, col="blue", lty=1, xlim = c(0, 150), ylim = c(0, 250))
lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lty=1, xlim = c(0, 150))
}
# n=
evals <- c(42, 47)

# coexist =======================================================================================#
coexist <- OUT[OUT[, 1] == "coexist", ]
(table(coexist[, 2]))
n.coexist <- length(table(coexist[, 2]))
ival <- unique(coexist[, 2])
ival

for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  plot(-100, -100, col="blue", lty=1, xlim = c(0, 150), ylim = c(0, 250), main = i)
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lty=1, xlim = c(0, 150))
}
# n=
covals <- c(46, 44, 34, 28, 19, 10)

# clear =======================================================================================#
clear <- OUT[OUT[, 1] == "clear", ]
(table(clear[, 2]))
n.clear <- length(table(clear[, 2]))
ival <- unique(clear[, 2])
ival

for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  plot(-100, -100, col="blue", lty=1, xlim = c(0, 150), ylim = c(0, 250), main = i)
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lty=1, xlim = c(0, 150))
}

clvals <- c(50, 31)


#panel A =========================================================================================#

all.ivals <- c(evals[2], covals[1:3], clvals[1])

plot(-10, -10, xlim = c(0, 150), ylim = c(0, max(dat$n.adults) +10), xlab = "Years", ylab = "Adult population size")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

n.sims <- all.ivals
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="red", lwd =2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {lines(1:length(dat2[, 1]), dat2$n.adults, col="purple", lwd =2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) {lines(1:length(dat2[, 1]), dat2$n.adults, pch=19, col="blue", lwd =2)}
}




# histogram=====================================================================================#
barplot(c(n.extinct, n.coexist, n.clear), col=c("red", "purple", "blue"))
barplot(c(n.extinct/50, n.coexist/50, n.clear/50), col=c("red", "purple", "blue"), ylab = "Frequency", names.arg = c("Extinct", "Coexist", "Clear"))
abline(h=0)
mtext()

nlines =5
(n.extinct/50) *nlines
(n.coexist/50) *nlines
(n.clear/50) *nlines
  