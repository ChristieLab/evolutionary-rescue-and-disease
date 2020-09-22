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

dat <- read.table("Output_stochastic.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("i", "n.gen", "plength", "pwidth", "n.species.set", "n.loci.set", "n.allele.set", "n.genotypes.set", "n.adults.set", "n.tads.set", "k.adults", "k.tads", "n.eggs", "adult.survival.var", "tadpole.survival.var", "proportion.metamorph", "r", "days", "threshold", "host.scepts.min", "host.scepts.max", "n.generations", "n.replicates", "n.gtypes", "heterozygosity.expected", "heterozygosity.observed","p.polymorphic", "suscepts", "n.individs", "n.species", "n.tads", "n.tads.inf", "n.tads.uninf", "n.adults", "n.adults.inf", "n.adults.uninf", "n.inf", "n.uninf")
head(dat)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

# full =======================================#
#par(mfrow=c(1,3))

length(dat[, 1])
#plot(-10, -10, xlim = c(0, 150), ylim = c(0, max(dat$n.adults) +10), xlab = "Years", ylab = "Adult population size")
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")


OUT <- NULL
n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  #lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) { out <- cbind("extinct", dat2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) {out <- cbind("coexist", dat2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) { out <- cbind("clear", dat2)}
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

OUT3 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat2 <- rbind(dat2, 0)
  dat3 <- dat2$n.adults
  n.years  <- length(dat3)
  n.adults <- dat3[n.years]
  
  #change allele freqs
  year1 <- dat2[1, 28]
  year1 <- unlist(strsplit(year1, "/"))
  year1 <- unlist(strsplit(year1, " "))
  year1 <- mean(as.numeric(year1[seq(from = 1, to = length(year1), by = 2)]))
  
  
  year2 <- dat2[n.years-1, 28]
  year2 <- unlist(strsplit(year2, "/"))
  year2 <- unlist(strsplit(year2, " "))
  year2 <- mean(as.numeric(year2[seq(from = 1, to = length(year2), by = 2)]))
  
  category <- 0 # 0 = extinct
  
  output <- cbind(category, n.years, n.adults, year1, year2, abs(year2-year1))
  
  OUT3 <- rbind(OUT3, output)
}



# coexist =======================================================================================#
coexist <- OUT[OUT[, 1] == "coexist", ]
(table(coexist[, 2]))
n.coexist <- length(table(coexist[, 2]))
ival <- unique(coexist[, 2])
ival

#OUT3 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat3 <- dat2$n.adults
  n.years  <- length(dat3)
  n.adults <- dat3[n.years]
  
  #change allele freqs
  year1 <- dat2[1, 28]
  year1 <- unlist(strsplit(year1, "/"))
  year1 <- unlist(strsplit(year1, " "))
  year1 <- mean(as.numeric(year1[seq(from = 1, to = length(year1), by = 2)]))
  
  
  year2 <- dat2[n.years-1, 28]
  year2 <- unlist(strsplit(year2, "/"))
  year2 <- unlist(strsplit(year2, " "))
  year2 <- mean(as.numeric(year2[seq(from = 1, to = length(year2), by = 2)]))
  
  category <- 1 # 1 = coexist
  
  output <- cbind(category, n.years, n.adults, year1, year2, abs(year2-year1))
  
  OUT3 <- rbind(OUT3, output)
}





# clear =======================================================================================#
clear <- OUT[OUT[, 1] == "clear", ]
(table(clear[, 2]))
n.clear <- length(table(clear[, 2]))
ival <- unique(clear[, 2])
ival

#OUT3 <- NULL
for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat3 <- dat2$n.adults
  n.years  <- length(dat3)
  n.adults <- dat3[n.years]
  
  #change allele freqs
  year1 <- dat2[1, 28]
  year1 <- unlist(strsplit(year1, "/"))
  year1 <- unlist(strsplit(year1, " "))
  year1 <- mean(as.numeric(year1[seq(from = 1, to = length(year1), by = 2)]))
  
  
  year2 <- dat2[n.years-1, 28]
  year2 <- unlist(strsplit(year2, "/"))
  year2 <- unlist(strsplit(year2, " "))
  year2 <- mean(as.numeric(year2[seq(from = 1, to = length(year2), by = 2)]))
  
  category <- 2 # 1 = clear
  
  output <- cbind(category, n.years, n.adults, year1, year2, abs(year2-year1))
  
  OUT3 <- rbind(OUT3, output)
}





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

setwd("C:/Users/Mark Christie/Dropbox/manuscripts/In_Review/evolutionary_rescue_review/model/chytrid2.8/output")
list.files()

dat <- read.table("Output_deterministic.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("i", "n.gen", "plength", "pwidth", "n.species.set", "n.loci.set", "n.allele.set", "n.genotypes.set", "n.adults.set", "n.tads.set", "k.adults", "k.tads", "n.eggs", "adult.survival.var", "tadpole.survival.var", "proportion.metamorph", "r", "days", "threshold", "host.scepts.min", "host.scepts.max", "n.generations", "n.replicates", "n.gtypes", "heterozygosity.expected", "heterozygosity.observed","p.polymorphic", "suscepts", "n.individs", "n.species", "n.tads", "n.tads.inf", "n.tads.uninf", "n.adults", "n.adults.inf", "n.adults.uninf", "n.inf", "n.uninf")
head(dat)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

# full =======================================#

length(dat[, 1])
#plot(-10, -10, xlim = c(0, 150), ylim = c(0, max(dat$n.adults) +10), xlab = "Years", ylab = "Adult population size")
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")


OUT <- NULL
n.sims <- unique(dat[, 1])
for(n in n.sims){
  dat2 <- dat[dat[, 1] == n, ]
  #lines(1:length(dat2[, 1]), dat2$n.adults, col="blue", lwd=2)
  lyr <- dat2[length(dat2[, 1]), ]
  if(length(dat2[, 1]) < 150) {out <- cbind("extinct", dat2)}
  if(length(dat2[, 1]) == 150 & lyr$n.inf > 0) { out <- cbind("coexist", dat2)}  
  if(length(dat2[, 1]) == 150 & lyr$n.inf == 0) { out <- cbind("clear", dat2)}
  colnames(out)[1] <- "result"
  OUT <- rbind(OUT, out)
}

# extinct =======================================================================================#
table(OUT[, 1])
extinct <- OUT[OUT[, 1] == "extinct", ]
(table(extinct[, 2]))
n.extinct <- length(table(extinct[, 2]))  # none went exting
#ival <- unique(extinct[, 2])
#ival

#OUT2 <- NULL
#for(i in ival){
#dat2 <- dat[dat[, 1] == i, ]
#dat2 <- rbind(dat2, 0)
#lines(1:length(dat2[, 1]), dat2$n.adults, col="orange3", lty=1, xlim = c(0, 150))
##dat2 <- dat2$n.adults
#if(length(dat2 < 150)){n.zeros <- 150-length(dat2); dat2 <- c(dat2, rep(0, n.zeros))}
#OUT2 <- cbind(OUT2, dat2)
#}

#temp <- rowMeans(OUT2)
#temp <- apply(OUT2, 1, median)
#lines(1:68, temp[1:68], col="orange", lwd = 4)


# coexist =======================================================================================#
coexist <- OUT[OUT[, 1] == "coexist", ]
(table(coexist[, 2]))
n.coexist <- length(table(coexist[, 2]))
ival <- unique(coexist[, 2])
ival

for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat3 <- dat2$n.adults
  n.years  <- length(dat3)
  n.adults <- dat3[n.years]
  
  #change allele freqs
  year1 <- dat2[1, 28]
  year1 <- unlist(strsplit(year1, "/"))
  year1 <- unlist(strsplit(year1, " "))
  year1 <- mean(as.numeric(year1[seq(from = 1, to = length(year1), by = 2)]))
  
  
  year2 <- dat2[n.years-1, 28]
  year2 <- unlist(strsplit(year2, "/"))
  year2 <- unlist(strsplit(year2, " "))
  year2 <- mean(as.numeric(year2[seq(from = 1, to = length(year2), by = 2)]))
  
  category <- 4 # 4 coexist deterministic
  
  output <- cbind(category, n.years, n.adults, year1, year2, abs(year2-year1))
  
  OUT3 <- rbind(OUT3, output)
}




# clear =======================================================================================#
clear <- OUT[OUT[, 1] == "clear", ]
(table(clear[, 2]))
n.clear <- length(table(clear[, 2]))
ival <- unique(clear[, 2])
ival

for(i in ival){
  dat2 <- dat[dat[, 1] == i, ]
  dat3 <- dat2$n.adults
  n.years  <- length(dat3)
  n.adults <- dat3[n.years]
  
  #change allele freqs
  year1 <- dat2[1, 28]
  year1 <- unlist(strsplit(year1, "/"))
  year1 <- unlist(strsplit(year1, " "))
  year1 <- mean(as.numeric(year1[seq(from = 1, to = length(year1), by = 2)]))
  
  
  year2 <- dat2[n.years-1, 28]
  year2 <- unlist(strsplit(year2, "/"))
  year2 <- unlist(strsplit(year2, " "))
  year2 <- mean(as.numeric(year2[seq(from = 1, to = length(year2), by = 2)]))
  
  category <- 5 # 1 = clear deterministic
  
  output <- cbind(category, n.years, n.adults, year1, year2, abs(year2-year1))
  
  OUT3 <- rbind(OUT3, output)
}


# create actual plot ==============================================================================#
OUT3

plot(1:length(OUT3[, 1]), OUT3[, 6], )
coexist <- OUT3[which(OUT3[, 1] == 1), ]
extinct <- OUT3[which(OUT3[, 1] == 0), ]
clear <- OUT3[which(OUT3[, 1] == 2), ]

coexist2  <- OUT3[which(OUT3[, 1] == 4), ]
clear2    <- OUT3[which(OUT3[, 1] == 5), ]

plot(-100, -100, xlim = c(0.5, 5.5), ylim = c(0, 0.55), xlab = "Population outcome", ylab = "Allele frequency change" )
points(coexist[, 1] + 1, coexist[, 6], pch = 21, bg = "purple", cex = 2)
points(extinct[, 1] + 1, extinct[, 6], pch = 21, bg = "orange", cex = 2)
points(clear[, 1] + 1, clear[, 6], pch = 21, bg = "blue", cex = 2)

points(coexist2[, 1], coexist2[, 6], pch = 21, bg = "purple", cex = 2)
points(clear2[, 1], clear2[, 6], pch = 21, bg = "blue", cex = 2)


dat5 <- OUT3[order(OUT3[, 6]), ]
dat5 <- dat5[-which(dat5[, 1] == 4), ]
dat5 <- dat5[-which(dat5[, 1] == 5), ]
dat5 <- dat5[order(dat5[, 6]), ]


dat5 <- cbind(1:length(dat5[, 1]), dat5)
plot(1:length(dat5[, 1]), dat5[, 7], xlim = c(0,  75), ylim = c(0, 0.6), ylab = "Allele frequency change", xlab = "Simulation")
points(dat5[which(dat5[, 2] == 1), 1], dat5[which(dat5[, 2] == 1), 7], pch = 19, col = "purple", cex = 2)
points(dat5[which(dat5[, 2] == 0), 1], dat5[which(dat5[, 2] == 0), 7], pch = 19, col = "orange", cex = 2)
points(dat5[which(dat5[, 2] == 2), 1], dat5[which(dat5[, 2] == 2), 7], pch = 19, col = "blue", cex = 2)


dat5 <- OUT3[order(OUT3[, 6]), ]
dat5 <- dat5[-which(dat5[, 1] == 0), ]
dat5 <- dat5[-which(dat5[, 1] == 1), ]
dat5 <- dat5[-which(dat5[, 1] == 2), ]
dat5 <- dat5[order(dat5[, 6]), ]

dat5 <- cbind(1:length(dat5[, 1]), dat5)
points(dat5[which(dat5[, 2] == 5), 1] + 15, dat5[which(dat5[, 2] == 5), 7], pch = 19, col = "blue", cex = 2)
points(dat5[which(dat5[, 2] == 4), 1] + 15, dat5[which(dat5[, 2] == 4), 7], pch = 19, col = "purple", cex = 2)


#Hump shaped graph =======================================================================

dat5 <- OUT3[order(OUT3[, 6]), ]
dat5 <- dat5[-which(dat5[, 1] == 4), ]
dat5 <- dat5[-which(dat5[, 1] == 5), ]
dat5 <- dat5[order(dat5[, 6]), ]

plot(dat5[, 3], dat5[, 6], xlim = c(0, 250))

plot(dat5[, 6], dat5[, 3], xlim = c(0, 0.55), xlab = "Allele frequency change", ylab = "Final population size")
points(dat5[which(dat5[, 1] == 1), 6], dat5[which(dat5[, 1] == 1), 3], pch = 19, col = "purple", cex = 2)
points(dat5[which(dat5[, 1] == 0), 6], dat5[which(dat5[, 1] == 0), 3], pch = 19, col = "orange", cex = 2)
points(dat5[which(dat5[, 1] == 2), 6], dat5[which(dat5[, 1] == 2), 3], pch = 19, col = "blue", cex = 2)


dat5 <- OUT3[order(OUT3[, 6]), ]
dat5 <- dat5[-which(dat5[, 1] == 0), ]
dat5 <- dat5[-which(dat5[, 1] == 1), ]
dat5 <- dat5[-which(dat5[, 1] == 2), ]
dat5 <- dat5[order(dat5[, 6]), ]
points(dat5[which(dat5[, 1] == 4), 6], dat5[which(dat5[, 1] == 4), 3], pch = 21, bg = "purple", col="orange", cex = 2)
points(dat5[which(dat5[, 1] == 5), 6], dat5[which(dat5[, 1] == 5), 3], pch = 19, col = "pink", cex = 2)



#length(extinct[, 1])
#length(coexist[, 1])
#length(clear[, 1])


text(1, 0.54, "N=11", cex = 2)
text(2, 0.54, "N=30", cex = 2)
text(3, 0.54, "N=9", cex = 2)

abline(h=0.52)

