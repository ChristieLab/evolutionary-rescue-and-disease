#==================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.1.1
# This script:  Generates model output for Daphnia plasticity project
# Usage notes: Set all parameters below and then source this file
#==================================================================================================#
# Set working directory and output directory
# Directory where model.R and 'source' folder reside  
setwd("C:/Users/Mark Christie/Dropbox/manuscripts/In_Review/evolutionary_rescue_review/model/chytrid2.8_abiotic")
base.directory <- getwd()
outdir <- paste(base.directory,"/output/",sep="")  # directory to save model output  
source(paste(base.directory, "/source/FunctionSourcer.R", sep = '')) #loads packages, sources functions, and sets source directory

#Pond parameters
plength <- 100   # length of pond
pwidth  <- 100   # width of pond

#Community parameters
n.species <- 1 # number of species
#relative.abundances <- 

#individual species parameters
k.tads   <- 1000
k.adults <- 200
n.tads   <- k.tads  # total number of tadpoles (for all genotypes), within a species
n.adults <- k.adults  # total number of adults (for all genotypes), within a species
#adult.survival.var   <- c(7)   # for stochastic 
adult.survival.var   <- c(0)  # for deterministic
tadpole.survival.var <- c(0)
proportion.metamorph <- c(1)  # proportion of tadpoles that metamorph each generation
r <- c(0.25)
n.eggs <- 100 # must be an even number; see Reproduction

#Dispersal parameters (for parasite)
days <- c(2000)
#threshold <- c(0.2, 0.3, 0.4)  # values for original paper
threshold <- c(0.3) # value for abiotic disease (first panels)
#threshold <- c(0.99) # value for abiotic disease (first panels) - actually need to rewrite model to expose every indivdiual (not just infected) to selection

#Host and parasite parameters
#host.suscepts <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)    # range of suscepts (uniform dist, see Susceptibilities function); expressed as percent survival
#range.suscepts <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.01)     # +- range.suscepts
host.suscepts <- c(0.5)    # range of suscepts (uniform dist, see Susceptibilities function); expressed as percent survival
range.suscepts <- c(0.47)     # +- range.suscepts


#Model paramters
n.generations <- 150   # number of propagules of host to release
n.replicates  <- 50  # number of replicates for each combination of parametmers
n.loci <- 5# 50
n.alleles <- 2

#==========================================================================================================#
replicates <- Replicates(proportion.metamorph, adult.survival.var, tadpole.survival.var, r, k.tads, k.adults, host.suscepts, range.suscepts, days, threshold, n.replicates)
#i <- 50

for(i in 1:length(replicates[, 1])){
  n.adults <<- as.numeric(as.character(replicates[i, 1]))
  n.tads <<- as.numeric(as.character(replicates[i, 2]))
  k.adults <<- as.numeric(as.character(replicates[i, 1]))
  k.tads <<- as.numeric(as.character(replicates[i, 2]))
  host.suscepts <<- c(as.numeric(as.character(replicates[i, 3])), as.numeric(as.character(replicates[i, 4])))
  adult.survival.var <<- as.numeric(as.character(replicates[i, 5]))
  tadpole.survival.var <<- as.numeric(as.character(replicates[i, 6]))
  proportion.metamorph <<- as.numeric(as.character(replicates[i, 7]))
  r <<- as.numeric(as.character(replicates[i, 8]))
  days <<- as.numeric(as.character(replicates[i, 9]))
  threshold <<- as.numeric(as.character(replicates[i, 10]))
  n.genotypes <<- k.adults + k.tads
  

  model<- RunModel(n.generations, i)
}


