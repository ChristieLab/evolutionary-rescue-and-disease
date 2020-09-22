#FunctionSourcer <- function() {
# Set working directory, import packages, source functions, 
setwd(paste(base.directory,"/source/", sep = ''))    # set temp working directory 
#library(fields)  # used for calculation of pathogen dispersal distance matrix

source(paste(getwd(), "/PondSetup.R", sep = ''))
source(paste(getwd(), "/Susceptibilities.R", sep = ''))
source(paste(getwd(), "/PlotIt.R", sep = ''))
source(paste(getwd(), "/Genotypes.R", sep = ''))
#source(paste(getwd(), "/Dispersal.R", sep = ''))
source(paste(getwd(), "/Infection3.R", sep = ''))
source(paste(getwd(), "/HostDispersal.R", sep = ''))
source(paste(getwd(), "/AdultEmmigration.R", sep = ''))
source(paste(getwd(), "/Metamorphosis.R", sep = ''))
source(paste(getwd(), "/WinterTadpoles.R", sep = ''))
source(paste(getwd(), "/WinterAdults.R", sep = ''))
source(paste(getwd(), "/ChytridAdults.R", sep = ''))
source(paste(getwd(), "/Reproduction.R", sep = ''))
source(paste(getwd(), "/Output.R", sep = ''))
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/Replicates.R", sep = ''))
#source(paste(getwd(), "/PopulationRegulationWithVariation.R", sep = ''))
