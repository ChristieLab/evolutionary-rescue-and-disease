RunModel <- function(n.generations, i) {
  i <<- i

  # hosts columns are as follows: individual, species, genotype, stage(tadpole=1;adult=2), infected(no=1,yes=2), xposition, yposition

  hosts <- PondSetup(plength, pwidth, n.species, n.genotypes, n.adults, n.tads)
  susceptibilities <- Susceptibilities(hosts, host.suscepts) # creates susceptibilities
  susceptibilities <- Genotypes(susceptibilities, n.loci, n.alleles) # creates geneotypes
  hosts <- cbind(hosts, susceptibilities[, -c(1, 3)])
  one.individual <- ncol(hosts)

  #PlotIt(hosts, pwdith, plength) # plots hosts

  for(n in 1:n.generations){
    
    adults   <- hosts[hosts[, 4] == 2, ]
    n.adults <- nrow(adults)
    Nt1      <- n.adults + (r*n.adults*(1-(n.adults/k.adults)))

    if(length(hosts) <= (one.individual * 3)) {break}   # break if only one adult (or less left)
    hosts <- Reproduction(i, n.loci, hosts, n.eggs)
    hosts <- HostDispersal(hosts, plength, pwidth) # must occur, becuase new tads given 0,0 coordinates
  
    if(length(hosts[hosts[, 4] == 1, ]) > one.individual) {hosts <- WinterTadpoles(hosts, k.tads, k.adults, tadpole.survival.var)}
    if(length(hosts) <= one.individual) {break}   # break if only one adult (or less left)
    #pop.size <- data.frame(table(hosts[, 4]))
    
    if(n > 49 & mean(hosts[, 8]) <= 1) {
      if(n == 50) {hosts[1, 5] <- 2}
      hosts.temp <- hosts
      hosts <- hosts[, 5:7]
      hosts <- Infection3(hosts, days, threshold, plength, pwidth) # No Infecteds Die
      hosts.temp[, 5:7] <- hosts
      hosts <- hosts.temp
    }
    #PlotIt(hosts, pwdith, plength) # plots hosts  

    if(length(hosts[hosts[, 4] == 1, ]) > one.individual) {
      emmi   <- AdultEmmigration(hosts)
      hosts  <- emmi[[1]]
      adults <- emmi[[2]]
    }
 
    if(length(hosts) <= one.individual) {break}
    if(length(sample(1:length(hosts[, 1]), proportion.metamorph*length(hosts[, 1]), replace = FALSE)) > 1) {
      meta   <- Metamorphosis(hosts, adults, proportion.metamorph)
      hosts  <- meta[[1]]
      adults <- meta[[2]]
    }

    #if(length(hosts) <= 7) {break}
    if(length(adults) <= one.individual) {break}   # break if only one individual (or less left)
    if(length(adults) > one.individual) {adults <- WinterAdults(adults, Nt1, adult.survival.var)}

    # step 10: Infected adults experience chytrid mortality only after 49 generations
    if(n > 49) {
      if(length(adults) > one.individual){
        if(length(adults[adults[, 5] == 2, ]) > one.individual) {adults <- ChytridAdults(adults, host.suscepts)}
      }
    }
   
    hosts  <- rbind(hosts, adults)
    counts <- table(hosts[, 4])
    if(length(counts) < 1) {break} 
    if(counts[1] < 2) {break}   # break if only one tadpole 
    #if(counts[2] < 2) {break}   # break if only one adult 
    output <- Output(hosts, n, i)
  }
}
