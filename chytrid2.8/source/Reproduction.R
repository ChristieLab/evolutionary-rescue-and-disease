Reproduction <- function(i, n.loci, hosts, n.eggs) {
  adults <- hosts[hosts[, 4] == 2, ]
  n.adults <- nrow(adults)
  tads <- hosts[hosts[, 4] == 1, ]
  tads.0 <- tads   # tads that are currently alive;
  
  # randomly pair individuals (no a priori sex)
  ndivids <- length(adults[, 1])
  if(ndivids %% 2 !=0) {ndivids <- ndivids-1}  # round down by 1 if even number of individuals
  individuals <- 1:ndivids
  
  males   <- sample(individuals, length(individuals)/2, replace = FALSE) # if odd number, should just reduce by 1 individual
  females <- individuals[-males]
  females <- sample(females, length(females), replace = FALSE)
  pairs   <- cbind(adults[males, ], adults[females, ])
  
  # COULD ADD COST OF INFECTION/RESISTANCE RIGHT HERE (IF INFECTED, THEN LOWER REPRO OUTPUT)
  # could speed up by first finding identical pairs?
  OUT <- NULL
  for (p in 1:length(pairs[, 1])){
    pair <- pairs[p, ]
    n.col <- (length(pair)/2)+1
    gtypes <- rbind(pair[1:(n.col-1)], pair[n.col:length(pair)])
    gtypes2 <- gtypes[, -(1:8)]
        
    n.offspring <- n.eggs # *   add in factor right here for COST OF RESISTNACE?INFECTION

    OFFS <- NULL
    for (n in 1:n.offspring){
      parent1 <- sample(0:1, n.loci, replace = TRUE)
      parent2 <- sample(0:1, n.loci, replace = TRUE)
      positions <- seq(from = 1, to = n.loci*2, by = 2)
      p1 <- positions + parent1
      p2 <- positions + parent2
      o1 <- gtypes2[1, p1]
      o2 <- gtypes2[2, p2]
      off1 <- cbind(positions, o1)
      off2 <- cbind(positions+ 1, o2)
      off <- rbind(off1, off2)
      off <- off[order(off[, 1]), ]
      off <- off[, 2]
      OFFS <- rbind(OFFS, c(n, off))
    }
  
  pair.data <- gtypes[, 1:8]
  pair.scept <- sort(pair.data[, 8])
  #pscept1 <- rnorm(length(OFFS[, 1])/2, pair.scept[1], pair.scept[2]-pair.scept[1])
  pscept1 <- rnorm(length(OFFS[, 1])/2, pair.scept[1], 0.001) # That standard Dev is mui important!
  pscept2 <- rnorm(length(OFFS[, 1])/2, pair.scept[2], 0.001)
  pair.scept <- c(pscept1, pscept2)
  pair.scept[which(pair.scept < 0)] <- 0
  pair.scept[which(pair.scept > 1)] <- 1
  #pair.scept <- sample(seq(from = pair.scept[1], to = pair.scept[2], by = 0.00001), length(OFFS[, 1]), replace = TRUE)

  #pair.dat2 <- pair.data[sample(1:2, 1), ] # randomly sample one of the pairs for susceptibility info!

  out2 <- cbind(p, pair.scept, OFFS)
  OUT <- rbind(OUT, out2)
  }
  
  scept  <- OUT[, 2]
  spp    <- rep(1, length(OUT[, 1]))
  ids    <- as.numeric(paste(i, ".", 1:length(OUT[, 1]), sep = "")) # create new individual ids (with generation) 
  tads2  <- cbind(ids, spp, 0, 1, 1, 0, 0, scept)          # no new infections; id, species, genotype (set to 0), stage, infected, x, y, scept
  tads3  <- cbind(tads2, OUT[, -c(1:3)])
  
  hosts  <- rbind(adults, tads.0, tads3)      # create one big hosts data frame with adults that reproduced, surviving tadpoles, new tadpoles
  return(hosts)
}


