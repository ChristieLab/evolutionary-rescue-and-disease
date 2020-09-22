WinterTadpoles <- function(hosts, k.tads, k.adults, tadpole.survival.var) {
  adults <- hosts[hosts[, 4] == 2, ]
  n.adults <- nrow(adults)
  tads <- hosts[hosts[, 4] == 1, ]
  n.tads <- nrow(tads)

  tadpole.mortality <- rnorm(1, (n.tads - k.tads), tadpole.survival.var)
  if(tadpole.mortality < 0) {tadpole.mortality <- 0}
  n.tads <- (n.tads - tadpole.mortality) * (n.adults/k.adults)
  if(n.tads <= 0) {n.tads <- 0}

  if(n.tads > length(tads[, 1])) {n.tads = length(tads[, 1])}   # can't take larger sample  
  survivors <- sample(1:length(tads[, 1]), n.tads, replace = FALSE) 
  tads <- tads[survivors, ]
  hosts <- rbind(tads, adults)
  return(hosts)
}



