Metamorphosis <- function(hosts, adults, proportion.metamorph) {
  metas      <- sample(1:length(hosts[, 1]), proportion.metamorph*length(hosts[, 1]), replace = FALSE)  #sampe by proportion.metamorph
  new.adults <- hosts[metas, ]
  hosts      <- hosts[-(metas), ]
  #new.adults[, 5] <- 1  # all new metamorphs clear their infection!
  new.adults[, 4] <- 2  # all new metamorphs are now adults
  adults     <- rbind(adults, new.adults)
  new        <- list(hosts, adults)
  return(new)
}



