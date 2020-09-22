WinterAdults <- function(adults, Nt1, adult.survival.var) {
	n.adults <- nrow(adults)  
	adult.mortality <- rnorm(1, (n.adults - Nt1), adult.survival.var)
	if(adult.mortality < 0) {adult.mortality <- 0}
	n.adults <- n.adults - adult.mortality
    if(n.adults <=0) {n.adults <- 0}

  	if(n.adults > length(adults[, 1])) {n.adults <- length(adults[, 1])}   # can't take larger sample 
  	survivors <- sample(1:length(adults[, 1]), n.adults, replace = FALSE)
  	adults    <- adults[survivors, ]
  	return(adults)
}



