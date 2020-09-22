#=============================================================================================================#
# Script created by Mark Christie, all rights reserved, contact at markchristie1500@gmail.com
# Script created in version R 4.0.0 on xx/xx/2020
# This script:
# Usage notes: 
#============================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
#library("")

#source("~/Drive/scripts.R", sep = ''))


# Panel A================================================#

n.blue <- 6/50
n.purple <- 44/50

par(bg = 'grey')
barplot(c(n.blue, n.purple), col = c("blue", "purple"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1))
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray", border = NA)
#barplot(c(n.blue, n.purple), col = c("blue", "purple"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1), add = TRUE)
abline(h = 0)

# Panel B================================================#

n.orange <- 11/50
n.blue   <- 9/50
n.purple <- 30/50

barplot(c(n.orange, n.blue, n.purple), col = c("orange", "blue", "purple"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1))
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray", border = NA)
#barplot(c(n.orange, n.blue, n.purple), col = c("orange", "blue", "purple"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1), add = TRUE)
abline(h = 0)


# Panel D================================================#


n.orange <- 12/50
n.black   <- 38/50

barplot(c(n.orange, n.black), col = c("orange", "black"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1))
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray", border = NA)
#barplot(c(n.orange, n.black), col = c("orange", "black"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1), add = TRUE)
abline(h = 0)





# Panel E ================================================#

n.orange <- 30/50
n.black   <- 20/50

barplot(c(n.orange, n.black), col = c("orange", "black"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1))
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray", border = NA)
#barplot(c(n.orange, n.black), col = c("orange", "black"), ylim = c(0, 1), width = c(0.1, 0.1), space = 0.2, xlim = c(0, 1), add = TRUE)
abline(h = 0)









