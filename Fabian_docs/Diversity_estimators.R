## Estimation of diversity measures using iNEXT for trees in LLNP

library(iNEXT)  # Load packages
library(ggplot2)

setwd("C:/Users/Fabian/ownCloud/DISS Ergebnisse/diversity-paper/")  # Load abundance data
abund <- read.csv2("abundances_r_spperplot.csv", row.names = 1)

# Compute and show base sample size
  # Chao et al. (2014) suggest to use a sample size of twice the smallest reference sample size or the largest sample size, whichever larger.
bss <-      max(c(min(colSums(abund)*2)), max(colSums(abund)))
bss.cons <- c(min(colSums(abund)*2))  # or use the more conservative approach of twice the smallest reference sample size 
bss
bss.cons


#### Compute diversity measures (dm), can take a while depending on the number of runs specified in nboot:
  # observed (based on number of individuals present in each plot) and estimated (through rarefaction/extrapolation with bss as endpoint)
  # for 0D (species richness), 1D (effective number of species), and 2D (Simpson index)
dm <- iNEXT(abund.data, q = c(0, 1, 2), datatype =  "abundance", endpoint = bss, nboot = 500)

# Display data
dm$DataInfo  # display basic data info
dm$AsyEst  # show observed and estimated diversity values  -> These are the indices we want
# dm$iNextEst$S1450  # display estimated values for each knot for any plot

# Plot graphs (quite ugly, graphical parameters have to be adjusted in ggplot2)
  # see ggiNEXT for different options in plotting specified by type
windows(80/2.54, 20/2.54)
ggiNEXT(dm, type = 1, facet.var = "site")  # plot diversity indices against sample-size


windows(80/2.54, 20/2.54)
ggiNEXT(dm, type = 3, facet.var = "site")  # plot diversity indices against coverage

