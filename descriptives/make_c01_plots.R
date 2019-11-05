##############################################################
# iNEXT for C01 data
##############################################################
library(iNEXT)
library(ggplot2)

setwd("C:/Users/jakob/MA/Neuer Ordner/JambiBiodiveRsity/JambiBiodiveRsity/descriptives")

abund_c01 <- read.csv2("matrix_wide2018.csv", sep=",")
abund_c01$X <- NULL 

abund_c01 <- t(as.matrix(abund_c01))

abund_c01 <-  as.data.frame(abund_c01)
names(abund_c01) <- abund_c01[1,]

abund_c01 <- abund_c01[-1,]

bss <-      max(c(min(colSums(abund_c01)*2)), max(colSums(abund_c01)))
bss.cons <- c(min(colSums(abund_c01)*2))  # or use the more conservative approach of twice the smallest reference sample size 
bss
bss.cons



for( i in 1:211){
paste(dm,) <- iNEXT(abund_c01[,i], q = c(0, 1, 2), datatype =  "abundance", endpoint = bss, nboot = 500)

}


dm <- iNEXT(abund_c01[,2], q = c(0, 1, 2), datatype =  "abundance", endpoint = bss, nboot = 50)

dm$DataInfo  # display basic data info
dm$AsyEst

windows(80/2.54, 20/2.54)
ggiNEXT(dm, type = 1, facet.var = "site")  # plot diversity indices against sample-size


windows(40/2.54, 40/2.54)
ggiNEXT(dm, type = 3, facet.var = "site") + facet_wrap(~site, nrows=40)  # plot diversity indices against coverage





