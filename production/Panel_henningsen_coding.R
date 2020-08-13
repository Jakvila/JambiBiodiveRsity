# Reproducing the Henningsen analysis strategy for DEA in a panel structure
library(Benchmarking)

xMat <- cbind ( PalmDataRestricted$Size, PalmDataRestricted$Labour, PalmDataRestricted$Inputs)

yVec <- matrix(c(PalmDataRestricted$Production))


# calculate and decompose productivity changes:

xMat0 <- xMat[PalmDataRestricted$wave == 2015, ]
xMat1 <- xMat[PalmDataRestricted$wave == 2018, ]

yVec0 <- yVec[PalmDataRestricted$wave == 2015, ]
yVec1 <- yVec[PalmDataRestricted$wave == 2018, ]


c00 <- eff( dea( xMat0, yVec0, RTS = "crs" ) )
c01 <- eff( dea( xMat0, yVec0, XREF = xMat1, YREF = yVec1, RTS = "crs" ) )
c10 <- eff( dea( xMat1, yVec1, RTS = "crs" ) )
c11 <- eff( dea( xMat1, yVec1, XREF = xMat0, YREF = yVec0, RTS = "crs" ) )

dProd0 <- c11 / c00

hist ( dProd0 )

dProd1 <- c10 / c01

plot (dProd0, dProd1)

dProd <- sqrt( dProd0 * dProd1 )

hist( dProd )

summary(c00)

