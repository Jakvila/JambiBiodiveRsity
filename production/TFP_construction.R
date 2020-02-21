
# This script seeks to calculate the Total Factor Productivity of small-scale palm oil production in Jambi, Indonesia




PalmDataRestricted$XP <- with(PalmDataRestricted,
                  ( vCap + vLab + vMat ) /
                  ( mean( qCap ) * pCap + mean( qLab ) * pLab + mean( qMat ) * pMat ) )

PalmDataRestricted$XL <- with(PalmDataRestricted,
                  ( qCap * mean( pCap ) + qLab * mean( pLab ) + qMat * mean( pMat ) ) /
                  ( mean( qCap ) * mean( pCap ) + mean( qLab ) * mean( pLab ) +
                        mean( qMat ) * mean( pMat ) ) )


TFP12 <- PalmDataRestricted$Production[PalmDataRestricted$wave == 2012] /
          PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2012]

TFP15 <- PalmData$Production[PalmData$wave == 2015] /
          PalmData$Inputs[PalmData$wave == 2015]

TFP18 <- PalmData$Production[PalmData$wave == 2018] /
          PalmData$Inputs[PalmData$wave == 2018]


summary(TFP12)
summary(TFP15)
summary(TFP18)
