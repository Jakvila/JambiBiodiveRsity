# Replicate DEA in Henningsen 2019 book for our palmdata

table(PalmData$wave)
# 37 in 2012, 44 in 2015 and 45 in 2018

library(Benchmarking)
library(data.table)
library(dplyr)

# test if output factors have zero values
PalmData$hid[PalmData$Production == 0]

# test if input factors have zero values

PalmData$hid[PalmData$Labour == 0]
PalmData$hid[PalmData$Inputs == 0]
PalmData$hid[PalmData$Size == 0]
PalmData$hid[PalmData$TreeAge == 0]
PalmData$hid[PalmData$TreeDensity == 0]

# Restrict to the correct data set and kick out observations with Inputs == 0 and Labour == 0

PalmDataRestricted <- filter(PalmData, Inputs != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Size != 0)

# after correct restriction, 114 obs are left, 34 over three years and 6 that go from 2015 to 2018

table(PalmDataRestricted$wave)

  inputMat <- cbind(PalmDataRestricted$Inputs, PalmDataRestricted$Labour, PalmDataRestricted$Size,
                    PalmDataRestricted$TreeAge, PalmDataRestricted$TreeDensity) #, PalmDataRestricted$wave)

# corrected input matrix after having decided that quadratic term cannot be employed for DEA
  
  outputVec <- PalmDataRestricted$Production


deaVRSinputoriented <- dea(inputMat, outputVec)

hist(eff(deaVRSinputoriented))

peers(deaVRSinputoriented)[1:14,]

# displaying the 'peers' of the first 14 observations

lambda(deaVRSinputoriented)

# display the "slack"

deaVRSinputoriented <- dea(inputMat, outputVec, SLACK = T)
table(deaVRSinputoriented$slack)

## Input oriented DEA with CRS


deaCRSinputoriented <- dea(inputMat, outputVec, RTS = "crs")
hist(eff(deaCRSinputoriented))

# scale efficiencies

se_PalmDataRestricted <- eff(deaCRSinputoriented) / eff(deaVRSinputoriented)

hist(se_PalmDataRestricted)


## output-oriented efficiencies

deaVRSoutputoriented <- dea(inputMat, outputVec, ORIENTATION = "out")
hist(eff(deaVRSoutputoriented))

# output-oriented DEA with CRS

deaCRSoutputoriented <- dea( inputMat, outputVec, RTS = "crs", ORIENTATION = "out" )
hist( eff( deaCRSoutputoriented ) )


all.equal(eff(deaCRSinputoriented), 1 / eff(deaCRSoutputoriented))

# graph hyperbolic efficiencies

deaVRSgraph <- dea(inputMat, outputVec, ORIENTATION = "graph")
hist(eff(deaVRSgraph))
plot(eff(deaVRSinputoriented), eff(deaVRSgraph))
abline(0,1)

library(ggplot2)
  effplot <- data.frame(eff(deaVRSinputoriented), eff(deaVRSgraph))
ggplot(data = effplot, aes(effplot$eff.deaVRSinputoriented., effplot$eff.deaVRSgraph.)) +
  geom_point(aes(colour=PalmDataRestricted$wave)) + ggtitle("Input oriented efficiencies with VRS")+
  theme_minimal()

# scatterplot input-output
plot(PalmDataRestricted$Production, PalmDataRestricted$Inputs)
abline(0,0.02945)
