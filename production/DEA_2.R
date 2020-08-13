
# make Malmquist indices

table(PalmData$wave)
# 37 in 2012, 44 in 2015 and 45 in 2018

library(Benchmarking)
library(data.table)
library(dplyr)

PalmDataRestricted <- filter(PalmData, Inputs != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Labour != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Size != 0)

# test if output factors have zero values
PalmDataRestricted$hid[PalmDataRestricted$Production == 0]

# test if input factors have zero values

PalmDataRestricted$hid[PalmDataRestricted$Labour == 0]
PalmDataRestricted$hid[PalmDataRestricted$Inputs == 0]
PalmDataRestricted$hid[PalmDataRestricted$Size == 0]
PalmDataRestricted$hid[PalmDataRestricted$TreeAge == 0]
PalmDataRestricted$hid[PalmDataRestricted$TreeDensity == 0]

# after data revision I noticed that I need to restrict the data set even further as there were
# several replacement plots. This will reduce the number of observations to 

##  31 for 2015 to 2018
##  23 for 2012 to 2015
## 22 for 2012 to 2018

##  18 for the complete time serie 2012-15-18

noTimeHid <- count( PalmDataRestricted, hid)
noTimeHid <- filter( noTimeHid, n == 1 )

PalmDataRestricted <- PalmDataRestricted[! PalmDataRestricted$hid %in% noTimeHid$hid , ]

table(PalmDataRestricted$wave)

# finally, 31 matched ovservations for 2018 and 2015 are needed
#####################################################################################################

# hids_1518 <- c(40, 71, 181, 191, 325, 326, 334, 338, 342, 347, 349, 351, 355, 358, 382, 387, 388, 395,
#               403, 416, 419, 423, 424, 460, 467, 481, 483, 486, 488, 489, 615)

library(data.table)

hids_1518 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2018] %in%
              PalmDataRestricted$hid[PalmDataRestricted$wave == 2015]
hids2018 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2018]
hids_1518 <- hids2018[hids_1518]

PalmDataRestricted <- as.data.table(PalmDataRestricted)
PalmDataRestricted <- setDT(PalmDataRestricted)[hid %in% hids_1518]
PalmDataRestricted <- setDT(PalmDataRestricted)[wave != 2012]

# test if correctly restricted:

setDT(PalmDataRestricted)[wave == 2015][,hid] == setDT(PalmDataRestricted)[wave == 2018][,hid]

PalmDataRestricted <- filter(PalmDataRestricted, Labour != 0)

 # some corrections have to be done to obtain proper results, on 9th March 2020

Y0 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2015])) # restrict for 2015 and 2018

X0 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2015],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2015],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2015] ))

                    # PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2015], 
                    # PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2015])) 

# quadratic terms excluded

Y1 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2018])) # restrict for 2015 and 2018

X1 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2018],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2018],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2018]))
                    # PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2018],
                    #  PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2018] ))

# quadratic terms excluded

Malmquist2015_18 <- malmq(X0, Y0,, X1, Y1,, RTS = "crs") # the double commas are intentional

# CRS applied!


table(Malmquist2015_18$m)

############### For 2012 - 2015 change #########################################################


# hids_1215 <- c(191, 325, 326, 349, 351, 354, 355, 358, 379, 382, 387, 388, 40, 415, 419, 423, 424, 460, 481, #486,  488, 495, 629)

PalmDataRestricted <- filter(PalmData, Inputs != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Labour != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Size != 0)

hids_1215 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2012] %in%
               PalmDataRestricted$hid[PalmDataRestricted$wave == 2015]
hids2012 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2012]
hids_1215 <- hids2012[hids_1215]

PalmDataRestricted <- as.data.table(PalmDataRestricted)
PalmDataRestricted <- setDT(PalmDataRestricted)[hid %in% hids_1215]
PalmDataRestricted <- setDT(PalmDataRestricted)[wave != 2018]

# test if correctly restricted:

setDT(PalmDataRestricted)[wave == 2012][,hid] == setDT(PalmDataRestricted)[wave == 2015][,hid]


Y0 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2012]))

X0 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2012],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2012],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2012]))

                    #  PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2012],
                    #  PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2012]))

# quadratic terms excluded

Y1 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2015]))

X1 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2015],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2015],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2015]))
                    # PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2015],
                    #  PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2015]))

# quadratic terms excluded

Malmquist2012_15 <- malmq(X0, Y0,, X1, Y1,, RTS = "crs") # the double commas are intentional

# CRS applied!

table(Malmquist2012_15$m)

# there is one data spot which returns infinity. Is this a measurment error? it seems like an outlier.

ggplot(data = PalmDataRestricted, aes(PalmDataRestricted$Size, PalmDataRestricted$Production)) +
  geom_point(aes(colour=PalmDataRestricted$wave)) + ggtitle("Production over size for 2012-2015")+
  theme_classic()

 ################## For 2012 - 2018  ##############################################################

PalmDataRestricted <- filter(PalmData, Inputs != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Labour != 0)
PalmDataRestricted <- filter(PalmDataRestricted, Size != 0)

hids_1218 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2012] %in%
  PalmDataRestricted$hid[PalmDataRestricted$wave == 2018]

hids2012 <- PalmDataRestricted$hid[PalmDataRestricted$wave == 2012]

hids_1218 <- hids2012[hids_1218]

library(data.table)

PalmDataRestricted <- as.data.table(PalmDataRestricted)
PalmDataRestricted <- setDT(PalmDataRestricted)[hid %in% hids_1218]
PalmDataRestricted <- setDT(PalmDataRestricted)[wave != 2015]

# test if correctly restricted:

setDT(PalmDataRestricted)[wave == 2012][,hid] == setDT(PalmDataRestricted)[wave == 2018][,hid]


Y0 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2012])) # restrict for 2012 and 2018

X0 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2012],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2012],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2012]))
                     # PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2012],
                     # PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2012]))


# quadratic terms excluded

Y1 <- as.matrix(c(PalmDataRestricted$Production[PalmDataRestricted$wave == 2018])) # restrict for 2015 and 2018

X1 <- as.matrix(cbind(PalmDataRestricted$Size[PalmDataRestricted$wave == 2018],
                      PalmDataRestricted$Labour[PalmDataRestricted$wave == 2018],
                      PalmDataRestricted$Inputs[PalmDataRestricted$wave == 2018] ))
                      # PalmDataRestricted$TreeDensity[PalmDataRestricted$wave == 2018],
                      # PalmDataRestricted$TreeAge[PalmDataRestricted$wave == 2018]))
# quadratic terms excluded

Malmquist2012_18 <- malmq(X0, Y0,, X1, Y1,, RTS = "crs") # the double commas are intentional

# CRS applied!

hist(Malmquist2012_15$m)
hist(Malmquist2015_18$m)
hist(Malmquist2012_18$m)

summary(Malmquist2012_15$m)
summary(Malmquist2015_18$m)
summary(Malmquist2012_18$m)

summary(Malmquist2012_15$ec)
summary(Malmquist2015_18$ec)
summary(Malmquist2012_18$ec)

summary(Malmquist2012_15$tc)
summary(Malmquist2015_18$tc)
summary(Malmquist2012_18$tc)

# some observations are available in one wave and not in the other which gives us an Inf as a result
# need to fix that

plot(Malmquist2012_15$ec, Malmquist2012_15$tc)
plot(Malmquist2015_18$ec, Malmquist2015_18$tc)
plot(Malmquist2012_18$ec, Malmquist2012_18$tc)

plot(Malmquist2012_15$m, Malmquist2012_15$tc)
plot(Malmquist2015_18$m, Malmquist2015_18$tc)
plot(Malmquist2012_18$m, Malmquist2012_18$tc)

plot(Malmquist2012_15$m, Malmquist2012_15$ec)
plot(Malmquist2015_18$m, Malmquist2015_18$ec)
plot(Malmquist2012_18$m, Malmquist2012_18$ec)


plot( Malmquist2012_15$mq, ddfchange$`Inefficiency growth 2012-2015`)



# bootstrap example

mijo = NULL

for(i in 1:10000){
  
  sam = sample (deaVRSinputoriented$eff, 4, replace = T )
  miwe <- mean(sam)
  mijo[i] <- miwe
}

hist(mijo)



