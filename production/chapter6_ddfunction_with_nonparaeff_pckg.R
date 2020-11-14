library(nonparaeff)
library(data.table)
library(dplyr)
library(tidyr)
waves <- c(2012, 2015, 2018)

# We start with 2018, the most recent wave
PalmData2018 <- as.data.table(PalmDataRestricted)
PalmData2018$wave <- as.numeric(PalmData2018$wave)
PalmData2018$hid <- as.numeric(PalmData2018$hid)

PalmData2018 <- subset(PalmData2018, wave ==2018)
PalmData2018 <- PalmData2018 %>% select(hid, Size, Labour, Inputs,
                                              Production, ShannonEst)  # EDITED, caution!
PalmData2018$ShannonEst <- PalmData2018$ShannonEst^(-1)

direc.dea(base = PalmData2018inputs, frontier = PalmData2018inputs, ngood = 1, nbad = 1)
ddfresults2018 <- as.data.frame(cbind(PalmData2018$hid,
                                      direc.dea(base = PalmData2018, ngood = 1, nbad = 1)))
ddfresults2018$wave <- 2018
table(direc.dea(base = PalmData2018, ngood = 1, nbad = 1))

# 2015 wave

PalmData2015 <- as.data.table(PalmDataRestricted)
PalmData2015$wave <- as.numeric(PalmData2015$wave)
PalmData2015$hid <- as.numeric(PalmData2015$hid)

PalmData2015 <- subset(PalmData2015, wave ==2015)
PalmData2015 <- PalmData2015 %>% select(hid, Size, Labour, Inputs,
                                        Production, ShannonEst)  # EDITED, caution!
PalmData2015$ShannonEst <- PalmData2015$ShannonEst^(-1)


direc.dea(base = PalmData2015, ngood = 1, nbad = 1)
ddfresults2015 <- as.data.frame(cbind(PalmData2015$hid,
                                      direc.dea(base = PalmData2015, ngood = 1, nbad = 1)))
ddfresults2015$wave <- 2015
table(direc.dea(base = PalmData2015, ngood = 1, nbad = 1))

# 2012 wave

PalmData2012 <- as.data.table(PalmDataRestricted)
PalmData2012$wave <- as.numeric(PalmData2012$wave)
PalmData2012$hid <- as.numeric(PalmData2012$hid)

PalmData2012 <- subset(PalmData2012, wave ==2012)
PalmData2012 <- PalmData2012 %>% select(hid, Size, Labour, Inputs,
                                        Production, ShannonEst)  # EDITED, caution!
PalmData2012$ShannonEst <- PalmData2012$ShannonEst^(-1)

direc.dea(base = PalmData2012, ngood = 1, nbad = 1)
ddfresults2012 <- as.data.frame(cbind(PalmData2012$hid,
                                      direc.dea(base = PalmData2012, ngood = 1, nbad = 1)))
ddfresults2012$wave <- 2012
table(direc.dea(base = PalmData2012, ngood = 1, nbad = 1))

# merge data frame with DDF results for all three waves

ddfresults <- rbind(ddfresults2012,
                    ddfresults2015,
                    ddfresults2018)


names(ddfresults) <- c("hid", "ddfscore", "wave")

table(round(ddfresults$ddfscore, digits=2))
table(ddfresults$hid)

MLPI2012_2015 <- subset(ddfresults, wave !=2018)
table(table(MLPI2012_2015$hid))

# mean inefficiencies

mean(ddfresults$ddfscore[ddfresults$wave == 2012])
median(ddfresults$ddfscore[ddfresults$wave == 2012])
mean(ddfresults$ddfscore[ddfresults$wave == 2015])
median(ddfresults$ddfscore[ddfresults$wave == 2015])
mean(ddfresults$ddfscore[ddfresults$wave == 2018])
median(ddfresults$ddfscore[ddfresults$wave == 2018])


#for better overview: long to wide transformation
MLPI2012_2015 <- spread(MLPI2012_2015, wave, ddfscore)

# drop all the observations with NA or 0 as they cannot be taken into account for any calculation
# MLPI2012_2015[MLPI2012_2015 == 0] <- NA
# MLPI2012_2015 <- MLPI2012_2015[complete.cases(MLPI2012_2015),]

# problematic, because some observations are deleted which could be needed afterwards for the other
# changes. Better keep all of them for the moment and make calculations only with those hids which are
# unequal to zero. 


(MLPI2012_2015$`2015`-MLPI2012_2015$`2012`) / MLPI2012_2015$`2012`
MLPI2012_2015$inef_growth<- (MLPI2012_2015$`2015`/ MLPI2012_2015$`2012`)
  
  
 # MLPI2012_2015 <- subset(MLPI2012_2015, hid == MLPI2012_2015$hid[duplicated(MLPI2012_2015$hid)])
# subset(MLPI2012_2015, hid %in% MLPI2012_2015$hid[duplicated(MLPI2012_2015$hid)])

MLPI2015_2018 <- subset(ddfresults, wave !=2012)
table(table(MLPI2015_2018$hid))

#for better overview: long to wide transformation
MLPI2015_2018 <- spread(MLPI2015_2018, wave, ddfscore)

# drop all the observations with NA or 0 as they cannot be taken into account for any calculation
# MLPI2015_2018[MLPI2015_2018 == 0] <- NA
# MLPI2015_2018 <- MLPI2015_2018[complete.cases(MLPI2015_2018),]
(MLPI2015_2018$`2018`-MLPI2015_2018$`2015`) / MLPI2015_2018$`2015`
MLPI2015_2018$inef_growthrate <- ((MLPI2015_2018$`2018`-MLPI2015_2018$`2015`) / MLPI2015_2018$`2015`)
round(MLPI2015_2018$inef_growthrate, digits = 3)
# density of inefficiencies

library(ggplot2)
library(ggrepel)

ddfresults$wave <- as.factor(ddfresults$wave)

ggplot(ddfresults, aes( x = ddfscore, fill = wave)) + geom_density(alpha = 0.25) +
  xlab("inefficiencies")+
  theme_classic()

# inefficiency growth and Technical change 2012 - 2015
ddfchange     <- merge(MLPI2012_2015, MLPI2015_2018, by="hid", all = T)

TC_inef_graph <- as.data.frame(cbind(hids_1215, Malmquist2012_15$tc))
names(TC_inef_graph) <- c("hid", "TC")
  
TC_inef_graph   <-  merge(ddfchange, TC_inef_graph, by= "hid")

TC_inef_graph   <- subset(TC_inef_graph, select = c(1, 4, 8))
TC_inef_graph[,3] <- as.numeric(as.character(TC_inef_graph[,3]))
names(TC_inef_graph) <- c("hid", "inef_growth", "TC")

plot(TC_inef_graph$inef_growth, TC_inef_graph$TC)

# test graph 
ggplot(TC_inef_graph, aes( x = inef_growth, y = TC)) +
        geom_point(color = "red") +
        geom_text_repel(aes(label =hid),
                         size =2.5,
                         box.padding   = 0.1, 
                         point.padding = 0.5,
                         segment.color = 'grey25') +
        ggtitle("Inefficiency growth and technical change 2012 to 2015") +
        theme_classic()


##############################################################################
# 1st graph for 12-15

# inefficiency growth and Technical Efficiency change 2012 - 2015

# data restriction 
EC_inef_graph1215 <- as.data.frame(cbind(hids_1215, Malmquist2012_15$ec))
names(EC_inef_graph1215) <- c("hid", "EC")

EC_inef_graph1215   <-  merge(ddfchange, EC_inef_graph1215, by= "hid")

EC_inef_graph1215   <- subset(EC_inef_graph1215, select = c(1, 4, 8))
EC_inef_graph1215[,3] <- as.numeric(as.character(EC_inef_graph1215[,3]))
names(EC_inef_graph1215) <- c("hid", "inef_growth", "EC")

plot(EC_inef_graph1215$inef_growth, EC_inef_graph1215$EC)

# graph 12-15
ggplot(EC_inef_graph1215, aes( x = (1 - inef_growth), y = EC)) +
  geom_vline (xintercept = 0, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  # ggtitle("Environmental performance growth and technical efficiency change from 2012 to 2015") +
  theme_classic() +
  xlim(0, 2) +
  ylim(0,4) +
  xlab("Environmental performance growth") +
  ylab("MPI technical efficiency change") + 
  geom_smooth(method = "lm")

###################################################
# Environmental performance growth, technical change and technical efficiency change 2012-15
###################################################
######################### added October 20th, 2020

# Env. performance and Technical Efficiency change 2012 - 2015

# data restriction 
TC_EnvPerf_1215 <- as.data.frame(cbind(hids_1215, Malmquist2012_15$tc))
names(TC_EnvPerf_1215) <- c("hid", "tc")

TC_EnvPerf_1215   <-  merge(ddfchange, TC_EnvPerf_1215, by= "hid")

TC_EnvPerf_1215   <- subset(TC_EnvPerf_1215, select = c(1, 4, 8))
TC_EnvPerf_1215[,3] <- as.numeric(as.character(TC_EnvPerf_1215[,3]))
names(TC_EnvPerf_1215) <- c("hid", "inef_growth", "tc")

plot(TC_EnvPerf_1215$inef_growth, TC_EnvPerf_1215$tc)

# drop NaN and Inf values

TFPC_EnvPerf_1518 <- TFPC_EnvPerf_1518[ TFPC_EnvPerf_1518$ENVPERF != Inf, ]

# graph 12-15
ggplot(TC_EnvPerf_1215, aes( x = tc, y = inef_growth*(-1))) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 0, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  theme_classic() +
  xlab("MPI technical change") +
  ylab("Environmental performance growth") + 
  geom_smooth(method = "lm" , se = FALSE ) +
  xlim(0.5,2) +
  ylim(-1,1) 

######################### Environmental performance growth and TECHNICAL EFFICIENCY CHANGE for 2012 - 2015

# data restriction
EC_EnvPerf_1215 <- as.data.frame(cbind(hids_1215, Malmquist2012_15$ec))
names(EC_EnvPerf_1215) <- c("hid", "ec")

EC_EnvPerf_1215   <-  merge(ddfchange, EC_EnvPerf_1215, by= "hid")
EC_EnvPerf_1215   <- subset(EC_inef_graph1215, select = c(1, 7, 8))
EC_EnvPerf_1215[,3] <- as.numeric(as.character(EC_EnvPerf_1215[,3]))
names(EC_EnvPerf_1215) <- c("hid", "inef_growth", "ec")

plot(EC_EnvPerf_1215$inef_growth, EC_EnvPerf_1215$ec)

# graph 15 -18
ggplot(EC_EnvPerf_1215, aes( x = ec , y = inef_growth*(-1))) +
    geom_vline (xintercept = 1, linetype = "dotted") +
    geom_hline (yintercept = 0, linetype = "dotted") +
    geom_point(color = "red") +
    geom_text_repel(aes(label = hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
      theme_classic() +
      geom_smooth(method = "lm" , se = FALSE ) +
        xlab("Environmental performance growth") +
        ylab("Malmquist technical efficiency change") +
         xlim(0,2) +
         ylim(-1,1) 

  # + ggtitle("Environmental performance growth and technical efficiency change from 2015 to 2018") 


######################### Environmental performance growth and technical change for 2015 - 2018
######################### added October 18th, 2020

# data restriction
TC_inef_graph_1518 <- as.data.frame(cbind(hids_1518, Malmquist2015_18$tc))
names(TC_inef_graph_1518) <- c("hid", "TC")

TC_inef_graph_1518   <- merge(ddfchange, TC_inef_graph_1518, by= "hid")
TC_inef_graph_1518   <- subset(TC_inef_graph_1518, select = c(1, 7, 8))
TC_inef_graph_1518[,3] <- as.numeric(as.character(TC_inef_graph_1518[,3]))
names(TC_inef_graph_1518) <- c("hid", "inef_growth", "tc")

plot(TC_inef_graph_1215$inef_growth, TC_inef_graph_1215$tc)

# technical change graph 15 - 18
ggplot(TC_inef_graph_1518, aes( x = tc , y = inef_growth*(-1) )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 0, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("Technical change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm" , se = FALSE ) +
 ylim(-1, 1)

############ same graph for TECHNICAL EFFICIENCY CHANGE 

# data restriction
EC_inef_graph_1518 <- as.data.frame(cbind(hids_1518, Malmquist2015_18$ec))
names(EC_inef_graph_1518) <- c("hid", "EC")

EC_inef_graph_1518   <- merge(ddfchange, EC_inef_graph_1518, by= "hid")
EC_inef_graph_1518   <- subset(EC_inef_graph_1518, select = c(1, 7, 8))
EC_inef_graph_1518[,3] <- as.numeric(as.character(EC_inef_graph_1518[,3]))
names(EC_inef_graph_1518) <- c("hid", "inef_growth", "ec")

plot(EC_inef_graph_1518$inef_growth, EC_inef_graph_1518$ec)

# technical efficiency change graph 15 -18
ggplot(EC_inef_graph_1518, aes( x = ec , y = inef_growth*(-1) )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 0, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("Technical efficiency change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm" , se = FALSE ) +
  xlim(0, 2.5) 


############ same graph for TFP Change, taking the general result of the Malmquist Productivity Index

# data restriction
TFPC_EnvPerf_graph_1518 <- as.data.frame(cbind(hids_1518, Malmquist2015_18$m))
names(TFPC_EnvPerf_graph_1518) <- c("hid", "TFPC")

TFPC_EnvPerf_graph_1518   <- merge(ddfchange, TFPC_EnvPerf_graph_1518, by= "hid")
TFPC_EnvPerf_graph_1518   <- subset(TFPC_EnvPerf_graph_1518, select = c(1, 7, 8))
TFPC_EnvPerf_graph_1518[,3] <- as.numeric(as.character(TFPC_EnvPerf_graph_1518[,3]))
names(EC_inef_graph_1518) <- c("hid", "inef_growth", "ec")

plot(EC_inef_graph_1518$inef_growth, EC_inef_graph_1518$ec)

# TOTAL FACTOR PRODUCTIVITY CHANGE graph 15 -18
ggplot(EC_inef_graph_1518, aes( x = ec , y = inef_growth*(-1) )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 0, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("Technical efficiency change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm" , se = FALSE ) +
  xlim(0, 2.5) 

#######################################################################################
################## Table with all environmental inefficiency and growth rates #########
################## included in the appendix ###########################################
#######################################################################################

library(stargazer)
library(knitr)

ddfchange[,3] <- ifelse(is.na(ddfchange$`2015.x`), ddfchange$`2015.y`, ddfchange$`2015.x`)
ddfchange     <- subset(ddfchange, select = c(1, 2, 3, 6, 4, 7))
ddfchange     <- round(ddfchange, digits = 3)
names(ddfchange) <- c("Household ID", "Inefficiency 2012", "Inefficiency 2015", "Inefficiency 2018",
                      "Inefficiency growth 2012-2015", "Inefficiency growth 2015-2018")

ddfresults_withmeans <- as.data.frame( cbind(ddfchange[,1], 
            rbind(as.matrix(ddfchange$`Inefficiency 2012`), rbind(
                                      median(ddfchange$`Inefficiency 2012`, na.rm = T),
                                      mean(ddfchange$`Inefficiency 2012`, na.rm = T))),
            rbind(as.matrix(ddfchange$`Inefficiency 2015`), rbind(
                                      median(ddfchange$`Inefficiency 2015`, na.rm = T),
                                      mean(ddfchange$`Inefficiency 2015`, na.rm = T))),
            rbind(as.matrix(ddfchange$`Inefficiency 2018`), rbind(
                                      median(ddfchange$`Inefficiency 2018`, na.rm = T),
                                      mean(ddfchange$`Inefficiency 2018`, na.rm = T)))
))

ddfchange[ddfchange == -1] <- NA
# zero growth needs to be included in the mean calculation
# ddfchange[ddfchange == 0] <- NA

ddfchange_withmeans <- as.data.frame( cbind(ddfchange[,1], 
                                            rbind(as.matrix(ddfchange[,5]), rbind(
                                              median(ddfchange[,5][is.finite(ddfchange[,5])], na.rm = T),
                                              mean(ddfchange[,5][is.finite(ddfchange[,5])], na.rm = T))),
                                            rbind(as.matrix(ddfchange[,6]), rbind(
                                              median(ddfchange[,6][is.finite(ddfchange[,6])], na.rm = T),
                                              mean(ddfchange[,6][is.finite(ddfchange[,6])], na.rm = T)))
))



 # ddfchange_withmeans <- as.data.frame( cbind(ddfchange_withmeans[,1], 
 #                                            rbind(as.matrix(ddfchange[,2]), rbind(
 #                                              median(ddfchange[,2][is.finite(ddfchange[,2])], na.rm = T),
  #                                             mean(ddfchange[,2][is.finite(ddfchange[,2])], na.rm = T))),
   #                                          rbind(as.matrix(ddfchange[,3]), rbind(
    #                                           median(ddfchange[,3][is.finite(ddfchange[,3])], na.rm = T),
     #                                          mean(ddfchange[,3][is.finite(ddfchange[,3])], na.rm = T)))
#         ))

 mean(ddfchange$`Inefficiency 2015`, na.rm = T)
mean(ddfchange$`Inefficiency 2018`, na.rm = T)
median(ddfchange$`Inefficiency 2012`, na.rm = T)
median(ddfchange$`Inefficiency 2015`, na.rm = T)
median(ddfchange$`Inefficiency 2018`, na.rm = T)

mean(ddfchange[,5][is.finite(ddfchange[,5])])
 mean(ddfchange[,6][is.finite(ddfchange[,6]) && ddfchange[,6]<1])


mean(ddfchange$`Inefficiency growth 2012-2015`, na.rm = T)
median(mean(ddfchange$`Inefficiency growth 2012-2015`, na.rm = T))

rbind(ddfchange, mean(ddfchange[,1]))

# Form Latex table with all the results

ddfresults_withmeans   <- round(ddfresults_withmeans, digits = 3)
  names(ddfresults_withmeans) <- c("HHID", "Inef. 12", "Inef. 15", "Inef. 18")
  kable(ddfresults_withmeans, format = "latex")
  
  
ddfchange_withmeans <- round(ddfchange_withmeans, digits = 3)
  names(ddfchange_withmeans) <- c("HHID", "Growth 12-15", "Growth 15-18")
  ddfchange_withmeans     <- subset(ddfchange_withmeans, select = c(2, 3))
  names(ddfchange_withmeans) <- c("Growth 12-15", "Growth 15-18")
  
# table used in Appendix 
kable(cbind(ddfresults_withmeans, ddfchange_withmeans), format = "latex")


# For the second subchapter of the results:
## 
# for this purpose, we need to make a long-to-wide transformation

library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)

plot(PalmDataRestricted$Production, PalmDataRestricted$Size)
plot(PalmDataRestricted$Production, PalmDataRestricted$Labour)
plot(PalmDataRestricted$Production, PalmDataRestricted$Inputs)

plot(PalmDataRestricted$Production, PalmDataRestricted$ShannonEst)

PalmDataWithEffScores <- PalmDataRestricted %>%
                            select(hid, wave, Production, ShannonEst, Size, Labour, Inputs)
ddfresults <- as.data.frame(ddfresults)

PalmDataWithEffScores <- merge(PalmDataWithEffScores, ddfresults,
                               by.x = c("hid", "wave"), by.y = c("hid", "wave"))


### Inputs and environmental performance

ggplot(PalmDataWithEffScores, aes( x = Size, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Environmental performance and plot Size") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Plot Size (ha)") + 
  ylab("Environmental performance") + 
  xlim(0, 6)

ggplot(PalmDataWithEffScores, aes( x = Labour, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Environmental performance and employed labour") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Labour (hours)") + 
  ylab("Environmental performance")

ggplot(PalmDataWithEffScores, aes( x = Inputs, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Environmental performance and agrochemical use") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Agrochemicals (kg)") + 
  ylab("Environmental performance") + 
  xlim(0, 4000)

### pro forma check: positive linear correlation between environmental performance and ESN

ggplot(PalmDataWithEffScores, aes( x = ShannonEst, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Environmental performance and effective number of species") +
  geom_smooth(method = "lm") +
  theme_classic()+
  xlab("ENS") +
  ylab("Environmental performance")

### environmental performance, ESN and production

ggplot(PalmDataWithEffScores, aes( x = Production, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Environmental performance and palm oil production") +
  geom_smooth(method = "lm") +
  theme_classic() +
  ylab("Environmental performance") +
  xlab("Production (kg)") + 
  xlim(0, 100000)

plot(PalmData$ShannonEst, PalmData$Production)

ggplot(PalmDataRestricted, aes( x = ShannonEst, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave)) +
  labs(colour = "Wave") +
  ggtitle("Effective number of species and production") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("Production (kg)")

ggplot(PalmDataRestricted, aes( x = ShannonEst, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave,
                 size = PalmDataRestricted$Size)) +
  labs(colour = "Wave", size = "Plot size (ha)") +
  ggtitle("Effective number of species and production") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("Production (kg)") # the same graph including the plot size

