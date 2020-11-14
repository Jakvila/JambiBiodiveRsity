library(nonparaeff)
library(data.table)
library(dplyr)
library(tidyr)
waves <- c(2012, 2015, 2018)

# transform inefficiency into ENVIRONMENTAL PERFORMANCE
ddfresults$ddfscore <- ( 1 -  ddfresults$ddfscore )

names(ddfresults) <- c("hid", "EnvPerf", "wave")

##############################################################################################
# calculate changes for 2012-2015
direc.dea_2012_2015 <- subset(ddfresults, wave !=2018)
table(table(MLPI2012_2015$hid))

# for better overview: long to wide transformation
direc.dea_2012_2015 <- spread(direc.dea_2012_2015, wave, EnvPerf)

# calculate growth of Environmental Performance from 2012 to 2015
direc.dea_2012_2015$EnvPerfChange <- (direc.dea_2012_2015$`2015` / direc.dea_2012_2015$`2012`)

##############################################################################################
# Calculate changes for 2015-2018

direc.dea_2015_2018 <- subset(ddfresults, wave !=2012)
table(table(direc.dea_2015_2018$hid))

#for better overview: long to wide transformation
direc.dea_2015_2018 <- spread(direc.dea_2015_2018, wave, EnvPerf)


direc.dea_2015_2018$EnvPerfChange <- ( direc.dea_2015_2018$`2018` / direc.dea_2015_2018$`2015` )
round(direc.dea_2012_2015$EnvPerfChange, digits = 3)

##############################################################################################
# Calculate changes for 2012-2018

direc.dea_2012_2018 <- ddfresults
table(table(direc.dea_2012_2018$hid))

#for better overview: long to wide transformation
direc.dea_2012_2018 <- spread(direc.dea_2012_2018, wave, EnvPerf)

direc.dea_2012_2018$EnvPerfChange <- ( direc.dea_2012_2018$`2018` /
                                         direc.dea_2012_2018$`2012` )
round(direc.dea_2012_2018$EnvPerfChange, digits = 3)


# density of environmental performance

library(ggplot2)
library(ggrepel)
ddfresults$wave <- as.factor(ddfresults$wave)
ggplot(ddfresults, aes( x = ddfscore, fill = wave)) + geom_density(alpha = 0.25) +
  xlab("inefficiencies")+
  theme_classic()


####################################################################################
# MPI measure for TFP change and Environmental performance graph
##################################################
# combine TFPC and ENVPERFC results from 2012-2015

# 1st step: data restriction
EnvPerfChanges <- merge(direc.dea_2012_2015, direc.dea_2015_2018, by="hid", all = T)

TFPC_EnvPerf_1215 <- as.data.frame(cbind(hids_1215, Malmquist2012_15$m))
names(TFPC_EnvPerf_1215) <- c("hid", "TFPC")

TFPC_EnvPerf_1215   <-  merge(EnvPerfChanges, TFPC_EnvPerf_1215, by= "hid")

TFPC_EnvPerf_1215   <- subset(TFPC_EnvPerf_1215, select = c(1, 4, 8))
TFPC_EnvPerf_1215[,3] <- as.numeric(as.character(TFPC_EnvPerf_1215[,3]))
names(TFPC_EnvPerf_1215) <- c("hid", "ENVPERF", "TFPC")

plot(TFPC_EnvPerf_1215$ENVPERF, TFPC_EnvPerf_1215$TFPC)

# 2nd steph: write graph for changes 2012 - 2015

ggplot(TFPC_EnvPerf_1215, aes( x = TFPC , y = ENVPERF )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 1, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
    xlab("TFP change with desirable output") +
    ylab("Environmental performance growth") + 
    theme_classic() +
    geom_smooth(method = "lm" , se = FALSE ) +
    xlim(0, 2.5) +
    ylim(0, 2)

ggsave("MPITFPCH_ENVPERFCH_2012-2015.png")

##################################################
# combine TFPC and ENVPERFC results from 2015-2018

# 1st step: data restriction

TFPC_EnvPerf_1518 <- as.data.frame(cbind(hids_1518, Malmquist2015_18$m))
names(TFPC_EnvPerf_1518) <- c("hid", "TFPC")

TFPC_EnvPerf_1518   <-  merge(EnvPerfChanges, TFPC_EnvPerf_1518, by= "hid")

TFPC_EnvPerf_1518   <- subset(TFPC_EnvPerf_1518, select = c(1, 7, 8))
TFPC_EnvPerf_1518[,3] <- as.numeric(as.character(TFPC_EnvPerf_1518[,3]))
names(TFPC_EnvPerf_1518) <- c("hid", "ENVPERF", "TFPC")

# drop outliers

TFPC_EnvPerf_1518 <- TFPC_EnvPerf_1518[ TFPC_EnvPerf_1518$ENVPERF != Inf, ]
TFPC_EnvPerf_1518 <- TFPC_EnvPerf_1518[ TFPC_EnvPerf_1518$ENVPERF <= 3, ]
TFPC_EnvPerf_1518 <- TFPC_EnvPerf_1518[ TFPC_EnvPerf_1518$TFPC    <= 3, ]


# 2nd steph: write graph for changes 2015 - 2018

ggplot(TFPC_EnvPerf_1518, aes( x = TFPC , y = ENVPERF )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 1, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("TFP change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm" , se = FALSE ) +
  xlim(0, 2.5) +
  ylim(0, 2)

ggsave("MPITFPCH_ENVPERFCH_2015-2018.png")


##################################################
# combine TFPC and ENVPERFC results from 2012-2018

# 1st step: data restriction

TFPC_EnvPerf_1218        <- as.data.frame(cbind(hids_1218, Malmquist2012_18$m))
names(TFPC_EnvPerf_1218) <- c("hid", "TFPC")

TFPC_EnvPerf_1218        <-  merge(direc.dea_2012_2018, TFPC_EnvPerf_1218, by= "hid")
TFPC_EnvPerf_1218$ENVPERFCH1218 <- TFPC_EnvPerf_1218$`2018`/TFPC_EnvPerf_1218$`2012`
TFPC_EnvPerf_1218        <- subset(TFPC_EnvPerf_1218, select = c(1, 6, 7))
TFPC_EnvPerf_1218[,2]    <- as.numeric(as.character(TFPC_EnvPerf_1218[,2]))
names(TFPC_EnvPerf_1218) <- c("hid", "TFPC", "ENVPERFCH")


# drop NaN and Inf values

TFPC_EnvPerf_1218 <- TFPC_EnvPerf_1218[ TFPC_EnvPerf_1218$TFPC <= 3, ]
TFPC_EnvPerf_1218 <- TFPC_EnvPerf_1218[ TFPC_EnvPerf_1218$ENVPERFCH <= 3, ]

# 2nd steph: write graph for changes 2015 - 2018

ggplot(TFPC_EnvPerf_1218, aes( x = TFPC , y = ENVPERFCH )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 1, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("TFP change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE ) +
  xlim(0, 2.5) +
  ylim(0, 2)

ggsave("MPITFPCH_ENVPERFCH_2012-2018.png")

# result: a negative correlation between the two figures from 2012 to 2018.


############################ COUNTERCHECK: DATA RESTRICTION TO HOUSEHOLDS WHICH 
############################                REPORTED RESULTS IN ALL THREE WAVES
###############################################################################
##### test 15-18 with different data restriction, without outliers ############

TFPC_EnvPerf_1518 <- as.data.frame(cbind(hids_1518, Malmquist2015_18$m))
names(TFPC_EnvPerf_1518) <- c("hid", "TFPC")

TFPC_EnvPerf_1518   <-  merge(EnvPerfChanges, TFPC_EnvPerf_1518, by= "hid")
TFPC_EnvPerf_1518   <-  subset(TFPC_EnvPerf_1518, TFPC_EnvPerf_1518$hid %in% hids_1218)

TFPC_EnvPerf_1518   <- subset(TFPC_EnvPerf_1518, select = c(1, 7, 8))
TFPC_EnvPerf_1518[,3] <- as.numeric(as.character(TFPC_EnvPerf_1518[,3]))
names(TFPC_EnvPerf_1518) <- c("hid", "ENVPERFCH", "TFPC")

# drop NaN and Inf values

TFPC_EnvPerf_1518 <- TFPC_EnvPerf_1518[ TFPC_EnvPerf_1518$ENVPERFCH != Inf, ]


# 2nd steph: write graph for changes 2015 - 2018

ggplot(TFPC_EnvPerf_1518, aes( x = TFPC , y = ENVPERFCH )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 1, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("TFP change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE ) +
  xlim(0, 2.5) +
  ylim(0, 2)

# result: the direction of the correlation does not change, thus it is 
# reasonable to keep additional observations which are not available in
# the other period.


###############################################################################
##### test 12-15 with different data restriction and without outliers #########

TFPC_EnvPerf_1215 <- as.data.frame(cbind(hids_1215, Malmquist2012_15$m))
names(TFPC_EnvPerf_1215) <- c("hid", "TFPC")

TFPC_EnvPerf_1215   <-  merge(EnvPerfChanges, TFPC_EnvPerf_1215, by= "hid")
TFPC_EnvPerf_1215   <-  subset(TFPC_EnvPerf_1215, TFPC_EnvPerf_1215$hid %in%
                                                                   hids_1218)
TFPC_EnvPerf_1215   <- subset(TFPC_EnvPerf_1215, select = c(1, 4, 8))
TFPC_EnvPerf_1215[,3] <- as.numeric(as.character(TFPC_EnvPerf_1215[,3]))
names(TFPC_EnvPerf_1215) <- c("hid", "ENVPERFCH", "TFPC")

# drop outliers 

TFPC_EnvPerf_1215 <- TFPC_EnvPerf_1215[ TFPC_EnvPerf_1215$ENVPERFCH <= 2.5, ]
TFPC_EnvPerf_1215 <- TFPC_EnvPerf_1215[ TFPC_EnvPerf_1215$TFPC <= 2.5, ]

# 2nd steph: write graph for changes 2012 - 2015

ggplot(TFPC_EnvPerf_1215, aes( x = TFPC , y = ENVPERFCH )) +
  geom_vline (xintercept = 1, linetype = "dotted") +
  geom_hline (yintercept = 1, linetype = "dotted") +
  geom_point(color = "red") +
  geom_text_repel(aes(label =hid),
                  size =2.5,
                  box.padding   = 0.1, 
                  point.padding = 0.5,
                  segment.color = 'grey25') +
  xlab("TFP change with desirable output") +
  ylab("Environmental performance growth") + 
  theme_classic() +
  geom_smooth(method = "lm" , se = FALSE ) +
  xlim(0, 2.5) +
  ylim(0, 2)

ggsave("MPITFPCH_ENVPERFCH_2012-2015.png")

# result: the direction of the correlation does not change neither albeit the exclusion
# of the three outliers which report an environmental performance change of > 3 or TFPC > 3

