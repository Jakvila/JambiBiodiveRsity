###################################################################################
######################### MPI - TFP and environmental performance for each wave  ##
######################### added October 31st, 2020 ################################
###################################################################################

library(data.table)


# preparation: transform inefficiency into env. performance measure
EnvPerfScore <- ddfresults
EnvPerfScore$ddfscore <- 1 - EnvPerfScore$ddfscore

################### 2012
# take general MPI efficiency (TFP) of 2012
MPI_TFP_results_2012 <- cbind.data.frame(Malmquist2012_15$e00, as.character(hids_1215))
MPI_TFP_results_2012$wave <- 2012
names(MPI_TFP_results_2012) <- c("MPI_TFP", "hid", "wave")

# merge both scores for 2012
MPI_EnvPerf_2012 <- merge(MPI_TFP_results_2012, EnvPerfScore, by.x = c("hid", "wave"),
                                          by.y = c("hid", "wave"))
plot(MPI_EnvPerf_2012$ddfscore, MPI_EnvPerf_2012$MPI_TFP)


################### 2015
# take general MPI efficiency (TFP) of reference period 0 (2015)
MPI_TFP_results_2015 <- cbind.data.frame(as.numeric(Malmquist2015_18$e00), as.character(hids_1518))
MPI_TFP_results_2015$wave <- 2015
names(MPI_TFP_results_2015) <- c("MPI_TFP", "hid", "wave")

# merge both scores for 2012
MPI_EnvPerf_2015 <- merge(MPI_TFP_results_2015, EnvPerfScore, by.x = c("hid", "wave"),
                          by.y = c("hid", "wave"))
plot(MPI_EnvPerf_2015$ddfscore, MPI_EnvPerf_2015$MPI_TFP)


################### 2018
# take general MPI efficiency (TFP) of reference period 1 (2018)
MPI_TFP_results_2018 <- cbind.data.frame(as.numeric(Malmquist2015_18$e11), as.character(hids_1518))
MPI_TFP_results_2018$wave <- 2018
names(MPI_TFP_results_2018) <- c("MPI_TFP", "hid", "wave")

# merge both scores for 2012
MPI_EnvPerf_2018 <- merge(MPI_TFP_results_2018, EnvPerfScore, by = c("hid", "wave"))
plot(MPI_EnvPerf_2018$ddfscore, MPI_EnvPerf_2018$MPI_TFP)


###############################
library(ggpubr)

# construct ggplots

# 2012
plot_EnvPerf_MPI_2012 <- ggplot(MPI_EnvPerf_2012, aes( x = ddfscore, y = MPI_TFP)) +
                          geom_point() +
                          ggtitle("2012") +
                          geom_smooth(method = "lm", se= FALSE) +
                          theme_classic() +
                            xlab("Environmental performance") + 
                            ylab("MPI")

# 2015
plot_EnvPerf_MPI_2015 <- ggplot(MPI_EnvPerf_2015, aes( x = ddfscore, y = MPI_TFP)) +
                          geom_point() +
                          ggtitle("2015") +
                          geom_smooth(method = "lm", se= FALSE) +
                          theme_classic() +
                            xlab("Environmental performance") + 
                            ylab("MPI")

# 2018
plot_EnvPerf_MPI_2018 <- ggplot(MPI_EnvPerf_2018, aes( x = ddfscore, y = MPI_TFP)) +
                          geom_point() +
                          ggtitle("2018") +
                          geom_smooth(method = "lm", se= FALSE) +
                          theme_classic() +
                            xlab("Environmental performance") + 
                            ylab("MPI")

# all
MPI_EnvPerf <- rbind.data.frame(MPI_EnvPerf_2012, MPI_EnvPerf_2015, MPI_EnvPerf_2018)
MPI_EnvPerf$wave <- as.factor(MPI_EnvPerf$wave)

ggplot(MPI_EnvPerf, aes( x = ddfscore, y = MPI_TFP)) +
  geom_point(aes(colour = MPI_EnvPerf$wave)) +
  labs(colour = "Wave") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_classic() +
  xlab("Environmental performance") + 
  ylab("MPI")

ggsave("plot_MPI_EnvPerformance_combined.png")


ggarrange(plot_EnvPerf_MPI_2012, plot_EnvPerf_MPI_2015,
          plot_EnvPerf_MPI_2018)
ggsave("MPI_EnvPerformance_PerWave_plots.png")


# write table with both results
require(kableExtra)
MPI_EnvPerf_table <- cbind.data.frame( c(2012, 2015, 2018),
                                      rbind(mean(MPI_EnvPerf_2012$MPI_TFP),
                                            mean(MPI_EnvPerf_2015$MPI_TFP),
                                            mean(MPI_EnvPerf_2018$MPI_TFP)),
                                      rbind(mean(MPI_EnvPerf_2012$ddfscore),
                                            mean(MPI_EnvPerf_2015$ddfscore),
                                            mean(MPI_EnvPerf_2018$ddfscore)))

names(MPI_EnvPerf_table) <- c("Year", "MPI", "ENVPERF")
kable(MPI_EnvPerf_table, booktabs = T, format = "latex")

                 