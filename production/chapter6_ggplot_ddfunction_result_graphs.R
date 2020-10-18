### This rscript writes the graphs of chapter 6
###

library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
require(ggpubr)

plot(PalmDataRestricted$Production, PalmDataRestricted$Size)
plot(PalmDataRestricted$Production, PalmDataRestricted$Labour)
plot(PalmDataRestricted$Production, PalmDataRestricted$Inputs)

plot(PalmDataRestricted$Production, PalmDataRestricted$ShannonEst)

PalmDataWithEffScores <- PalmDataRestricted %>%
  select(hid, wave, Production, ShannonEst, Size, Labour, Inputs)
ddfresults <- as.data.frame(ddfresults)

PalmDataWithEffScores <- merge(PalmDataWithEffScores, ddfresults,
                               by.x = c("hid", "wave"), by.y = c("hid", "wave"))


########## Inputs and environmental performance

# CPO Production 
envperf_plot1 <- ggplot(PalmDataWithEffScores, aes( x = Production, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Desirable output") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("CPO production (kg)") + 
  ylab("Environmental performance")

# plot size
envperf_plot2 <- ggplot(PalmDataWithEffScores, aes( x = Size, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Input 1") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Plot Size (ha)") + 
  ylab("Environmental performance") + 
  xlim(0, 6)

# labour
envperf_plot3 <- ggplot(PalmDataWithEffScores, aes( x = Labour, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Input 2") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Labour (hours)") + 
  ylab("Environmental performance")

# agrochemicals
envperf_plot4 <- ggplot(PalmDataWithEffScores, aes( x = Inputs, y = ( 1 - ddfscore))) +
  geom_point(aes(colour = PalmDataWithEffScores$wave)) +
  labs(colour = "Wave") +
  ggtitle("Input 3") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Agrochemicals (kg)") + 
  ylab("Environmental performance ") + 
  xlim(0, 4000)

## combine plots in one graph
ggarrange(envperf_plot1, envperf_plot2, envperf_plot3, envperf_plot4)

ggsave("EnvPerformance_components.png")


######### pro forma check: positive linear correlation between environmental performance and ESN

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
  ggtitle("Environmental performance and CPO production") +
  geom_smooth(method = "lm") +
  theme_classic() +
  ylab("Environmental performance") +
  xlab("CPO production (kg)") + 
  xlim(0, 100000)

plot(PalmData$ShannonEst, PalmData$Production)

ggplot(PalmDataRestricted, aes( x = ShannonEst, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave)) +
  labs(colour = "Wave") +
  ggtitle("Effective number of species and CPO production") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("CPO Production (kg)")

ggplot(PalmDataRestricted, aes( x = ShannonEst, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave,
                 size = PalmDataRestricted$Size)) +
  labs(colour = "Wave", size = "Plot size (ha)") +
  ggtitle("Effective number of species and production") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("Production (kg)") # the same graph including the plot size

