
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
require(ggpubr)
library(RColorBrewer)
library(scales)


plot_prod_input1 <- ggplot(PalmDataRestricted, aes( x = Size, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave)) +
  labs(colour = "Wave") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("CPO Production (kg)") +
  xlab("Plot size (ha)") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma)

plot_prod_input2 <- ggplot(PalmDataRestricted, aes( x = Labour, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave)) +
  labs(colour = "Wave") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("CPO Production (kg)") +
  xlab("Labour (manhours)") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma)

plot_prod_input3 <- ggplot(PalmDataRestricted, aes( x = Inputs, y = Production)) +
  geom_point(aes(colour = PalmDataRestricted$wave)) +
  labs(colour = "Wave") +
  geom_smooth(method = "lm") +
  theme_classic() + 
  ylab("CPO Production (kg)") +
  xlab("Agrochemical use (kg)")+
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma)

plot_prod_factors <- ggarrange(plot_prod_input1, plot_prod_input2, plot_prod_input3,
          ncol = 1, nrow = 3)

#save the plots in high resolution

ggsave("plot_prod_factors.png")

# now a boxplot which replaces the table, because it looks nicer and gives a better overview

# for CPO production

plot_box_production <- ggplot(PalmDataRestricted, aes( x = wave, y = Production, fill = wave)) +
  geom_boxplot(alpha= 0.5) + theme(panel.background = element_blank(),
                                   axis.line = element_line(colour = "black"),
                                   legend.position="none") +
      scale_fill_brewer(palette="Dark2") +
       ylab("CPO production (kg)") +
        xlab("Wave") +
          ggtitle("Desirable output") +
            scale_y_continuous(label=comma, limits=c(0, 150000))

# for plot size

plot_box_size <- ggplot(PalmDataRestricted, aes( x = wave, y = Size, fill = wave)) +
  geom_boxplot(alpha= 0.5) + theme(panel.background = element_blank(),
                                   axis.line = element_line(colour = "black"),
                                   legend.position="none") +
      scale_fill_brewer(palette="Dark2") +
        ylab("Plot size (ha)") +
         xlab("Wave") +
          ggtitle("Input 1") +
            scale_y_continuous(label=comma, limits=c(0,7))

# for employed labour

plot_box_labour <- ggplot(PalmDataRestricted, aes( x = wave, y = Labour, fill = wave)) +
  geom_boxplot(alpha= 0.5) + theme(panel.background = element_blank(),
                                   axis.line = element_line(colour = "black"),
                                   legend.position="none") +
      scale_fill_brewer(palette="Dark2") +
        ylab("Labour (manhours)") +
          xlab("Wave") +
            ggtitle("Input 2") +
              scale_y_continuous(label=comma, limits=c(0,6000))

# for agrochemical use 

plot_box_agrochems <- ggplot(PalmDataRestricted, aes( x = wave, y = Inputs, fill = wave)) +
  geom_boxplot(alpha= 0.5) + theme(panel.background = element_blank(),
                                   axis.line = element_line(colour = "black"),
                                   legend.position="none") +
        scale_fill_brewer(palette="Dark2") +
          ylab("Agrochemical use (kg)") +
            xlab("Wave") +
              ggtitle("Input 3") +
                scale_y_continuous(label=comma, limits=c(0,4000))

# arrange of the four plots in one

descriptive_stats_boxplots <- ggarrange(plot_box_production,
      plot_box_size, plot_box_labour, plot_box_agrochems)

# save the plots in high resolution

ggsave("descriptive_stats_boxplots.png")

# remove the "subplots" to maintain the environment clean

rm(plot_prod_input1)
rm(plot_prod_input2)
rm(plot_prod_input3)

rm(plot_box_production)
rm(plot_box_size)
rm(plot_box_labour)
rm(plot_box_agrochems)



