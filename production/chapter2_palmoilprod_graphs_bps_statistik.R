
# setwd("//ug-uyst-ba-cifs.student.uni-goettingen.de/home/users/j.latzko/Desktop/Tesis/production")

# PalmOilProdIndonesia <- read.csv("FAOSTAT_data_PalmOilIndonesia.csv")

PalmOilProdYear <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

PalmOilProdCrude <- c(8458.7, 8797.9, 9197.7, 10010.73, 10205.4, 10527.8, 11575.6, 13191.2, 13999.8)

PalmOilProdKernel <- c(1894.8, 1759.6, 1839.5, 2002.15, 2041.1, 2105.6, 2315.1, 2638.2, 2800)

PalmOilProdArea <- c(NA, 9102.3, 10133.32, 10465.02, 10754.8, 11260.3, 11201.5, 12383.1, 14327.1)

PalmOilProdBunch <- c(NA, NA, 26015.52, 27782, 29278, 31070, 31731, 34940.3, 40567.2)



# kernel production correction

PalmOilProdKernel <- c(PalmOilProdIndonesia$EstateKernelProduction +
                         PalmOilProdIndonesia$EstateKernelProduction)

PalmOilProdIndonesia$KernelProduction <- PalmOilProdKernel


PalmOilProdIndonesia <- data.frame(PalmOilProdYear, PalmOilProdBunch, PalmOilProdCrude, PalmOilProdKernel, PalmOilProdArea)


names(PalmOilProdIndonesia) <- c("Year", "FruitBunches", "CrudeOil",
                                 "Kernel", "Area")

summary(PalmOilProdIndonesia)


library(ggplot2)
require(scales)
library(gridExtra)

grid.arrange(p1, p2, p3)

setwd("C:/Users/jakob/ownCloud/Master_Thesis/production(GWDG)")

write.csv(PalmOilProdIndonesia, file ="BPS_PalmOil_prod_Indonesia.csv")

# graph for palm oil fruit production

barplot_palmoil <- melt(PalmOilProdIndonesia[,c(1,4,2)], id = "Year" )

p1 <- ggplot( data = barplot_palmoil, aes(x= Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label=round(value/1000, 2)), vjust= -0.5, color = "black", position = position_dodge(0.9),
            size = 3.5) +
  scale_fill_brewer(palette= "Paired", name = "Production type") +
  scale_x_continuous("Year", labels = Yearlabel, breaks = Yearlabel) +
  labs( title = "Palm oil production in Indonesia", colour = "Production type") +
  ylab("Production in 1,000,000 tons") +
  theme_minimal()


# graph for palm oil kernel production

barplot_palmoil <- melt(PalmOilProdIndonesia[,c(1,7,5)], id = "Year" )

p2 <- ggplot( data = barplot_palmoil, aes(x= Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label=value), vjust= 1.6, color = "black", position = position_dodge(0.9),
            size = 3.5) +
  scale_fill_brewer(palette= "Paired", name = "Production type") +
  scale_x_continuous("Year", labels = Yearlabel, breaks = Yearlabel) +
  labs( title = "Palm oil kernel production in Indonesia", colour = "Production type") +
  ylab("Production in 1000 tons") +
  theme_minimal()

# graph for palm oil production area

barplot_palmoil <- melt(PalmOilProdIndonesia[,c(1,10,8)], id = "Year" )

p3 <- ggplot( data = barplot_palmoil, aes(x= Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label=value), vjust= 1.6, color = "black", position = position_dodge(0.9),
            size = 3.5) +
  scale_fill_brewer(palette= "Paired", name = "Production type") +
  scale_x_continuous("Year", labels = Yearlabel, breaks = Yearlabel) +
  labs( title = "Palm oil production area in Indonesia", colour = "Production type") +
  ylab("Cultivation area in 1000 ha") +
  theme_minimal()

grid.arrange(p1, p2, p3)


