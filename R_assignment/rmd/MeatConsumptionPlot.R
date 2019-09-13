# 7 Data Visualization


# Base plot 


x <- rnorm(100)
y <- rnorm(100)

plot(x,y, main = "Two random variables", 
     cex = 1,type = "p", xlab = "X-axis", 
     ylab ="Y-axis", col = "red", sub = "some plot", pch=87, xlim = c(-15, 15))
for(i in 0:100){
  points(i-13,0, cex = 3, type="p", pch = i, col= "blue")
}
plot(x, type = "line")
?abline
?plot

plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")


# b) c)
plot(cars[,"speed"], cars[, "dist"], lab = c(20,10,0), cex.axis=.6)


MeatConsumption = read.csv("MeatConsumptionData.csv")
#install.packaes("ggplot2")
#install.packages("WDI")
library(ggplot2)
library(WDI)
?`WDI-package`

WDIsearch("GNI")
"NY.GNP.PCAP.PP.CD" 

GNIData = WDI(country = "all", indicator = "NY.GNP.PCAP.PP.CD",
              start = 2000, end = 2019, extra = FALSE, cache = NULL)

str(GNIData)
head(GNIData)
MeatConsumption = read.csv("MeatConsumptionData.csv")
str(MeatConsumption)

head(MeatConsumption)
head(GNIData)
install.packages("countrycode")

library(countrycode)
?countrycode

MeatConsumption$iso2c = countrycode(MeatConsumption$wbCode, 
                                    origin = "iso3c", 
                                    destination = "iso2c")

head(MeatConsumption)

head(GNIData)
names(GNIData) = c("iso2c", "country", "NY.GNP.PCAP.PP.CD", "Year.Code")     
# Merge the two dataset 
MeatGNIData = merge(MeatConsumption, GNIData, 
                    by.x = c("iso2c", "Year.Code"),
                    by.y = c("iso2c", "year"),
                    # keep only observations for which we have both variables
                    all = FALSE)

# Alternatively we can reneame the variables before merging the data.frames
names(GNIData) = c("iso2c", "country", "NY.GNP.PCAP.PP.CD", "Year.Code") 
MeatGNIData = merge(MeatConsumption, GNIData, 
                    by = c("iso2c", "Year.Code"),
                    # keep only observations for which we have both variables
                    all = FALSE)

str(MeatGNIData)
MeatGNIData$Labels <- ifelse(MeatGNIData$NY.GNP.PCAP.PP.CD > 20000 & 
                               MeatGNIData$Value > 72, MeatGNIData$country, NA)
  
  
ggplot(data = MeatGNIData[MeatGNIData$Year.Code %in% c(2010), ], 
       aes(x = NY.GNP.PCAP.PP.CD, y = Value)) +
  scale_color_brewer(palette ="Set1") +
  stat_smooth(method="lm", formula = y  ~ x + I(x^2), colour = "black") +
  geom_point(aes(colour = as.factor(GroupCode))) +
  geom_text(aes(label = Labels, x = (NY.GNP.PCAP.PP.CD + 5000), y = Value)) +
  xlab("GNI per capita per year (PPP)")+
  ylab("Meat consumption per capita per year")+
  theme_minimal()
+
  facet_wrap(~Year.Code)


