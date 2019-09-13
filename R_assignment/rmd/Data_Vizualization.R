#7 Data Visualization

#Base plot

x <- rnorm(100)
y <- rnorm(100)
plot(x,y,main = "Two Random Variables", sub="What a chaos", 
     cex=0.7,xlab="X-Axis", ylab="Y-Axis",
     col = "dark violet" , pch=11)



for(i in 0:10){
  points(i-13,0,cex = 1, type="p",pch=i, col="blue")
}

plot(x, type = "line", col = "dark red")

?plot

# Exercise 1 plot
data(cars)
summary(cars)
p<-plot(cars, main = "Beautiful Cars", col = "dark red",pch=19, type="o",
     xlab = "Distance", ylab = "Speed", cex=1.2, cex.main=2,
     cex.axis = 0.5, cex.lab=1.2,sub="I'm in love with my car", cex.sub = 1.5)
      
#b #c lab argument

plot(cars[,"speed"], cars[,"dist"], lab = c(25,20,0),
      cex.axis=0.5, pch=19, main="Using the Lab argument",xlab = "Distance",
      ylab = "Speed", cex.main=1.5)

#d   las argument  to change annotation of axis
plot(cars[,"speed"], cars[,"dist"], lab = c(25,20,0),
     cex.axis=0.5, pch=19, main="Using the Lab argument",xlab = "Distance",
     ylab = "Speed", cex.main=1.5, las=1)

#e adding new observations to plot, using red to identify
plot(cars[,"speed"], cars[,"dist"], lab = c(25,20,0),
     cex.axis=0.5, pch=19, main="Using the Lab argument",xlab = "Distance",
     ylab = "Speed", cex.main=1.5, las=1)
points=
  
  
  
  
  
  
  
  
  
  
  
install.packages("WDI") #world bank database
library(ggplot2)
library(WDI)
MeatConsumptionData = read.csv("MeatConsumptionData.csv")

summary("MeatConsumptionData")


WDIsearch("GNI")
"NY.GNP.PCAP.PP.CD"
GNIData = WDI(country = "all", indicator = "NY.GNP.PCAP.PP.CD", start= 2000, end = 2019, extra=FALSE,cache=NULL)

str(GNIData)
head(GNIData)
head(MeatConsumptionData)
#every UN organisation has a different naming system for countries

install.packages("countrycode")
library(countrycode)
?countrycode
countrycode("AFG", origin = "iso3c", destination = "iso2c")

MeatConsumptionData$iso2c = countrycode(MeatConsumptionData$wbCode, 
                                    origin = "iso3c",
                                    destination = "iso2c")
head(MeatConsumptionData)


# Merge the 2 datasets
MeatGNIData = merge(MeatConsumptionData, GNIData, by.x = c("iso2c", "Year.Code"),
                                              by.y = c("iso2c", "year"),
                                              all = FALSE) #keep only observations for which we have both variables
head(MeatGNIData)
#Alternatively we cane rename variables before merging the data.frames

library(ggplot2)

on$Labels <- ifelse(MeatGNIData[MeatGNIData$NY.GNP.PCAP.PP.CD > 20000 
                      | MeatGNIData$Value > 72, MeatGNIData$country, NA])


ggplot(data = MeatGNIData[MeatGNIData$Year.Code %in% c(2000,2005, 2010,2013),],
       aes(x = NY.GNP.PCAP.PP.CD+5000, y = Value))+
      geom_point(aes(colour = as.factor(GroupCode)))+
      #geom_text(aes(label = country))+
      scale_color_brewer(palette="Set5")+
      xlab("GNI per capita and year")+
      ylab("Meat consumption per capita per year") +
      ggtitle("Relationship between GNI and Meat consumption in 2010")+
      facet_wrap(~Year.Code)+
      theme_minimal()+
      stat_smooth(method="lm",formula =y~ x+I(x^2),colour = "black")
      #annotate("text", aes(label = country, x = NY.GNP.PCAP.PP.CD, y = Value), size = 4)
      


#Exercise2
diamonds

#basic histogram
ggplot(data=diamonds, aes(x=diamonds$carat)) + geom_histogram()
#change width of bins
ggplot(data=diamonds, aes(x=diamonds$carat)) + geom_histogram(binwidth=0.01)
#change colors
p<-ggplot(data=diamonds, aes(x=diamonds$carat)) + geom_histogram(binwidth=0.1, color="black", fill="violet")
p
#scatterplot
ggplot(data=diamonds, aes(x=carat, y=price, color=clarity)) + geom_point(pch=19)
                        +geom_smooth(method=lm,se=FALSE)

#Show carat vs. cut, make a jitter, a violin and a boxplot.
ggplot(data=diamonds, aes(x=carat, y=cut, color=clarity))+geom_violin(pch=19,cex=5)

ggplot(data=diamonds, aes(x=carat, y=cut, color=clarity))+geom_jitter(pch=19,cex=2)

ggplot(data=diamonds, aes(x=carat, y=cut, color=clarity))+geom_boxplot(pch=19,cex=2)

ggsave("diamonds.pdf")
ggsave("diamonds.png")
