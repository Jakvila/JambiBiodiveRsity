# exc 2


if(!is.integer(x)){
  x = is.integer(x)
  warning("X is not an integer, converting it to integer")
}


# b 
functionXtoB = function(x,y){
  if(is.integer())
    if(x %in% y ){
      
    }
}

# alternatively

function2 <- funcion(x,y){
  is.element(y,x)
}


# d)

x <- c(1,2,3,5,2, 6, 9, 7)
unique(x)


elementcount <- function(vec, int){
  x = table(vec)
  x[names(x == int)]
}
  elementcount(x, 4)
  #never overwrite existing R functions 
  
  
  
  ## OLS estimator
  
# cars 
  
  plot(cars$speed, cars$dist)

  linearModel <- lm(dist ~ speed, data= cars)  
linearModel$coefficients  
  

myOLS <- function(x, y){
  ((t(x)%*%x)^-1)%*%t(x)%*%y
  
}
  
intercepto <- c(rep(1, length.out = length(x)))
                

                  
        set.seed()          
  y = rnorm(100)
  x = rnorm(100)
                 
  plot(x, y, type="b", main="I like drawing random lines", sub = "I really do, dude",
       ylab= "Y axis label", xlab = "X axis label", col = "magenta",
       pch = 11)
  
cars <- cars

plot(cars$speed, cars$dist)                  

plot(cars$speed, cars$dist, xlab="speed", ylab="distance", cex = 0.5)
axis(1, at = seq(1, 25, by = 1), las=2, cex.axis= 0.5)
axis(2, at = seq(1, 25, by = 1), las=2, cex.axis= 0.5)


library(ggplot2)

ggplot(data= cars, aes(speed, dist)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Data analysis") + xlab("X-Axis titles") + ylab("Y-axis title") +
  theme_minimal() + scale_x_continuous(breaks = seq(0, 25, 1)) +
  scale_y_continuous(breaks = seq(0, 125, 5))


diamonds <- diamonds

ggplot(data = diamonds, aes(carat)) +geom_histogram()

ggplot(data = diamonds, aes(carat)) +geom_histogram(binwidth = 0.01)

ggplot(data = diamonds, aes(carat, price)) + geom_point()


# World Bank map

  library(WDI)
WDIsearch("GNI")

WDIdata <- WDI(country = "all", indicator = "NY.GNP.MKTP.PC.CD", start = 1990, end= 2019)

WDIdata[WDIdata$iso2c == "DE",]

library(countrycode)

countrycode("ID", origin = "iso2c", destination = "iso3c")

setwd("C:/Users/jakob/Downloads")
MeatConsumption <- read.csv("MeatConsumptionData.csv")

MeatConsumption$iso2c <- countrycode(MeatConsumption$Country, 
                                     origin = "country.name",
                                     destination = "iso2c")

FullData <-  merge(WDIdata, MeatConsumption, by.x = c("iso2c", "year"),
                                            by.y = c("iso2c", "Year.Code"),
                                            all= F)


ggplot( data = FullData[FullData$year == 2010,],
        aes(y = NY.GNP.MKTP.PC.CD, x = Value)) +
        geom_point()


FullData$Label <- ifelse(FullData$Value > 90, FullData$country, NA)

ggplot(data = FullData[FullData$year == 2010,],
       aes(x = NY.GNP.MKTP.PC.CD, y = Value, colour = GroupCode)) +
    geom_text(data = FullData[FullData$year == 2010],
              aes(label = Label, x = NY.GNP.MKTP.PC.CD + 5000, y = Value)) +
        ylab("kg meat consumption p.c. per year") + xlab("GNI p.c. per year") +
        theme_minimal()


library(ggmap)

Europe = getData("GADM", country = "Europe", level = 2)
Jambi = Indo[Indo$NAME_1 == "Jambi",]



View(matrix_wide_2018[155,])



# Efficiency 11th October
library(micEcon)
data("appleProdFr86", package = "micEcon")


AppleData <- appleProdFr86
rm(appleProdFr86)



AppleData$qCap <- AppleData$vCap / AppleData$pCap
AppleData$qLab <- AppleData$vLab / AppleData$pLab
AppleData$qMat <- AppleData$vMat / AppleData$pMat

# a)

par(mfrow = c(1,3))
plot(AppleData$qCap, AppleData$qApples)
plot(AppleData$qLab, AppleData$qApples)
plot(AppleData$qMat, AppleData$qApples)


# b)


AppleData$apCap <- AppleData$qOut / AppleData$qCap
AppleData$apLab <- AppleData$qOut / AppleData$qLab
AppleData$apMat <- AppleData$qOut / AppleData$qMat

# c)

for ( i in c("apCap", "apLab", "apMat")){
  hist(AppleData[, i], main = paste(i), xlab = paste(i), breaks = 20)
}

# d)

ln_Q <- log(15) + ((4/5)*log(5))*((1/5))


# exc 2
# a)
 
prodLin <- lm(qOut ~ qCap + qLab + qMat, data = AppleData)
summary(prodLin)

# b)

prodCD <- lm (log(qOut) ~ log(qCap) + log(qLab) + log(qMat), data = AppleData)
summary(prodCD)

# c)



# Exc 3

# a)
library(sfa)
prodCDSfa <- sfa( log(qOut) ~ log(qCap) + log(qLab) + log(qMat), data = AppleData)
summary(prodCDSfa)

# sfa: split measurement error and account a part of it to inefficiency. Splitting inefficiency and random error effect.








