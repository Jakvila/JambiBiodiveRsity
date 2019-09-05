library(data.table)

data = data.table(read.csv("~/GitHub/JambiBiodiveRsity/VegetationDataComplete.csv"))
data[, Abundance := as.numeric(as.character(Abundance))]
summary(table)
tapply(data$SpeciesName, data$wave, length)
tapply(data$Abundance, data$wave, sum, na.rm=T)


table(data)

sapply(unique(data[,hid]), function(x){
  subs = subset(data[hid == x & wave == '2015', ])
  unident = subs[grep("Unide", SpeciesName),]
  n =nrow(unident)
  rename = unident[1:n/2,]
  
  rename[, SpeciesName := paste("Unident_", seq(1, 5))]
  })
