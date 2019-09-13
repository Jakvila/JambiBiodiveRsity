library(data.table)

setwd("C:/Users/jakob/MA/Neuer Ordner/JambiBiodiveRsity")


data = data.table(read.csv("C:/Users/jakob/MA/Neuer Ordner/JambiBiodiveRsity/JambiBiodiveRsity/VegetationDataComplete.csv"))
data[, Abundance := as.numeric(as.character(Abundance))]
summary(data)
tapply(data$SpeciesName, data$wave, length)
tapply(data$Abundance, data$wave, sum, na.rm=T)


table(data)

hids <- unique(data[,hid ])

x <- 60
sapply(unique(data[,hid]),) 
       
       
cleanUnknownToKnown = function(x){
  
  subs = subset(data[hid == x & wave == '2015', ])
  unident = subs[grep("Unide", SpeciesName),]
  n =nrow(unident)
  if(((n %/% 2) == 0)){
  rename = unident[1:n/2,]
  
  rename[, SpeciesName := paste("Unident_", seq(1, nrow(unident[1:n/2,])))]
  
  }else{
    toRename = sample(c(0,1), 1, prob = c(0.48, 0.52))
    rename = unident[1:(n %/% 2 +toRename),]
    originalNames = rename[, SpeciesName]
    rename[, SpeciesName := paste0("Unident_", seq(1, nrow(unident[1:(n %/% 2 +toRename),])))]
  }
  
  rbind(subs[!SpeciesName %in% originalNames, ], rename)
  
  }


test = rbindlist(lapply(unique(data[wave =="2015",hid]), cleanUnknownToKnown))

test2 = data

rbindlist(lapply(unique(data[wave =="2015",hid]), cleanUnknownToKnown))

length(table(data[wave=="2015",SpeciesName]))



 