library(data.table)
library("vegan")
source('~/ownCloud/Efficiency/R/fixHHCode.R')


VegetationData = data.table(read.csv("Data/Vegetation.csv"))

VegetationData[, HHCode := fixHHcode(HHCode)]

VegetationData = VegetationData[, .(wave, HHCode, hid, pid, SpeciesName, Abundance, Invasive)]

VegetationData[, Invasive := ifelse(Invasive == 1, "Invasive", "Native")]
VegetationData[, Abundance := as.numeric(as.character(Abundance))]
VegetationData[, c("HHCode", "hid", "pid", "SpeciesName") := lapply(.SD, as.character), .SDcols =  c("HHCode", "hid", "pid", "SpeciesName")]
  Duplicated = VegetationData[duplicated(VegetationData[, .(HHCode, wave, SpeciesName)])==T,]

  duplicatedHHCodes = unique(Duplicated[, HHCode])
  
  # Wrong plot of 531.03. Weird, in 2012 it was pid 3 of this household. CO7 data says plot ID 1 is C01 plot, and Marys file says 1 as well
  # We're going with plot ID 1 in this round (Can we match it with plot ID 3 from 2012?) areas are checked
  # We do this for all cases where the wrong plot has been mistakenly surveyed.

  VegetationData = VegetationData[!substr(as.character(hid), 5,5) == "1",]
  # Check again for problems:
  Duplicated = VegetationData[duplicated(VegetationData[, .(HHCode, wave, SpeciesName)])==T,]
  duplicatedHHCodes = unique(Duplicated[, HHCode])
  
  # Still a lot in there. I found that there is some plot mistakes in the data (check for instance 421.01 is linked to both 510 and 516). 
  # will need to mathc back with C01Plot Data
  load("Data/C01PlotTable.RDA")
  Veg2012= VegetationData[wave ==2012,]
  Veg2012 = Veg2012[C01PlotTable[, -c("pid")], on = c("HHCode")]
  Veg2012[, hid := i.hid]
  Veg2012=Veg2012[, -"i.hid"]
  VegetationData = VegetationData[wave == 2015,]
  VegetationData = rbind(VegetationData, Veg2012)
  VegetationData = VegetationData[C01PlotTable[, -"pid"], on = c("HHCode", "hid")]
  Duplicated = VegetationData[duplicated(VegetationData[, .(HHCode, wave, SpeciesName)])==T,]
  duplicatedHHCodes = unique(Duplicated[, HHCode])
  
  # Turns out some species have been counted twice Two options: Either assume that they have been recounted or that they are counted twice
  # It is more likely that they have been counted twice, just didnt sum them up in the field
  dropData = VegetationData[HHCode %in% duplicatedHHCodes & SpeciesName %in% Duplicated[, SpeciesName],]
  
  saveData = data.table(aggregate(data = dropData, Abundance ~ wave + HHCode + hid + SpeciesName + Invasive , max))

  VegetationData = VegetationData[!dropData, on = names(VegetationData)]
  saveData[, pid := NA]
  VegetationData = rbind(VegetationData, saveData[, mget(names(VegetationData))])
  Duplicated = VegetationData[duplicated(VegetationData[, .(HHCode, wave, SpeciesName)])==T,]
  duplicatedHHCodes = unique(Duplicated[, HHCode])
  
  # Finally solved all problems, continue with compilation of measures
  # Ok
  setkey(VegetationData, wave, HHCode, hid, SpeciesName)
  
  TotalAbundance = data.table(aggregate(data = VegetationData,  Abundance ~ wave + HHCode, sum, na.rm = T))
  
  InvasiveAbundance = VegetationData[Invasive == "Invasive", list(InvasiveAbundance = sum(Abundance), num = .N), by = .(HHCode, wave)]
  
  DiversityData = dcast(data = VegetationData,  wave + HHCode ~ SpeciesName, value.var = "Abundance")
  DiversityData[is.na(DiversityData)] = 0
  simp <- diversity(DiversityData[, -(1:2)], "simpson")
  
  SimpsonDiversity = data.table(wave = DiversityData[, wave], HHCode = DiversityData[, HHCode], Simpson = simp)
  
  AbundanceInvasive = merge(TotalAbundance, InvasiveAbundance, all = T)
  AbundanceInvasive[, InvasiveAbundance := ifelse(is.na(InvasiveAbundance),0, InvasiveAbundance)]
  AbundanceInvasive[, InvasiveAbundance := ifelse(is.na(num),0, num)]
  DiversityIndicators = merge(AbundanceInvasive, SimpsonDiversity, all = T)
  
  DiversityIndicators = merge(DiversityIndicators, C01PlotTable, all.x = T)
  
  DiversityIndicators[, InvasiveShare := InvasiveAbundance / Abundance]
  DiversityIndicators = DiversityIndicators[, .(wave, hid, HHCode, pid, Abundance, InvasiveAbundance, num,  InvasiveShare, Simpson)]
  setkey(DiversityIndicators, wave, hid, HHCode)
  
  DiversityIndicators[, wave := as.character(wave)]
  save(object = DiversityIndicators, file = "Data/DiversityIndicators.RDA")
  # augment to commodity ID
  load("Data/ProductionData.RDA")
  HidCidTable = unique(ProductionData[, .(wave, hid, pid, cid)])
   merge(DiversityIndicators, HidCidTable, by = c("wave", "hid", "pid"), all.x = T)
  write.csv(DiversityIndicators, "Data/DiversityIndicators.csv", row.names = F)
  