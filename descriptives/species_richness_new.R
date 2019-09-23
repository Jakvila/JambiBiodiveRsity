##############################################################################################
# continuing the biodiversity-R script #######################################################
# We start with 2015 #########################################################################
##############################################################################################

library(data.table)
library(plyr)
library("vegan")
source('C:/Users/jakob/OneDrive/Desktop/Universidad/R_assignment/fixHHCode.R')
setwd("C:/Users/jakob/MA/Neuer Ordner/JambiBiodiveRsity/JambiBiodiveRsity/descriptives")

VegetationData = data.table(read.csv("VegetationUnidentCorrected.csv"))

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
load("C01PlotTable.RDA")
Veg2012= VegetationData[wave ==2012,]
Veg2012 = Veg2012[C01PlotTable[, -c("pid")], on = c("HHCode")]
Veg2012[, hid := i.hid]
Veg2012=Veg2012[, -"i.hid"]
VegetationData = VegetationData[wave == 2015,]

library(plyr)
VegetationData = rbind.fill(VegetationData, Veg2012)

library(dplyr)

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
VegetationData2015 = VegetationUnidentCorrected[wave == 2015,]
setkey(VegetationData2015, wave, HHCode, hid, SpeciesName)

TotalAbundance = data.table(aggregate(data = VegetationData2015,  Abundance ~ wave + HHCode, sum, na.rm = T))

InvasiveAbundance = VegetationData2015[Invasive == "Invasive", list(InvasiveAbundance = sum(Abundance), num = .N), by = .(HHCode, wave)]

VegetationData2015$hid <- as.numeric(as.character(VegetationData2015$hid))
matrix_wide_2015 = dcast(data = VegetationData2015,  wave + hid ~ SpeciesName, value.var = "Abundance")
matrix_wide_2015[is.na(matrix_wide_2015)] = 0


simpson2015 <- diversity(matrix_wide_2015[, -(1:2)], "simpson")

VegetationData2015$hid <- as.numeric(as.character(VegetationData2015$hid))
shannon2015 <- diversity(matrix_wide_2015)

shannon2015 <- data.frame(shannon2015, DiversityData$HHCode)
names(shannon2015) <- c("shannon", "HHCode")

# connect the right regencies with the hids/hhcodes

library(openxlsx)

household_list <- read.xlsx("farmer_plot_table_edited.xlsx")

# manually edited the excel sheet, deleted the irrelevant
# columns, added the plot ID and crop name to the replacement plots

colnames(household_list) <- c('regency','village','hid','plotID','crop')


################################################################################################
## SPECIES RICHNESS INDICES FOR 2018

library(vegan)
library(data.table)
library(reshape2)
library(dplyr)

VegetationDataComplete <- read.csv("VegetationUnidentCorrected.csv", stringsAsFactors = FALSE)
matrix_wide_2018 <- VegetationDataComplete[which(VegetationDataComplete$wave == 2018),]

  matrix_wide_2018$hid <- as.character(matrix_wide_2018$hid)
  matrix_wide_2018$hid <- as.numeric(matrix_wide_2018$hid)

  matrix_wide_2018$Abundance <- as.character(matrix_wide_2018$Abundance)
  matrix_wide_2018$Abundance <- as.numeric(matrix_wide_2018$Abundance)

# VegetationData2012$HHCode <- as.numeric(VegetationData2012$Abundance)
  matrix_wide_2018 = data.table(matrix_wide_2018)
  matrix_wide_2018 = matrix_wide_2018[, .(hid, SpeciesName, Abundance)]

# Check for duplicate entries
  Duplicated2018 = matrix_wide_2018[duplicated(matrix_wide_2018 [, .(hid, SpeciesName)])==T,]
  matrix_wide_2018 <- distinct(matrix_wide_2018, hid, SpeciesName, .keep_all = T)

#  matrix_wide_2018 <- matrix_wide_2018[!c(127, 453, 735, 2099),]
## in rows 127, 453, 735, 2099
  Duplicated2018 = matrix_wide_2018[duplicated(matrix_wide_2018 [, .(hid, SpeciesName)])==T,]
  
  matrix_wide_2018 <- dcast(data = matrix_wide_2018,  hid ~ SpeciesName, value.var = "Abundance")
  matrix_wide_2018[is.na(matrix_wide_2018)] <- 0
  
  write.csv(matrix_wide_2018, "matrix_wide2018.csv")
  
# make species richness indicators

simpson2018 <- diversity(matrix_wide_2018, index="simpson")
shannon2018 <- diversity(matrix_wide_2018)

shannon2018 <- data.frame(shannon2018, matrix_wide_2018$hid)
names(shannon2018) <- c("shannon", "hid")

simpson2018 <- data.frame(simpson2018, matrix_wide_2018$hid)
names(simpson2018) <- c("simpson", "hid")

# the same for 2012 ##################################################################################


  library(vegan)
  library(data.table)
  library(reshape2)
  library(stringi)

VegetationData2012 <- VegetationUnidentCorrected[which(VegetationUnidentCorrected$wave == 2012),]
rownames(VegetationData2012) <- NULL

# VegetationData2012 <- VegetationData2012[-c(1136, 2362),] question marks as answers... *rolleye*

HHCode <- C01PlotTable


HHCode$HHCode <- stri_replace_last(HHCode$HHCode, fixed = ".", ",")

VegetationData2012 <- merge(VegetationData2012, HHCode, by="HHCode") # ~ 175 observations lost due to the merge...
VegetationData2012$hid <- as.numeric(as.character(VegetationData2012$hid))


VegetationData2012$hid.x <- NULL
VegetationData2012$hid <- VegetationData2012$hid.y
VegetationData2012$hid <- as.numeric(as.character(VegetationData2012$hid))
VegetationData2012$hid.y <- NULL


# write.csv(VegetationData2012, file="Vegetation2012_hid.csv")
VegetationData2012$HHCode <- NULL
VegetationData2012$wave.y <- NULL
VegetationData2012$pid.y<- NULL
VegetationData2012$pid.x<- NULL



# VegetationData2012$HHCode <- as.numeric(as.factor(VegetationData2012$HHCode))
# VegetationData2012$Abundance <- as.numeric(as.character(VegetationData2012$Abundance))


 VegetationData2012 = data.table(VegetationData2012)
 VegetationData2012 = VegetationData2012[, .(hid, SpeciesName, Abundance)]
 VegetationData2012$Abundance[is.na(VegetationData2012$Abundance)] <- 0
# Check for duplicate entries
Duplicated2012 = VegetationData2012[duplicated(VegetationData2012 [, .(hid, SpeciesName)])==T,]
# no duplicates for 2012

    VegetationData2012$Abundance <- as.character(VegetationData2012$Abundance)
    VegetationData2012$Abundance <- as.numeric(VegetationData2012$Abundance)

 matrix_wide_2012 <- dcast(data = VegetationData2012,  hid ~ SpeciesName, value.var = "Abundance")
 matrix_wide_2012[is.na(matrix_wide_2012)] <- 0
 
 # These steps are necessary in order to make the input data numeric, a requirement of the vegan package,
 # while maintaining the data structure.

 
 shannon2012 <- diversity(matrix_wide_2012)
 shannon2012 <- data.frame(shannon2012, matrix_wide_2012$hid)
 
 
 names(shannon2012) <- c("shannon", "hid")

############ bring indices to the household IDs

 
 mean(shannon2012$shannon)
 mean(shannon2015$shannon)
 mean(shannon2018$shannon)
 
 
 write.csv(shannon2012, file = "shannon2012.csv")
 write.csv(shannon2015, file = "shannon2015.csv")
 write.csv(shannon2018, file = "shannon2018.csv")
 setwd("C:/Users/jakob/OneDrive/Desktop/Universidad/R_assignment")
 



