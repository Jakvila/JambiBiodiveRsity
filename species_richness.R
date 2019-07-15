##############################################################################################
# continuing the biodiversity-R script


setwd("C:/Users/jakob/OneDrive/Desktop/Universidad/R_assignment")

matrix_wide_2018 <- read.csv("matrix_wide_2018.csv")

library(vegan)
library(data.table)
library(reshape2)

matrix_wide_2018$hid <- as.numeric(matrix_wide_2018$hid)
matrix_wide_2018$Abundance <- as.numeric(matrix_wide_2018$Abundance)

VegetationData2012$HHCode <- as.numeric(VegetationData2012$Abundance)
matrix_wide_2018 = data.table(matrix_wide_2018)
matrix_wide_2018 = matrix_wide_2018[, .(hid, SpeciesName, Abundance)]

# Check for duplicate entries
Duplicated = matrix_wide_2018[duplicated(matrix_wide_2018 [, .(hid, SpeciesName)])==T,]

# only 3 found

matrix_wide_2018 <- dcast(data = matrix_wide_2018,  hid ~ SpeciesName, value.var = "Abundance")

# make species richness indicators

simpson2018 <- diversity(matrix_wide_2018, index="simpson")
shannon2018 <- diversity(matrix_wide_2018)

shannon2018 <- data.frame(shannon2018, matrix_wide_2018$hid)
names(shannon2018) <- c("shannon", "hid")

# the same for 2012 ##################################################################################

VegetationDataComplete <- read.csv("VegetationDataComplete.csv", stringsAsFactors = FALSE)
hhcodes <- read.csv("Vegetation_corrected.csv", header = TRUE, sep=";")
hhcodes <- hhcodes[which(VegetationDataComplete$wave == 2015),]
hhcodes <- hhcodes[-c(1,4:9)]

hhcodes <- unique(hhcodes) 
names(hhcodes) <- c("hid", "HHCode")


  library(vegan)
  library(data.table)
  library(reshape2)

VegetationDataComplete$X <- NULL

VegetationData2012 <- VegetationDataComplete[which(VegetationDataComplete$wave == 2012),]
rownames(VegetationData2012) <- NULL
VegetationData2012 <- VegetationData2012[-c(1136, 2362),] # question marks as answers... *rolleye*


VegetationData2012 <- merge(VegetationData2012, hhcodes, by="HHCode") # ~ 600 observations lost due to the merge...
VegetationData2012$hid.x <- NULL
VegetationData2012$hid <- VegetationData2012$hid.y
VegetationData2012$hid.y <- NULL
write.csv(VegetationData2012, file="Vegetation2012_with-hid.csv")
VegetationData2012$HHCode <- NULL




# VegetationData2012$HHCode <- as.numeric(as.factor(VegetationData2012$HHCode))
# VegetationData2012$Abundance <- as.numeric(as.character(VegetationData2012$Abundance))


 VegetationData2012 = data.table(VegetationData2012)
 VegetationData2012 = VegetationData2012[, .(hid, SpeciesName, Abundance)]

# Check for duplicate entries
Duplicated2012 = VegetationData2012[duplicated(VegetationData2012 [, .(hid, SpeciesName)])==T,]
# no duplicates for 2012

    VegetationData2012$Abundance <- as.character(VegetationData2012$Abundance)
    VegetationData2012$Abundance <- as.numeric(VegetationData2012$Abundance)

 VegetationData2012 <- dcast(data = VegetationData2012,  hid ~ SpeciesName, value.var = "Abundance")
 VegetationData2012[is.na(VegetationData2012)] <- 0
 
 # These steps are necessary in order to make the input data numeric, a requirement of the vegan package,
 # while maintaining the data structure.
 
 VegetationData2012$hid <- as.character(VegetationData2012$hid)
 VegetationData2012$hid <- as.numeric(VegetationData2012$hid)

 shannon2012 <- diversity(VegetationData2012)
 shannon2012 <- data.frame(shannon2012, VegetationData2012$hid)
 
 names(shannon2012) <- c("shannon", "hid")

 
### BERNHARD'S CODE
 
# setkey(VegetationDataComplete, wave, HHCode, hid, SpeciesName)
# TotalAbundance = data.table(aggregate(data = VegetationDataComplete,  Abundance ~ wave + HHCode, sum, na.rm = T))
# InvasiveAbundance = VegetationDataComplete[Invasive == "Invasive", list(InvasiveAbundance = sum(Abundance), num = .N), by = .(HHCode, wave)]
# DiversityData = dcast(data = VegetationDataComplete,  wave + HHCode ~ SpeciesName, value.var = "Abundance")
# DiversityData[is.na(DiversityData)] = 0
# simp <- diversity(DiversityData[, -(1:2)], "simpson")
# SimpsonDiversity = data.table(wave = DiversityData[, wave], HHCode = DiversityData[, HHCode], Simpson = simp)
# simpson <- data.frame(SimpsonDiversity, DiversityData$hid)

# same for 2015 ###################################################################################################

VegetationData2015 <- VegetationDataComplete[which(VegetationDataComplete$wave == 2015),]

  VegetationData2015$hid <- as.character(VegetationData2015$hid)
    VegetationData2015$hid <- as.numeric(VegetationData2015$hid)
  
  VegetationData2015$Abundance <- as.character(VegetationData2015$Abundance)
    VegetationData2015$Abundance <- as.numeric(VegetationData2015$Abundance)
    
# ESTOY RE PERDIDO, NO SÉ NI QUE HACER

VegetationData2015 = data.table(VegetationData2015)
VegetationData2015 = VegetationData2015[, .(hid, SpeciesName, Abundance)]

  Duplicated2015 <- VegetationData2015[duplicated(VegetationData2015 [, .(hid, SpeciesName)])==T,]
# 64 observations of 7 different households and of many NAs are duplicated...
  
  VegetationData2015$Abundance[is.na(VegetationData2015$Abundance)] <- 0
  
  VegetationData2015 <- dcast(data = VegetationData2015,  hid ~ SpeciesName, value.var = "Abundance")

VegetationData2015[is.na(VegetationData2015)] <- 0

shannon2015 <- diversity(VegetationData2015)

shannon2015 <- data.frame(shannon2015, VegetationData2015$hid)

names(shannon2015) <- c("shannon", "hid")

# Still some NAs for 2012 and 2015, these observations need to be dropped

shannon2012 <- na.omit(shannon2012)
shannon2015 <- na.omit(shannon2015)

mean(shannon2012$shannon)
mean(shannon2015$shannon)
mean(shannon2018$shannon)


write.csv(shannon2012, file = "shannon2012.csv")
write.csv(shannon2015, file = "shannon2015.csv")
write.csv(shannon2018, file = "shannon2018.csv")

# connect the right regencies with the hids/hhcodes

library(openxlsx)

household_list <- read.xlsx("farmer_plot_table_edited.xlsx")

# manually edited the excel sheet, deleted the irrelevant
# columns, added the plot ID and crop name to the replacement plots

colnames(household_list) <- c('regency','village','hid','plotID','crop') 

shannon2015 <- merge(shannon2015, household_list, by="hid")




