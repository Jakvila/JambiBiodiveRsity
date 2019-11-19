library(data.table)

invasives_new <- VegetationData2018
invasives_new$Abundance[is.na(invasives_new$Abundance)] <- 0

invasives_new$X <- NULL

invasives_new <- unique(invasives_new$SpeciesName)
invasives_new <- as.data.frame(invasives_new)
names(invasives_new) <- c("SpeciesName")

invasive_list <- c("Asystasia gangetica", "Asystasia spec.", "Clidemia hirta",
                   "Dicranopteris linearis", "Ageratum conyzoides")



SpeciesFreqTotal <- aggregate(as.numeric(invasives_new$Abundance), by=list(Category=invasives_new$SpeciesName), FUN=sum)



# SpeciesNumbersInv <- setDT(SpeciesFreqTotal)[, .(count= uniqueN(Abundance)), by = SpeciesName]

SpeciesFreqTotal$Invasive <- ifelse(SpeciesFreqTotal$SpeciesName %in% invasive_list, 1, 0)

names(SpeciesFreqTotal) <- c("SpeciesName", "Abundance", "Invasive")

write.csv(SpeciesFreqTotal, "InvasiveSpeciesList.csv")
