setwd("P:/MasterOfDisasterThesis/JambiBiodiveRsity-master")

# need to add the species names that were given out to the assistants on the documents.

names(species1) <- c("Species")

species_list <- rbind(species1, species2)
species_list <- unique(species_list)


other_species <- c("Sauropus androgynus", "Hevea Brasiliensis", "Rolandra fruticosa", "Pityrogramna Calomelanos",  #need to correct in fuzzy strings
"Colocasia Esculenta", "Molinera latifolia", "carex rupestris", "lygodium cf.salicifolium", "Borreria alata",
"pennisetum polystachion", "Nephrolepis biserrata", "Poikilispermum suaveolens", "Ottochloa nodosa",
"Lansium parasiticum", "Solanum betaceum", "Syzygium polyanthum", "")

chutapoweon <- read.csv("Vegetation2018_correctedstrings.csv")

# before after combining the two list we had 323 species, now we have 287.

chutapoweon$renamed <- NA
chutapoweon$renamed <- as.numeric(chutapoweon$SpeciesName %in% species_list$Species)

chutapoweon$renamed[chutapoweon$renamed==1] <- 2
chutapoweon$renamed[chutapoweon$renamed==0] <- 1
chutapoweon$renamed[chutapoweon$renamed==2] <- 0




write.csv(chutapoweon, "Vegetation2018_renamed.csv")
write.csv(species_list, "species_list.csv")






