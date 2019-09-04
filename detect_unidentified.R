# Detect the renamed plots

# loop idea


  
renamez <-  Vegetation2018_correctedstrings$SpeciesName %in% species_list$...1

Vegetation2018_correctedstrings$renamed <- 1

Vegetation2018_correctedstrings$renamed <- as.numeric(Vegetation2018_correctedstrings$SpeciesName %in% species_list$...1)

Vegetation2018_correctedstrings$renamed[Vegetation2018_correctedstrings$renamed==1] <- 2
Vegetation2018_correctedstrings$renamed[Vegetation2018_correctedstrings$renamed==0] <- 1
Vegetation2018_correctedstrings$renamed[Vegetation2018_correctedstrings$renamed==2] <- 0


write.csv(Vegetation2018_complete, "Vegetation2018_renamed.csv")


