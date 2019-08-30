# Fuzzy string matching

for (i in agrep("Asystasia gangetica", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Asystasia gangetica"
}

for (i in agrep("Centotheca lappacea", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Centotheca lappacea"
}

#    Vegetation2018_complete$SpeciesName[607] <- "Centotheca lappacea"
#    Vegetation2018_complete$SpeciesName[789] <- "Centotheca lappacea" #manual changes due to  agrep not grasping the right name

for (i in agrep("Cyrtococcum patens", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Cyrtococcum patens"
}

for (i in agrep("Endospermum diadenum", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Endospermum diadenum"
}
#    Vegetation2018_complete$SpeciesName[78] <- "Endospermum diadenum"
#    Vegetation2018_complete$SpeciesName[239] <- "Endospermum diadenum"
#    Vegetation2018_complete$SpeciesName[1002] <- "Endospermum diadenum"

for (i in agrep("Hevea Brasiliensis", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Hevea Brasiliensis"
}

for (i in agrep("Imperata cylindrica", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Imperata cylindrica"
}

for (i in agrep("Lasianthus inaequalis", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Lasianthus inaequalis"
}
Vegetation2018_complete$SpeciesName[331] <- "Like Lasianthus inaequalis"

for (i in agrep("Leptaspis Urceolata", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Leptaspis urceolata"
}
#    Vegetation2018_complete$SpeciesName[77] <- "Leptaspis urceolata"
#    Vegetation2018_complete$SpeciesName[95] <- "Leptaspis urceolata"
#    Vegetation2018_complete$SpeciesName[113] <- "Leptaspis urceolata"
#   Vegetation2018_complete$SpeciesName[204] <- "Leptaspis urceolata"

for (i in agrep("Mallotus peltatus", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Mallotus peltatus"
}

for (i in agrep("Mangga", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Mangifera indica"
}

# Vegetation2018_complete$SpeciesName[801] <- "Melastoma malabathricum"

for (i in agrep("Mimosa", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Mimosa pudica"
}  
for (i in agrep("Molinera latifolia", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Molinera latifolia"
}
for (i in agrep("Mussaenda? frondosa", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Mussaenda frondosa"
}
for (i in agrep("Ottochlosa nodosa", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Ottochloa nodosa"
}
for (i in agrep("Paspalum dilatatum", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Paspalum dilatatum"
}
for (i in agrep("Polygala", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Polygala paniculata"
}
for (i in agrep("Rothmania", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Rothmannia macrophylla"
}
for (i in agrep("Salomonia cantoniensis", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Salomonia cantoniensis"
}
for (i in agrep("Vittaria elongata", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Vittaria elongata"

}

###

agrep("Acmella paniculata", Vegetation2018_complete$SpeciesName)
length(agrep("Acmella paniculata", Vegetation2018_complete$SpeciesName))

for (i in agrep("Acmella paniculata", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Acmella paniculata"
}

Vegetation2018_complete$SpeciesName[1414] <- "Looks like Acmella paniculata"


agrep("Ageratum conyzoides", Vegetation2018_complete$SpeciesName)
length(agrep("Ageratum conyzoides", Vegetation2018_complete$SpeciesName))

for (i in agrep("Ageratum conyzoides", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Ageratum conyzoides"
}

for (i in agrep("Alstonia scholaris", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Alstonia scholaris"
}

length(agrep("Amplas Kijang", Vegetation2018_complete$SpeciesName))
for (i in agrep("Amplas kijang", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Amplas kijang"
}

length(agrep("Asplenium glaucophyllum", Vegetation2018_complete$SpeciesName))
for (i in agrep("Asplenium glaucophyllum", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Asplenium glaucophyllum"
}

length(agrep("Asplenium longissimum", Vegetation2018_complete$SpeciesName))
for (i in agrep("Asplenium longissimum", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Asplenium longissimum"
}

length(agrep("Asplenium nidus", Vegetation2018_complete$SpeciesName))
for (i in agrep("Asplenium nidus", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Asplenium nidus"
}

length(agrep("Axonopus compressus", Vegetation2018_complete$SpeciesName))
for (i in agrep("Axonopus compressus", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Axonopus compressus"
}

length(agrep("borreria alata", Vegetation2018_complete$SpeciesName))
for (i in agrep("borreria alata", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Borreria alata"
}


length(agrep("Calopogonium Mucunoides", Vegetation2018_complete$SpeciesName))
agrep("Calopogonium Mucunoides", Vegetation2018_complete$SpeciesName)


 for (i in agrep("Calopogonium Mucunoides", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Calopogonium mucunoides"
 }

which(Vegetation2018_complete$SpeciesName=="Calopogonium")

Vegetation2018_complete$SpeciesName[1917] <- "Calopogonium mucunoides"

which(Vegetation2018_complete$SpeciesName=="Calpogium mucunoides")

Vegetation2018_complete$SpeciesName[516] <- "Calopogonium mucunoides"
  
length(agrep("Centhoteca lappacea", Vegetation2018_complete$SpeciesName))
agrep("Centotheca", Vegetation2018_complete$SpeciesName)


for (i in agrep("Centotheca lappacea", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Centotheca lappacea"
}
for (i in agrep("Centhoteca lappacea", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Centotheca lappacea"
}

length(agrep("Centrosema pubescens", Vegetation2018_complete$SpeciesName))
agrep("Centrosema pubescens", Vegetation2018_complete$SpeciesName)
for (i in agrep("Centrosema pubescens", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Centrosema pubescens"
}

which(Vegetation2018_complete$SpeciesName=="Centrocema")
Vegetation2018_complete$SpeciesName[1915] <- "Centrosema pubescens"


length(agrep("Cheilocostus speciosus", Vegetation2018_complete$SpeciesName))
agrep("Cheilocostus speciosus", Vegetation2018_complete$SpeciesName)
for (i in agrep("Cheilocostus speciosus", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Cheilocostus speciosus"
}
 

which(Vegetation2018_complete$SpeciesName=="Looks like cleome rutidosperma")
which(Vegetation2018_complete$SpeciesName=="cleome berbulu")
which(Vegetation2018_complete$SpeciesName=="cleomerutidsperma")

length(agrep("Cleome rutidosperma", Vegetation2018_complete$SpeciesName))
 agrep("Cleome rutidosperma", Vegetation2018_complete$SpeciesName)

 for (i in agrep("Cleome rutidosperma", Vegetation2018_complete$SpeciesName)){
  Vegetation2018_complete$SpeciesName[i]<- "Cleome rutidosperma"
  }
 Vegetation2018_complete$SpeciesName[963] <- "Cleome rutidosperma"
 Vegetation2018_complete$SpeciesName[2078] <- "Looks like cleome rutidosperma"
 
 length(agrep("Clibadium surinamense", Vegetation2018_complete$SpeciesName))
 agrep("Clibadium surinamense", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Clibadium surinamense", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Clibadium surinamense"
 }
 
 which(Vegetation2018_complete$SpeciesName=="clidemia hirta")
 Vegetation2018_complete$SpeciesName[3] <- "Clidemia hirta"
 
 length(agrep("Cuphea carthagenensis", Vegetation2018_complete$SpeciesName))
 agrep("Cuphea carthagenensis", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Cuphea carthagenensis", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Cuphea carthagenensis"
 }
 
 length(agrep("Cyrtococcum patens", Vegetation2018_complete$SpeciesName))
 agrep("Cyrtococcum patens", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Cyrtococcum patens", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Cyrtococcum patens"
 }
 for (i in agrep("Cyrtococcum Pattern", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Cyrtococcum patens"
 }
 
 length(agrep("Daun Bergerigi", Vegetation2018_complete$SpeciesName))
 which(Vegetation2018_complete$SpeciesName=="Daun Bergerigi")
 
 Vegetation2018_complete$SpeciesName[113] <- "daun bergerigi"
 Vegetation2018_complete$SpeciesName[1428] <- "daun bergerigi"
 Vegetation2018_complete$SpeciesName[2135] <- "daun bergerigi"
 
 
 which(Vegetation2018_complete$SpeciesName=="Durio zibethius")
 Vegetation2018_complete$SpeciesName[2202] <- "Durio zibethinus"
 
 which(Vegetation2018_complete$SpeciesName=="Durian")
 Vegetation2018_complete$SpeciesName[1976] <- "Durio zibethinus"
 
 length(agrep("Elaeis guineensis", Vegetation2018_complete$SpeciesName))
 agrep("Elaeis guineensis", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Elaeis guineensis", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Elaeis guineensis"
 }
 
 
 length(agrep("Endospermum diadenum", Vegetation2018_complete$SpeciesName))
 agrep("Endospermum diadenum", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Endospermum diadenum", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Endospermum diadenum"
 }
 
 which(Vegetation2018_complete$SpeciesName=="endospermum")
 Vegetation2018_complete$SpeciesName[112] <- "Endospermum diadenum"
 Vegetation2018_complete$SpeciesName[271] <- "Endospermum diadenum"

 
 length(agrep("Globba pendula", Vegetation2018_complete$SpeciesName))
 agrep("Globba pendula", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Globba pendula", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Globba pendula"
 }
 
 for (i in agrep("goniophlebium percussum", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Goniophlebium verrucosum"
 }
 
 length(agrep("Helminthostachys zeylanica", Vegetation2018_complete$SpeciesName))
 agrep("Helminthostachys zeylanica", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Helminthostachys zeylanica", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Helminthostachys zeylanica"
 }
 
 for (i in agrep("helmin minthostachys", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Helminthostachys zeylanica"
 }
 
 	
 length(agrep("Ixora grandifolia", Vegetation2018_complete$SpeciesName))
 agrep("Ixora grandifolia", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Ixora grandifolia", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Ixora grandifolia"
 }
 
 which(Vegetation2018_complete$SpeciesName=="Kayu berumbung")
 Vegetation2018_complete$SpeciesName[569] <- "kayu berumbung"

 
 length(agrep("Leptaspis urceolata", Vegetation2018_complete$SpeciesName))
 agrep("Leptaspis urceolata", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Leptaspis urceolata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Leptaspis urceolata"
 }
 for (i in agrep("leptaspis", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Leptaspis urceolata"
 }
 
 
 length(agrep("Lindernia diffusa", Vegetation2018_complete$SpeciesName))
 agrep("Lindernia diffusa", Vegetation2018_complete$SpeciesName) 
 
 for (i in agrep("Lindernia diffusa", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Lindernia diffusa"
 }
 
 which(Vegetation2018_complete$SpeciesName=="Looks like Lindsaea Ensifolia")
 length(agrep("Lindsaea ensifolia", Vegetation2018_complete$SpeciesName))
 agrep("Lindsaea ensifolia", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Lindsaea ensifolia", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Lindsaea ensifolia"
 }
 
 for (i in agrep("lynseae ensifolia", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Lindsaea ensifolia"
 }
 
 Vegetation2018_complete$SpeciesName[357] <- "Looks like Lindsaea Ensifolia"
 
 for (i in agrep("Macaranga bancana", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Macaranga bancana"
 }
 
 
 
 length(agrep("Marantodes pumilum", Vegetation2018_complete$SpeciesName))
 agrep("Marantodes pumilum", Vegetation2018_complete$SpeciesName)
 which(Vegetation2018_complete$SpeciesName=="mirip marantodes pumilium")
 
 for (i in agrep("Marantodes pumilum", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Marantodes pumilum"
 }
 
 Vegetation2018_complete$SpeciesName[1797] <- "mirip marantodes pumilium"

 
 length(agrep("Merremia umbellata", Vegetation2018_complete$SpeciesName))
 agrep("Merremia umbellata", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Merremia umbellata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Merremia umbellata"
 }
 for (i in agrep("Meremia umbellata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Merremia umbellata"
 }
 
 
 length(agrep("Molinera latifolia", Vegetation2018_complete$SpeciesName))
 agrep("Molinera latifolia", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Molinera latifolia", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Molinera latifolia"
 }
 
 
 
 length(agrep("Mussaenda frondosa", Vegetation2018_complete$SpeciesName))
 agrep("Mussaenda frondosa", Vegetation2018_complete$SpeciesName)
 which(Vegetation2018_complete$SpeciesName=="Looks like Mussaenda Frondusa")
 for (i in agrep("Mussaenda frondosa", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Mussaenda frondosa"
 }
 Vegetation2018_complete$SpeciesName[351] <- "Looks like Mussaenda Frondusa"
 
 
 
 length(agrep("Ophioglossum pendulum", Vegetation2018_complete$SpeciesName))
 agrep("Ophioglossum pendulum", Vegetation2018_complete$SpeciesName)

 for (i in agrep("Ophioglossum pendulum", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Ophioglossum pendulum"
 }

 which(Vegetation2018_complete$SpeciesName=="Ottochlosa nodosa")
 Vegetation2018_complete$SpeciesName[2239] <- "Ottochloa nodosa"
 
 which(Vegetation2018_complete$SpeciesName=="paspalum dilatatum")
 Vegetation2018_complete$SpeciesName[2243] <- "Paspalum dilatatum"
 Vegetation2018_complete$SpeciesName[2358] <- "Paspalum dilatatum"
 
 
 length(agrep("Passiflora foetida", Vegetation2018_complete$SpeciesName))
 agrep("Passiflora foetida", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Passiflora foetida", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Passiflora foetida"
 }
 
 
 length(agrep("pennisetum polystachion", Vegetation2018_complete$SpeciesName))
 agrep("pennisetum polystachion", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("pennisetum polystachion", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "pennisetum polystachion"
 }
 
 
 
 length(agrep("Peperomia pellucida", Vegetation2018_complete$SpeciesName))
 agrep("Peperomia pellucida", Vegetation2018_complete$SpeciesName)
 which(Vegetation2018_complete$SpeciesName=="Looks Like peperomia pellucida")
 for (i in agrep("Peperomia pellucida", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Peperomia pellucida"
 }
 Vegetation2018_complete$SpeciesName[1408] <- "Looks Like peperomia pellucida"

 
 which(Vegetation2018_complete$SpeciesName=="rhynchospora colorata")
 Vegetation2018_complete$SpeciesName[1104] <- "Rhynchospora colorata"
 
 
 
 for (i in agrep("Rothmannia macrophylla", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Rothmannia macrophylla"
 }
 
 length(agrep("Sauropus androgynus", Vegetation2018_complete$SpeciesName))
 agrep("Sauropus androgynus", Vegetation2018_complete$SpeciesName)

 for (i in agrep("Sauropus androgynus", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Sauropus androgynus"
 }
  
 
 
 length(agrep("Scleria ciliaris", Vegetation2018_complete$SpeciesName))
 agrep("Scleria ciliaris", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Scleria ciliaris", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Scleria ciliaris"
 }

  
 length(agrep("Scoparia dulcis", Vegetation2018_complete$SpeciesName))
 agrep("Scoparia dulcis", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Scoparia dulcis", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Scoparia dulcis"
 }
 
 
 
 length(agrep("Selaginella plana", Vegetation2018_complete$SpeciesName))
 agrep("Selaginella plana", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Selaginella plana", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Selaginella plana"
 }
 
 length(agrep("Spermaococe cristata", Vegetation2018_complete$SpeciesName))
 agrep("Spermaococe cristata", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Spermaococe cristata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Spermacoce cristata"
 }
 
 
 length(agrep("Sphagneticola trilobata", Vegetation2018_complete$SpeciesName))
 agrep("Sphagneticola trilobata", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Sphagneticola trilobata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Sphagneticola trilobata"
 }
  
 
 length(agrep("Taenitis blechnoides", Vegetation2018_complete$SpeciesName))
 agrep("Taenitis blechnoides", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Taenitis blechnoides", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Taenitis blechnoides"
 }
 
 
 
 length(agrep("Tetracera indica", Vegetation2018_complete$SpeciesName))
 agrep("Tetracera indica", Vegetation2018_complete$SpeciesName)
 which(Vegetation2018_complete$SpeciesName=="Looks Like Tetracera Indica")
 
 for (i in agrep("Tetracera indica", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Tetracera indica"
 }
 Vegetation2018_complete$SpeciesName[2076] <- "Looks Like Tetracera Indica"
 
 
 
 "Torenia violacea"
 
 length(agrep("Torenia violacea", Vegetation2018_complete$SpeciesName))
 agrep("Torenia violacea", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep("Torenia violacea", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Torenia violacea"
 }

 
 for (i in agrep( "Tradescantia fluminensis variegata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Tradescantia fluminensis variegata"
 }
 
 
 length(agrep("Uncaria cordata", Vegetation2018_complete$SpeciesName))
 agrep("Uncaria cordata", Vegetation2018_complete$SpeciesName)
 
 for (i in agrep( "Uncaria cordata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Uncaria cordata"
 }
 
 for (i in agrep( "Urena lobata", Vegetation2018_complete$SpeciesName)){
   Vegetation2018_complete$SpeciesName[i]<- "Urena lobata"
 }
 
 which(Vegetation2018_complete$SpeciesName=="tumbuhan paku")
 Vegetation2018_complete$SpeciesName[252] <- "Tumbuhan Paku"
 
 
 which(Vegetation2018_complete$SpeciesName=="talas")
 Vegetation2018_complete$SpeciesName[1224] <- "Talas"
 
 which(Vegetation2018_complete$SpeciesName=="synedrella nudiflora")
 Vegetation2018_complete$SpeciesName[975] <- "Synedrella nudiflora"
 
 write.csv(Vegetation2018_complete, "Vegetation2018_correctedstrings")
  