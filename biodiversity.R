#### Biodiversity data merge




setwd("P:/R-Kurs/Biodiversitas/3")
library(openxlsx)

biodiv_dat3 <- read.xlsx("survey3.xlsx")
  biodiv_dat3 <- biodiv_dat3[!(is.na(biodiv_dat3$X2)),]
  colnames(biodiv_dat3) <- c('species_name', 'abundance')
  biodiv_dat3 <- biodiv_dat3[!(biodiv_dat3$abundance=="Abundance"),]


# read xlsx sheet where abundance was accidentally entered in the 3rd column  
setwd("P:/R-Kurs/Biodiversitas/12")


folderlist <- c( 3, 12, 13, 15, 22, 24, 25, 29, 34, 37, 39,
                 39, 40, 44, 46, 47, 54, 56, 57, 60, 63, 70,
                 71, 74, 76, 80, 83, 84, 89, 116:118, 125,
                 126, 128, 169, 170, 173, 175, 181, 183, 191, 
                 196, 197, 200, 201, 204, 207, 209, 210, 220,
                 246:249, 251, 254, 255, 258, 260, 266, 269,
                 270, 273, 277, 279, 282, 325, 326, 329, 334,
                 338, 342, 346, 347, 349, 351, 354, 355, 358,
                 361, 362, 363, 367, 369, 373:376, 379, 382,
                 385, 387, 388, 395, 397, 399, 403,
                 410,415, 416, 419, 423:425, 433, 435:437,
                 442:445, 449, 455, 459, 460, 463, 464, 467,
                 471, 474, 481:483, 485, 486, 488, 489, 495,
                 496, 502, 503, 505, 507, 508, 511, 515,
                 516, 519, 527:529, 537, 540, 541, 543,
                 547:548, 550, 554, 555, 559, 565, 569,
                 571, 574, 575, 577:579, 581, 584, 588, 590,
                 597, 599, 600, 609:611, 615, 619,
                 629, 630, 632, 633, 635, 638, 639, 641,
                 649, 650, 654, 656, 658, 667, 670,
                 672, 675, 676, 678, 681, 687, 690, 695, 698,
                 699)

n <- c( 12, 13, 15, 22, 24, 25, 29, 34, 37, 39,
        39, 40, 44, 46, 47, 54, 56, 57, 60, 63, 70,
        71, 74, 76, 80, 83, 84, 89, 116:118, 125,
        126, 128, 169, 170, 173, 175, 181, 183, 191, 
        196, 197, 200, 201, 204, 207, 209, 210, 220,
        246:249, 251, 254, 255, 258, 260, 266, 269,
        270, 273, 277, 279, 282, 325, 326, 329, 334,
        338, 342, 346, 347, 349, 351, 354, 355, 358,
        361, 362, 363, 367, 369, 373:376, 379, 382,
        385, 387, 388, 395, 397, 399, 403,
        410,415, 416, 419, 423:425, 433, 435:437,
        442:445, 449, 455, 459, 460, 463, 464, 467,
        471, 474, 481:483, 485, 486, 488, 489, 495,
        496, 502, 503, 505, 507, 508, 511, 515,
        516, 519, 527:529, 537, 540, 541, 543,
        547:548, 550, 554, 555, 559, 565, 569,
        571, 574, 575, 577:579, 581, 584, 588, 590,
        597, 599, 600, 609:611, 615, 619,
        629, 630, 632, 633, 635, 638, 639, 641,
        649, 650, 654, 656, 658, 667, 670,
        672, 675, 676, 678, 681, 687, 690, 695, 698,
        699, 750)

library(openxlsx)
library(plyr)

# survey542 renamed manually to "survey541.xlsx" --> replacement made,
# but document name wasn't changed

setwd(file.path("P:/R-Kurs/Biodiversitas/3")) 
biodiv_dat <- read.xlsx(paste("survey", 3, ".xlsx", sep="")) # create first dataset

#rbind loop 
x <-  read.xlsx(paste("survey", 3, ".xlsx", sep=""),
                startRow = 1, colNames = FALSE )
x$hhid <- 3


for(i in n){
  
  setwd(file.path("P:/R-Kurs/Biodiversitas/",i ))
  
  x <- rbind.fill(x, read.xlsx(paste("survey", i, ".xlsx", sep=""),
                               startRow = 1, colNames = FALSE))
        
  x$hhid[is.na(x$hhid)] <- i
}


x$X2 <- x$X3
x$X3 <- NULL
x <- x[!(is.na(x$X2)),]
colnames(x) <- c('species_name', 'abundance', 'hhid')

biodiv_dat_jambi_2018 <- x
  
  
setwd("//ug-uyst-ba-cifs.student.uni-goettingen.de/home/users/j.latzko/Desktop/Master_thesis/bernie_stuff")
library(openxlsx)
household_list <- read.xlsx("farmer_plot_table_edited.xlsx")

# manually edited the excel sheet, deleted the irrelevant
# columns, added the plot ID and crop name to the replacement plots

colnames(household_list) <- c('regency','village','hhid','plotID','crop') 

VegetationData2018 <- merge(biodiv_dat_jambi_2018, household_list, by="hhid")
VegetationData2018$year <- 2018 

write.csv(VegetationData2018, file="VegetationData2018.csv")
# 16 observations lost

# Fuzzy string matching
library(stargazeR)
table(VegetationData2018$species_name)

agrep("Asystasia gangetica", VegetationData2018$species_name)

# MAKE DIVERSITY INDICATORS

setwd("//ug-uyst-ba-cifs.student.uni-goettingen.de/home/users/j.latzko/Desktop/Master_thesis/bernie_stuff")

VegetationData <- read.csv("Vegetation.csv")

library(data)





