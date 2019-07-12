
setwd("C:/Users/jakob/OneDrive/Desktop/Universidad/R_assignment")

matrix_wide_2018 <- read.csv("matrix_wide_2018.csv")

library(permute)
library(lattice)
library(vegan)
library(data.table)
library(reshape2)

matrix_wide_2018 = data.table(matrix_wide_2018)
matrix_wide_2018 = matrix_wide_2018[, .(hid, SpeciesName, Abundance)]


# Check for duplicate entries
Duplicated = matrix_wide_2018[duplicated(matrix_wide_2018 [, .(hid, SpeciesName)])==T,]


# 20 found, all copies
#....

matrix_wide_2018 <- dcast(data = matrix_wide_2018,  hid ~ SpeciesName, value.var = "Abundance")



# make species richness indicators

simpson <- diversity(matrix_wide_2018, index="simpson")
shannon <- diversity(matrix_wide_2018)


View(shannon)
shannon2018 <- data.frame(shannon, matrix_wide_2018$hid)
names(shannon2018) <- c("shannon", "hid")

# the same for the other rounds

read.csv()