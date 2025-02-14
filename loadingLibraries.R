######################
# Loading libraries
# Needs a packageList to be defined
######################

if('gstlearn' %in% packageList){
  # Installing the up-to-date gstlearn library
  install.packages("gstlearn",repos="https://soft.mines-paristech.fr/cran")
  library(gstlearn)
  packageList <- packageList[-which(packageList=='gstlearn')]
}

# Installing (if needed) and opening required CRAN libraries
notInList <- CRAN_package_list[!(packageList %in% installed.packages())]
lapply(notInList,install.packages,dependencies=TRUE)
lapply(packageList, require, character.only = TRUE)

# Choosing a Wesanderson colour palette if appropriate
if('wesanderson' %in% packageList){
  # Choosing a color palette
  myPalette = wes_palette("Zissou1", 16, type = "continuous")
}

# clean
rm(packageList,notInList)

