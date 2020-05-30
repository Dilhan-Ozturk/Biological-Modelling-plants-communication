# Script to change the value of mycorrhizal growth rate and record the result (need of io function.R) with CRootBox

library(tidyverse)
library(data.table)
library(stringi)
library(plyr)
library(Hmisc)
options(scipen=999) # Disable scientific notation
#-----------------------------------------------------------------------------------------
#--------------------------- GENERAL OPTIONS ---------------------------------------------
#-----------------------------------------------------------------------------------------
# Main directory, where everthing is stored
dir.base <- "CRootBox-master/"
# Where is ArchiSimple folder
setwd(dir.base)
# Load custom functions
source("io_function.R")
r_range <- seq(from=0.05, to=5, by=0.01)
#-----------------------------------------------------------------------------------------
#--------------------------- READ THE INITIAL PARAMETER FILES ---------------------------------------------
#-----------------------------------------------------------------------------------------
# This is done to always start from the some parameters when we induce variations
dataset_init <- read_rparam('original/param.rparam')
# plant_init <- read_pparam('original/param.pparam')
#-----------------------------------------------------------------------------------------
#--------------------------- WRITE NEW PARAMETER FILES ---------------------------------------------
#-----------------------------------------------------------------------------------------
k <- 0
for(r in r_range){
  k <- k+1
  unid <- stri_rand_strings(1, 10) # Get a unique ID for the simulation
  # plant <- plant_init
  dataset <- dataset_init
  print("-----------------------")
  print(paste0("Simulation ",k," started"))
  # Vary "r" for type 20 and 50
  dataset$val1[dataset$type==20 & dataset$param == "r"] <- r
  dataset$val1[dataset$type==50 & dataset$param == "r"] <- r
  # Write the data in the parameter file that will be sued nby CRootBox
  write_rparam(dataset, "www/param.rparam")
  # write_pparam(plant, "www/param.pparam")
  #--------------------------- RUN CODE AND STORE DATA ---------------------------------------------
  system("a.exe")
  file.rename(from = "30_rootsystem.txt",
              to = paste0("resultats/",r,"-30-rootsystem.txt"))
}