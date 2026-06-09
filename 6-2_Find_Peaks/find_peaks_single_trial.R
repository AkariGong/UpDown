
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Description: This script is used to run find_peaks.R with one single trial 
##
## Author: Ming Gong
## Date: 2026-06-09
## Email: ming.gong@tufts.edu
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# load packages -----------------------------------------------------------

library(here)


# Specify which trial to check --------------------------------------------

filedate <- "20211028"
fishid <- "ms03"
trialnum <- 5

source_path = here("Code", "6-2_Find_Peaks", "find_peaks.R")
source(source_path)
