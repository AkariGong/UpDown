## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Description: This script is used to run find_peaks.R with all trials in 
## one folder.
## Author: Ming Gong
## Date: 2026-06-09
## Email: ming.gong@tufts.edu
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)


# 1. Read files -----------------------------------------------------------

source_path = here("Code", "6-2_Find_Peaks", "find_peaks.R")
tail_folder <- here("Data", "6_Processed3D", "good_data_v2", "3dtail")
cycles_folder <- here("Data", "6_Processed3D", "good_data_v2", "cycles")

tail_files <- list.files(
  tail_folder,
  pattern = "_3dtail\\.csv$",
  full.names = TRUE
)

trial_list <- tibble(
  tail_file = tail_files,
  file_name = basename(tail_files)
) %>%
  mutate(
    trial_name = str_remove(file_name, "_3dtail\\.csv$"),
    cycles_file = file.path(cycles_folder, paste0(trial_name, "_3dtail-cycles.csv")),
    
    filedate = str_extract(trial_name, "^\\d{8}"),
    fishid = str_extract(trial_name, "(?<=_)[^_]+(?=_trial)"),
    trialnum = str_extract(trial_name, "(?<=_trial)\\d+") %>% as.integer()
  ) %>%
  filter(
    file.exists(cycles_file)
  )


# 2. Find peaks for the list  ---------------------------------------------

for (i in seq_len(nrow(trial_list))) {
  
  trial_env <- new.env()
  
  trial_env$filedate <- trial_list$filedate[i]
  trial_env$fishid <- trial_list$fishid[i]
  trial_env$trialnum <- trial_list$trialnum[i]
  
  message(
    "Processing: ",
    trial_env$filedate, "_",
    trial_env$fishid, "_trial",
    sprintf("%02d", trial_env$trialnum)
  )
  
  source(source_path, local = trial_env)
}
