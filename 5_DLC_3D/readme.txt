Edited on May 10th, 2023:

New files:

	1_plot_3D_template.Rmd: New template based on "plot_3d_20220729_ms06_trial11.Rmd"

	1-1_Process_3D_data_allfiles.Rmd: It is a Rmd file running the template on all data files. 

Move all csv data files to ./Data/5_Raw3D

--------------------------------------------------------------------------------------
Edited on March 28th, 2023:

This folder includes the position data of points from triangulated data, and corresponding Rmd files.

20211013-20211110
9 points:
c("dorsal_top",
  "dorsal_middle",
  "middle",
  "ventral_middle",
  "ventral_bottom",
  "peduncle_top",
  "peduncle_middle",
  "peduncle_bottom",
  "eye")

20220706-20220729
15 points:
c("caudal_dorsal_top",
  "caudal_dorsal_middle",
  "caudal_middle",
  "caudal_ventral_middle",
  "caudal_ventral_bottom",
  "peduncle_top",
  "peduncle_middle",
  "peduncle_bottom",
  "left_eye",
  "spiny_dorsal_ant_base",
  "soft_dorsal_post_base",
  "left_lateral_strip",
  "anal_ant_base",
  "anal_post_base",
  "hyoid")

For the data from 20211013 to 20211029, they include codes for identifying peaks (ggpmisc:::find_peaks()) and calculating phase shift (ccf()), which were used in the course report of BIO194_spring2022. Because data for 20211110 and 2022 experiments were not used in the course report of BIO194_spring2022, their corresponding Rmd files do not have codes for identifying peaks and phase shift. For the latest quantitative analysis, all peak and phase calculation were processed in "~/findtailbeat/processed/alldata/alldata.Rmd".