library(ggplot2)
library(dplyr)
library(plotly)
library(ggpmisc)
library(readr)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)
library(circular) # calculate angle std.

# This "comparetailpoints_v2.rmd" file has more variables for output.

fps <- 200
tailbeatfreq <- 2  # Hz
bf <- signal::butter(9, 2*tailbeatfreq / (0.5*fps))

## filter function

filter_span <- function(filt, data) {
  good <- !is.na(data)
  chunks <- which(good)
  
  # get places when not-na data starts
  a <- which(!is.na(data) & is.na(lag(data)))
  # and where it ends
  b <- which(!is.na(data) & is.na(lead(data)))
  
  # longest chunk
  k <- which.max(b - a)
  span <- rep_along(data, FALSE)
  span[a[k]:b[k]] <- TRUE
  
  datareal <- data[span]
  data3 <- c(rev(datareal), datareal, rev(datareal))
  data3s <- signal::filtfilt(filt, data3)
  
  datas <- rep_along(data, NA_real_)
  datas[span] <- data3s[length(datareal)+(1:length(datareal))]
  
  datas
}

ang_mean <- function(angdeg) {
  C = cos(angdeg * pi/180)
  S = sin(angdeg * pi/180)
  Cbar = mean(C, na.rm = TRUE)
  Sbar = mean(S, na.rm = TRUE)
  
  atan2(Sbar, Cbar) * 180/pi
}

read_data <- function(filename) {

  data <- read_csv(filename, col_names = TRUE)
  
  filedate <- str_extract(filename, '20\\d{6}')
  fishid <- str_extract(filename, 'ms\\d{2}')
  trialnum <- str_extract(filename, 'trial\\d+_') %>%
    str_extract('\\d+') %>%
    as.numeric()
  
  data %>%
    mutate(date = ymd(filedate),
           fish = fishid,
           trial = trialnum)
}  


## Process the data
a <- list.files(pattern = "*3dtail.csv")
b <- list.files(pattern = "*3dtail-cycles.csv")
# remove the useless files in the folder, look up for a function

lateral_angle_csv_files <- list.files(pattern = ".csv", full.names = TRUE, path = "/Users/minggong/Documents/Tytell_Lab/Project2/Data/UpAndDownGoodLateral/angle_vel_data/")
lateral_angle <- lateral_angle_csv_files %>% map_dfr(~read_csv(.), col_names = TRUE)

alldata <- list()

for (i in seq_along(a[1:52])) {
  file1 <- a[i]
  
  basename <- tools::file_path_sans_ext(file1)
  file2 <- paste0(basename, "-cycles.csv")

  data <- read_data(file1)
  cycles <- read_csv(file2)
  
  print(file1)
  print(file2)
  
  if (!askYesNo('Matched?')) {
    next
  }
  
  datacycles <-
    left_join(data, cycles %>% select(frame, cyclenum, height, peaksign)) %>%
    mutate(startcycleframe = if_else(!is.na(cyclenum), frame, NA_real_),
           startcycletime = startcycleframe/200) %>%
    fill(cyclenum, height, peaksign)
  
  filedate <- str_extract(file1, '20\\d{6}') %>% ymd()
  fishid <- str_extract(file1, 'ms\\d{2}')
  trialnum <- str_extract(file1, 'trial\\d+_') %>%
    str_extract('\\d+') %>%
    as.numeric()
  
  lateral_angle1 <- lateral_angle %>% 
    dplyr::filter(date == filedate, fish == fishid, trial == trialnum) %>% 
    select(date,
           fish,
           trial,
           frame,
           time,
           original_angle_smooth,
           body_angle_smooth,
           body_angle_smooth_rad,
           ang_vel_smooth_rads,
           left_eye_vy_BL_s,
           left_eye_vx_BL_s) # these column names depend on the "lateral_angle" data file 
  
  datacycles <- left_join(datacycles, lateral_angle1, by = c("date", "fish", "trial", "frame"))
  
  
  datacycles <- datacycles %>% mutate(dorsal_top_p_top_s = filter_span(bf, dorsal_top_p_top),
                                      dorsal_middle_p_top_s = filter_span(bf, dorsal_middle_p_top),
                                      middle_s = filter_span(bf, middle),
                                      ventral_middle_p_bottom_s = filter_span(bf, ventral_middle_p_bottom),
                                      ventral_bottom_p_bottom_s = filter_span(bf, ventral_bottom_p_bottom))
  fig0 <- datacycles %>%
    plot_ly(x = ~frame) %>%
    add_markers(y = ~dorsal_top_p_top_s, name = "smooth dorsal") %>%
    add_markers(y = ~dorsal_middle_p_top_s, name = "smooth dorsal mid") %>% 
    add_markers(y = ~middle_s, name = "smooth middle") %>% 
    add_markers(y = ~ventral_middle_p_bottom_s, name = "smooth ventral mid") %>% 
    add_markers(y = ~ventral_bottom_p_bottom_s, name = "smooth ventral bottom")
  print(fig0)
  
  if (!askYesNo('Next?')) {
    next
  }
  
  fig1 <- datacycles %>%
    plot_ly(x = ~frame) %>%
    add_markers(y = ~dorsal_top_p_top, name = "original dorsal") %>%
    add_markers(y = ~dorsal_top_p_top_s, name = "smooth dorsal") %>% 
    add_markers(y = ~ventral_bottom_p_bottom, name = "original ventral") %>% 
    add_markers(y = ~ventral_bottom_p_bottom_s, name = "smooth ventral")
  print(fig1)
  
  #color <- c('rgb(122, 35, 250)', "#5579FB", "#65A5E0")
  #color <- c('rgb(122, 35, 250)', 'rgb(80,122,251)', 'rgb(108, 191, 231)')
  
  if (!askYesNo('Next?')) {
    next
  }
  
  fig2 <- datacycles %>%
    plot_ly(x = ~time) %>%
    add_lines(y = ~dorsal_top_p_top_s, name = "dorsal top", line = list(color = 'rgb(122, 35, 250)')) %>%
    add_markers(x = ~startcycletime, y = ~dorsal_top_p_top_s, color = ~peaksign) %>%
    #add_lines(y = ~middle_s, name = "middle", line = list(color = 'rgb(80,122,251)')) %>%
    #add_markers(x = ~startcycletime, y = ~middle_s, color = ~peaksign) %>% 
    add_lines(y = ~ventral_bottom_p_bottom_s, name = "ventral bottom", line = list(color = 'rgb(108, 191, 231)')) %>%
    add_markers(x = ~startcycletime, y = ~ventral_bottom_p_bottom_s, color = ~peaksign) %>% 
    layout(scene = list(xaxis = list(title = "Time", showgrid = F),
                        yaxis = list(title = "Position", showgrid = F)))
  print(fig2)
  
  if (!askYesNo('Next?')) {
    next
  }
  
  
  # summarize the data
  datacycles_summary <- datacycles %>% 
    group_by(cyclenum, peaksign) %>%
    summarize(cycledur = (max(frame) - min(frame)) * 2 / fps,  # cycle duration in seconds
              start = min(frame)/fps,
              end = max(frame)/fps,
              max_dorsal = max(dorsal_top_p_top_s),
              max_dorsal_lead = which.max(dorsal_top_p_top_s) / fps, # unit: second
              min_dorsal = min(dorsal_top_p_top_s),
              min_dorsal_lead = which.min(dorsal_top_p_top_s) / fps,
              
              max_dorsalmid = max(dorsal_middle_p_top_s),
              max_dorsalmid_lead = which.max(dorsal_middle_p_top_s) / fps, # unit: second
              min_dorsalmid = min(dorsal_middle_p_top_s),
              min_dorsalmid_lead = which.min(dorsal_middle_p_top_s) / fps,
              
              max_middle = max(middle_s),
              max_middle_lead = which.max(middle_s) / fps, # unit: second
              min_middle = min(middle_s),
              min_middle_lead = which.min(middle_s) / fps,
              
              max_ventralmid = max(ventral_middle_p_bottom_s),
              max_ventralmid_lead = which.max(ventral_middle_p_bottom_s) / fps, # unit: second
              min_ventralmid = min(ventral_middle_p_bottom_s),
              min_ventralmid_lead = which.min(ventral_middle_p_bottom_s) / fps,
              
              max_ventral = max(ventral_bottom_p_bottom_s),
              max_ventral_lead = which.max(ventral_bottom_p_bottom_s) / fps, # unit: second
              min_ventral = min(ventral_bottom_p_bottom_s),
              min_ventral_lead = which.min(ventral_bottom_p_bottom_s) / fps,
              
              mean_original_angle_deg = ang_mean(original_angle_smooth),
              sd_original_angle_deg = angular.deviation(original_angle_smooth),
              mean_bodyangle_deg = ang_mean(body_angle_smooth),
              sd_bodyangle_deg = angular.deviation(body_angle_smooth*pi/180)*180/pi,
              mean_angvel_rads = mean(ang_vel_smooth_rads, na.rm = TRUE),
              sd_angvel_rads = sd(ang_vel_smooth_rads, na.rm = TRUE),
              mean_vert_v_BLs = mean(left_eye_vy_BL_s, na.rm = TRUE),
              sd_vert_v_BLs = sd(left_eye_vy_BL_s, na.rm = TRUE),
              mean_horiz_v_BLs = mean(left_eye_vx_BL_s, na.rm = TRUE),
              sd_horiz_v_BLs = sd(left_eye_vx_BL_s, na.rm = TRUE)) %>%
    mutate(dorsal_amp = case_when(peaksign == 'up'  ~  max_dorsal,
                                  peaksign == 'down'  ~  min_dorsal),
           dorsal_lead = case_when(peaksign == 'up'  ~  max_dorsal_lead,
                                   peaksign == 'down'  ~  min_dorsal_lead),
           dorsal_phase = dorsal_lead / cycledur,
           
           dorsalmid_amp = case_when(peaksign == 'up'  ~  max_dorsalmid,
                                  peaksign == 'down'  ~  min_dorsalmid),
           dorsalmid_lead = case_when(peaksign == 'up'  ~  max_dorsalmid_lead,
                                   peaksign == 'down'  ~  min_dorsalmid_lead),
           dorsalmid_phase = dorsalmid_lead / cycledur,
           
           middle_amp = case_when(peaksign == 'up'  ~  max_middle,
                                  peaksign == 'down'  ~  min_middle),
           middle_lead = case_when(peaksign == 'up'  ~  max_middle_lead,
                                   peaksign == 'down'  ~  min_middle_lead),
           middle_phase = middle_lead / cycledur,
           
           ventralmid_amp = case_when(peaksign == 'up'  ~  max_ventralmid,
                                  peaksign == 'down'  ~  min_ventralmid),
           ventralmid_lead = case_when(peaksign == 'up'  ~  max_ventralmid_lead,
                                   peaksign == 'down'  ~  min_ventralmid_lead),
           ventralmid_phase = ventralmid_lead / cycledur,
           
           ventral_amp = case_when(peaksign == 'up'  ~  max_ventral,
                                   peaksign == 'down'  ~  min_ventral),
           ventral_lead = case_when(peaksign == 'up'  ~ max_ventral_lead,
                                    peaksign == 'down'  ~  min_ventral_lead),
           ventral_phase = ventral_lead / cycledur) %>% 
    ungroup()
  
  
  # drop the rows with NA in cyclenum
  datacycles_summary <- datacycles_summary[!is.na(datacycles_summary$cyclenum), ]
  datacycles_summary <- datacycles_summary[!is.na(datacycles_summary$dorsal_amp | datacycles_summary$ventral_amp), ]
  
  
  # label whether a cycle is dorsal-leading or ventral-leading
  datacycles_summary <- datacycles_summary %>% 
    mutate(leading = "dorsal")
  
  datacycles_summary[datacycles_summary$dorsal_phase > datacycles_summary$ventral_phase,]$leading <- "ventral"
  
  
  # add trial info
  datacycles_summary <- datacycles_summary %>% 
    mutate(date = data$date[1], fish = data$fish[1], trial = data$trial[1])
  
  
  # Plot: original body angle and normalized body angle of each tail beats
  fig3 <- datacycles_summary %>% 
    plot_ly(x = ~start, y = ~mean_bodyangle_deg, type = "bar",
            error_y = ~list(array = sd_bodyangle_deg, color = '#000000'),
            xaxis = "Tail Beat Cycle",
            yaxis = "Mean Body Angles(degree)") %>%
    add_lines(data = datacycles, x = ~frame/fps, y = ~body_angle_smooth, name = "normalized",
              inherit = FALSE) %>% 
    add_lines(data = datacycles, x = ~frame/fps, y = ~original_angle_smooth, name = "original",
              inherit = FALSE)
  print(fig3)
  
  if (!askYesNo('Next?')) {
    next
  }
  
  # Plot: amplitude of each tail beats
  datacycles_summary <- datacycles_summary %>% 
    mutate(cycle_dorsal_amp = lead(dorsal_amp) - dorsal_amp, # difference between cycles
           cycle_ventral_amp = c(diff(ventral_amp), NA_real_))
  
 fig4 <- datacycles_summary %>% 
    plot_ly(x = ~cyclenum,
            xaxis = "Tail Beat Cycle",
            yaxis = "Cycle Amplitude") %>% 
    add_trace(y = ~cycle_dorsal_amp, type = "bar", name = "Dorsal") %>% 
    add_trace(y = ~cycle_ventral_amp, type = "bar", name = "Ventral")
 print(fig4)
  
  if (!askYesNo('Next?')) {
    next
  }
  
  # Plot: difference of phase between dorsal and ventral lobes
  datacycles_summary <- datacycles_summary %>% 
    mutate(phase_diff = dorsal_phase - ventral_phase)
    # positive value: ventral leading
    # negative value: dorsal leading
  fig5 <- datacycles_summary %>% 
    plot_ly(x = ~cyclenum, y = ~phase_diff, type = "bar", 
            yaxis = "Phase Difference")
  print(fig5)
  
  if (!askYesNo('Data OK?')) {
    next
  }
  
  # Save dataset as csv. file
  alldata[[i]] <- datacycles_summary
  
  # Save figures?
}

alldata <- bind_rows(alldata)
write_csv(alldata, '2022_alldata_5points.csv')
