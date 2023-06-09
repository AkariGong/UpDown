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


lateral_angle <- read_csv("/Users/minggong/Documents/Tytell_Lab/Project2/Data/UpAndDownLateral3/videos/lateral_angle_20211013_ms03.csv")

alldata <- list()

for (i in seq_along(a[1:11])) {
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
    mutate(startcycleframe = if_else(!is.na(cyclenum), frame, NA_real_)) %>%
    fill(cyclenum, height, peaksign)
  
  filedate <- str_extract(file1, '20\\d{6}') %>% ymd()
  fishid <- str_extract(file1, 'ms\\d{2}')
  trialnum <- str_extract(file1, 'trial\\d+_') %>%
    str_extract('\\d+') %>%
    as.numeric()
  
  lateral_angle1 <- lateral_angle %>% 
    filter(date == filedate, fish == fishid, trial == trialnum) %>% 
    select(date,
           fish,
           trial,
           frame,
           body_angle_smooth,
           angular_speed,
           left_eye_vy_BL_s)
  
  datacycles <- left_join(datacycles, lateral_angle1, by = c("date", "fish", "trial", "frame"))
  
  fps <- 200
  tailbeatfreq <- 2  # Hz
  bf <- signal::butter(9, 2*tailbeatfreq / (0.5*fps))
  
  datacycles <- datacycles %>% mutate(dorsal_top_p_top_s = filter_span(bf, dorsal_top_p_top),
                                      dorsal_middle_p_top_s = filter_span(bf, dorsal_middle_p_top),
                                      middle_s = filter_span(bf, middle),
                                      ventral_middle_p_bottom_s = filter_span(bf, ventral_middle_p_bottom),
                                      ventral_bottom_p_bottom_s = filter_span(bf, ventral_bottom_p_bottom))
  
  fig1 <- datacycles %>%
    plot_ly(x = ~frame) %>%
    add_markers(y = ~dorsal_top_p_top, name = "original dorsal") %>%
    add_markers(y = ~dorsal_top_p_top_s, name = "smooth dorsal") %>% 
    add_markers(y = ~ventral_bottom_p_bottom, name = "original ventral") %>% 
    add_markers(y = ~ventral_bottom_p_bottom_s, name = "smooth ventral")
  print(fig1)
  
  fig2 <- datacycles %>%
    plot_ly(x = ~frame) %>%
    add_lines(y = ~dorsal_top_p_top_s, name = "dorsal top") %>%
    add_markers(x = ~startcycleframe, y = ~dorsal_top_p_top_s, color = ~peaksign) %>%
    add_lines(y = ~middle_s, name = "middle") %>%
    add_markers(x = ~startcycleframe, y = ~middle_s, color = ~peaksign) %>% 
    add_lines(y = ~ventral_bottom_p_bottom_s, name = "ventral bottom") %>%
    add_markers(x = ~startcycleframe, y = ~ventral_bottom_p_bottom_s, color = ~peaksign)
  print(fig2)
  
  if (!askYesNo('Are all smoothed traces OK?')) {
    next
  } else {
    tailbeatfreq <- readline(prompt="Enter frequency (integer): ")
  }

  
  # summarize the data
  datacycles_summary <- datacycles %>% 
    group_by(cyclenum, peaksign) %>%
    summarize(cycledur = (max(frame) - min(frame)) * 2 / fps,  # cycle duration in seconds
              start = min(frame)/fps,
              end = max(frame)/fps,
              max_dorsal = max(dorsal_top_p_top_s),
              max_dorsal_lead = which.max(dorsal_top_p_top_s) / fps,
              min_dorsal = min(dorsal_top_p_top_s),
              min_dorsal_lead = which.min(dorsal_top_p_top_s) / fps,
              max_ventral = max(ventral_bottom_p_bottom_s),
              max_ventral_lead = which.max(ventral_bottom_p_bottom_s) / fps,
              min_ventral = min(ventral_bottom_p_bottom_s),
              min_ventral_lead = which.min(ventral_bottom_p_bottom_s) / fps,
              mean_bodyangle = ang_mean(body_angle_smooth),
              sd_bodyangle = angular.deviation(body_angle_smooth*pi/180)*180/pi) %>%
    mutate(dorsal_amp = case_when(peaksign == 'up'  ~  max_dorsal,
                                  peaksign == 'down'  ~  min_dorsal),
           dorsal_lead = case_when(peaksign == 'up'  ~  max_dorsal_lead,
                                   peaksign == 'down'  ~  min_dorsal_lead),
           dorsal_phase = dorsal_lead / cycledur,
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
  
  
  # Plot: body angle of each tail beats
  fig3 <- datacycles_summary %>% 
    plot_ly(x = ~start, y = ~mean_bodyangle, type = "bar",
            error_y = ~list(array = sd_bodyangle, color = '#000000'),
            xaxis = "Tail Beat Cycle",
            yaxis = "Mean Body Angles(degree)") %>%
    add_lines(data = datacycles, x = ~frame/fps, y = ~body_angle_smooth,
              inherit = FALSE)
  print(fig3)
  
  if (!askYesNo('Next?')) {
    next
  }
  
  # Plot: amplitude of each tail beats
  datacycles_summary <- datacycles_summary %>% 
    mutate(cycle_dorsal_amp = lead(dorsal_amp) - dorsal_amp,
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
  
  fig5 <- datacycles_summary %>% 
    plot_ly(x = ~cyclenum, y = ~phase_diff, type = "bar", 
            yaxis = "Phase Difference")
  print(fig5)
  
  if (!askYesNo('Data OK?')) {
    next
  }
  
  alldata[[i]] <- datacycles_summary
  
  # Save dataset as csv. file
  
  # Save figures?
}

alldata <- bind_rows(alldata)
write_csv(alldata, 'alldata.csv')
