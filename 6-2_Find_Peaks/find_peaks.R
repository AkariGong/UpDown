# TODO
# make 'cyclenum' in peak_df as float showing one decimal place
# Make the plot look better. Maybe use plotly again?

library(pracma)
library(tidyverse)
# library(plotly)
library(readr)
# library(RColorBrewer)
library(here)
library(conflicted)
# library(fs)
library(lubridate)
library(ggplot2)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::lead,
  dplyr::select,
  dplyr::mutate,
  dplyr::pull,
  tidyr::fill,
  # plotly::layout,
  .quiet = TRUE
)

# =========================
# 1. Read files
# =========================
# Specify which trial to check
filedate <- "20211014"
fishid <- "ms04"
trialnum <- 1

# Read relevant files
trial_name <- paste0(filedate, "_", fishid, "_trial", sprintf("%02d", trialnum))

tail_df <- read_csv(
  here("Data", "6_Processed3D", "good_data_v2", paste0(trial_name, "_3dtail.csv")),
  show_col_types = FALSE
)

cycles_df <- read_csv(
  here("Data", "6_Processed3D", "good_data_v2", paste0(trial_name, "_3dtail-cycles.csv")),
  show_col_types = FALSE
)

# summary_df <- read_csv(
#   here("Data", "7_Stats_Analysis", "2022_alldata_v2.csv"),
#   show_col_types = FALSE
# )

# Convert the filedate string to a Date object for filtering
filedate <- ymd(filedate)

# =========================
# 2. Basic settings
# =========================
fps <- 200
frame_col <- "frame"

tail_cols <- c(
  "dorsal_top_p_top",
  "dorsal_middle_p_top",
  "middle",
  "ventral_middle_p_bottom",
  "ventral_bottom_p_bottom"
)

peak_amp_cols <- c(
  "dorsal_amp",
  "dorsalmid_amp",
  "middle_amp",
  "ventralmid_amp",
  "ventral_amp"
)

peak_time_cols <- c(
  "dorsal_lead",
  "dorsalmid_lead",
  "middle_lead",
  "ventralmid_lead",
  "ventral_lead"
)

peak_part_names <- c(
  "dorsal_top_p_top",
  "dorsal_middle_p_top",
  "middle",
  "ventral_middle_p_bottom",
  "ventral_bottom_p_bottom"
)

# =========================
# 3. Tail data
# =========================
tail_df <- tail_df %>%
  mutate(
    time_sec = .data[[frame_col]] / fps
  )

plot_df <- tail_df %>%
  select(all_of(c(frame_col, "time_sec", tail_cols))) %>%
  pivot_longer(
    cols = all_of(tail_cols),
    names_to = "tail_part",
    values_to = "tail_position"
  )

# =========================
# 4. Cycle data
# =========================
cycles_df <- cycles_df %>% 
  select(peaksign, cyclenum, frame, keep_or_not)

cycles_df <- cycles_df %>%
  arrange(frame) %>%
  mutate(
    start_frame = frame,
    end_frame = lead(frame)
  ) %>%
  select(start_frame, end_frame, peaksign, cyclenum, keep_or_not)


# =========================
# 5. Match cycles
# =========================
plot_df <- plot_df %>%
  left_join(
    cycles_df,
    by = join_by(
      frame >= start_frame,
      frame < end_frame
    )
  )

# ===========================
# 6. Find peaks in each cycle
# ===========================
peak_df <- plot_df %>%
  filter(!is.na(cyclenum)) %>%
  group_by(cyclenum, peaksign, tail_part) %>%
  arrange(frame, .by_group = TRUE) %>%
  summarise(
    start_frame = min(frame, na.rm = TRUE),
    end_frame = max(frame, na.rm = TRUE),
    cycle_data = list(pick(everything())),
    .groups = "drop"
  ) %>% 
  mutate(
    peak_result = pmap(
      list(cycle_data, peaksign),
      function(cycle_data, peaksign) {
        
        cycle <- cycle_data$tail_position
        
        if (all(is.na(cycle))) {
          return(tibble(
            peakframe = NA_real_,
            peakheight = NA_real_
          ))
        }
        
        if (peaksign == "up") {
          peaks <- findpeaks(cycle, zero = "+", npeaks = 1)
        } else if (peaksign == "down") {
          peaks <- findpeaks(cycle, zero = "-", npeaks = 1)
        } else {
          peaks <- NULL
        }
        
        if (is.null(peaks) || length(peaks) == 0) {
          tibble(
            peakframe = NA_real_,
            peakheight = NA_real_
          )
        } else {
          peak_index <- peaks[1, 2]
          
          tibble(
            peakframe = cycle_data$frame[peak_index],
            peakheight = peaks[1, 1]
          )
        }
      }
    )
  ) %>%
  select(-cycle_data) %>%
  unnest(peak_result) %>%
  mutate(
    peaktime = peakframe / fps
  ) %>%
  select(
    cyclenum,
    tail_part,
    start_frame,
    end_frame,
    peaksign,
    peakframe,
    peaktime,
    peakheight
  )

# Save data
write_csv(peak_df, here("Data", "6_Processed3D", "good_data_v2", paste0(trial_name, "_peaks.csv")))

# =========================
# 7. Plot peaks
# =========================
p <- ggplot() +
  geom_line(
    data = plot_df,
    aes(x = frame, y = tail_position),
    linewidth = 0.4) +
  geom_point(
    data = peak_df,
    aes(x = peakframe, y = peakheight, color = "red")) +
  labs(x = "Frame", y = "Tail position", color = "Peaks") +
  facet_grid(rows = vars(tail_part))
p

p_path <- '/Volumes/TytellLab$/NewData/UpDown/figures/3dtail_w_peaks'
p_path <- file.path(p_path, paste0(trial_name, "_3dtail_peaks.png"))
ggsave(
  filename = p_path,
  plot = p
)
