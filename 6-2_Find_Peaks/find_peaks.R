## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Description: This script is used to detect the peaks in each tailbeat
## cycle. It takes the 3D tail points and the previously defined cycles as
## input and save the auto-detected peaks as output.
## Author: Ming Gong
## Date: 2026-06-09
## Email: ming.gong@tufts.edu
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# load packages -----------------------------------------------------------

library(conflicted)
library(here)
library(pracma)
library(tidyverse)
library(plotly)
library(readr)
library(RColorBrewer)
library(lubridate)
library(htmlwidgets)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::lead,
  dplyr::select,
  dplyr::mutate,
  dplyr::pull,
  tidyr::fill,
  plotly::layout,
  .quiet = TRUE
)


# 1. Read files -----------------------------------------------------------

# Required variables from the calling script:
# filedate
# fishid
# trialnum

if (!exists("filedate")) stop("filedate is missing.")
if (!exists("fishid")) stop("fishid is missing.")
if (!exists("trialnum")) stop("trialnum is missing.")

trial_name <- paste0(filedate, "_", fishid, "_trial", sprintf("%02d", trialnum))

tail_df <- read_csv(
  here("Data", "6_Processed3D", "good_data_v2", "3dtail",
       paste0(trial_name, "_3dtail.csv")),
  show_col_types = FALSE
)

cycles_df <- read_csv(
  here("Data", "6_Processed3D", "good_data_v2", "cycles",
       paste0(trial_name, "_3dtail-cycles.csv")),
  show_col_types = FALSE
)

# Convert the filedate string to a Date object for filtering
filedate <- ymd(filedate)


# 2. Basic parameters -----------------------------------------------------

fps <- 200
frame_col <- "frame"

tail_cols <- c(
  "dorsal_top_p_top",
  "dorsal_middle_p_top",
  "middle",
  "ventral_middle_p_bottom",
  "ventral_bottom_p_bottom"
)


# 3. Tail data ------------------------------------------------------------

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


# 4. Cycle data -----------------------------------------------------------

cycles_df <- cycles_df %>% 
  select(peaksign, cyclenum, frame, keep_or_not)

cycles_df <- cycles_df %>%
  arrange(frame) %>%
  mutate(
    start_frame = frame,
    end_frame = lead(frame)
  ) %>%
  select(start_frame, end_frame, peaksign, cyclenum, keep_or_not)

max_frame <- max(tail_df$frame)

cycles_df$end_frame[nrow(cycles_df)] <- max_frame


# 5. Match cycles ---------------------------------------------------------

plot_df <- plot_df %>%
  left_join(
    cycles_df,
    by = join_by(
      frame >= start_frame,
      frame < end_frame
    )
  )


# 6. Find peaks in each cycle ---------------------------------------------

find_cycle_peak <- function(cycle_data, peaksign) {
  
  cycle <- cycle_data$tail_position
  
  if (all(is.na(cycle))) {
    return(tibble(
      peakframe = NA_real_,
      peakheight = NA_real_
    ))
  }
  
  if (peaksign == "up") {
    
    peaks <- findpeaks(cycle, ndowns = 7, npeaks = 1)
    
    if (is.null(peaks) || length(peaks) == 0) {
      return(tibble(
        peakframe = NA_real_,
        peakheight = NA_real_
      ))
    }
    
    peak_index <- peaks[1, 2]
    peakheight <- peaks[1, 1]
    
  } else if (peaksign == "down") {
    
    peaks <- findpeaks(-cycle, ndowns = 7, npeaks = 1)
    
    if (is.null(peaks) || length(peaks) == 0) {
      return(tibble(
        peakframe = NA_real_,
        peakheight = NA_real_
      ))
    }
    
    peak_index <- peaks[1, 2]
    peakheight <- -peaks[1, 1]
    
  } else {
    
    return(tibble(
      peakframe = NA_real_,
      peakheight = NA_real_
    ))
  }
  
  tibble(
    peakframe = cycle_data$frame[peak_index],
    peakheight = peakheight
  )
}

peak_df <- plot_df %>%
  filter(!is.na(cyclenum)) %>%
  group_by(cyclenum, peaksign, tail_part) %>%
  arrange(frame, .by_group = TRUE) %>%
  summarise(
    start_frame = min(frame, na.rm = TRUE),
    end_frame = max(frame, na.rm = TRUE),
    cycle_data = list(cur_data()),
    .groups = "drop"
  ) %>%
  mutate(
    peak_result = map2(
      cycle_data,
      peaksign,
      find_cycle_peak
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
write_csv(peak_df, here("Data", "6_Processed3D", "good_data_v2", "peaks",
                        paste0(trial_name, "_peaks_auto.csv")))


# 7. Colors -----------------------------------------------------------

n_cycles <- nrow(cycles_df)

if (n_cycles <= 12) {
  cycle_colors <- brewer.pal(n_cycles, "Set3")[1:n_cycles]
} else {
  cycle_colors <- colorRampPalette(brewer.pal(12, "Set3"))(n_cycles)
}

cycle_color_df <- tibble(
  cyclenum = cycles_df$cyclenum,
  fillcolor = cycle_colors
)

cycles_df <- cycles_df %>%
  left_join(cycle_color_df, by = "cyclenum")

peak_df <- peak_df %>%
  left_join(cycle_color_df, by = "cyclenum")

# 8. Build shape lists ----------------------------------------------------

rect_shapes <- lapply(seq_len(n_cycles), function(i) {
  list(
    type = "rect",
    xref = "x",
    yref = "paper",
    x0 = cycles_df$start_frame[i],
    x1 = cycles_df$end_frame[i],
    y0 = 0,
    y1 = 1,
    fillcolor = cycles_df$fillcolor[i],
    opacity = 0.18,
    line = list(width = 0),
    layer = "below"
  )
})

line_shapes <- lapply(seq_len(n_cycles), function(i) {
  list(
    type = "line",
    xref = "x",
    yref = "paper",
    x0 = cycles_df$start_frame[i],
    x1 = cycles_df$start_frame[i],
    y0 = 0,
    y1 = 1,
    line = list(color = "red", width = 1, dash = "dot"),
    layer = "above"
  )
})

shapes_list <- c(rect_shapes, line_shapes)


# 9. Annotations ----------------------------------------------------------

annotations_list <- lapply(seq_len(n_cycles), function(i) {
  list(
    x = (cycles_df$start_frame[i]+cycles_df$end_frame[i]) / 2,
    y = 1.02,
    xref = "x",
    yref = "paper",
    text = paste0("Cycle ", cycles_df$cyclenum[i]),
    showarrow = FALSE,
    font = list(size = 10)
  )
})


# 10. Plot ----------------------------------------------------------------

p1 <- plot_ly()

# tail traces
for (col_name in tail_cols) {
  df_sub <- plot_df %>% filter(tail_part == col_name)
  
  p1 <- p1 %>%
    add_lines(
      data = df_sub,
      x = ~frame,
      y = ~tail_position,
      name = col_name,
      line = list(width = 1.2),
      hovertemplate = paste0(
        "Tail part: ", col_name,
        "<br>Frame: %{x}",
        "<br>Position: %{y}",
        "<extra></extra>"
      )
    )
}

# peak/trough markers
for (part_name in unique(peak_df$tail_part)) {
  peak_sub <- peak_df %>% filter(tail_part == part_name)
  
  p1 <- p1 %>%
    add_markers(
      data = peak_sub,
      x = ~peakframe,
      y = ~peakheight,
      name = paste0(part_name, "_peak"),
      marker = list(
        size = 7,
        symbol = "circle",
        line = list(width = 0.5, color = "black")
      ),
      hovertemplate = paste0(
        "Tail part: ", part_name,
        "<br>Frame: %{x:.1f}",
        "<br>Amplitude: %{y}",
        "<extra></extra>"
      ),
      showlegend = TRUE
    )
}

p1 <- p1 %>%
  layout(
    title = paste0(filedate, "_", fishid, "_", trialnum),
    xaxis = list(title = "Frame"),
    yaxis = list(title = "Tail position"),
    hovermode = "x unified",
    shapes = shapes_list,
    annotations = annotations_list
  )

# Save plot as self-contained HTML
save_dir <- '/Volumes/TytellLab$/NewData/UpDown/figures/3dtail_w_re-detected_peaks'
p_html <- file.path(save_dir, paste0(trial_name, ".html"))

# create folder if it does not exist
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

saveWidget(
  widget = p1,
  file = p_html,
  selfcontained = FALSE
)

p1

