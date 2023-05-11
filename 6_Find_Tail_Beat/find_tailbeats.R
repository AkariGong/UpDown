require(tidyverse)
require(here)
require(plotly)

fps <- 200

tailbeatfreq <- 2  # Hz

bf <- signal::butter(9, 2*tailbeatfreq / (0.5*fps))

## Useful functions

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

get_tail_beats <- function(df, var) {
  d <- pull(df, {{var}})
  dd <- (lead(d) - lag(d)) / 2
  
  pm <- pracma::findpeaks(dd, nups = 2, ndowns = 2,
                          minpeakheight = 0,
                          zero = '+',
                          minpeakdistance = 0.25/tailbeatfreq * fps,
                          sortstr = FALSE)
  
  ups <- as_tibble(pm)
  colnames(ups) <- c('height', 'ipk', 'ipkstart', 'ipkend')
  ups <- ups |> 
    mutate(peaksign = 'up')
  
  pm <- pracma::findpeaks(-dd, nups = 2, ndowns = 2,
                          minpeakheight = 0,
                          zero = '-',
                          minpeakdistance = 0.25/tailbeatfreq * fps,
                          sortstr = FALSE)
  
  downs <- as_tibble(pm)
  colnames(downs) <- c('height', 'ipk', 'ipkstart', 'ipkend')
  downs <- downs |> 
    mutate(height = -height,
           peaksign = 'down')
  
  peaks <- bind_rows(ups, downs) |> 
    arrange(ipk) |> 
    mutate(cyclenum = seq(from=1, by=0.5, length.out=n()),
           peaksign = factor(peaksign))
  
  peaks |> 
    mutate('{{var}}_cyc' := d[peaks$ipk])
}


## Process the data

datafiles <- c('findtailbeat/20220707_ms02_trial03_3dtail.csv') # add files

for (file1 in datafiles) {
  data1 <- read_csv(here(file1))
  
  data1 <- data1 |> 
    mutate(middle_s = filter_span(bf, middle))
  
  fig1 <-
    data1 |> 
    plot_ly(x = ~frame) |> 
    add_lines(y = ~middle, name = 'middle') |> 
    add_lines(y = ~middle_s, name = 'smooth')
  print(fig1)
  
  if (!askYesNo('Is the smoothed data OK?')) {
    break
  }
  
  cycles <-
    data1 |> 
    get_tail_beats(middle_s) |> 
    mutate(frame = data1$frame[ipk])

  a <- min(data1$middle, na.rm = TRUE)
  b <- max(data1$middle, na.rm = TRUE)
  
  fig2 <-
    data1 |> 
    plot_ly() |> 
    add_lines(data = data1, 
              x = ~frame, y = ~middle, name = 'middle') |> 
    add_markers(data = cycles,
                x = ~frame, y = ~middle_s_cyc, color = ~peaksign) |>
    add_lines(data = data1,
              x = ~frame, y = ~dorsal_top_p_top, name = 'dorsal top') |>
    add_lines(data = data1,
              x = ~frame, y = ~ventral_bottom_p_bottom, name = 'ventral bottom')
  
  cyclefile <- tools::file_path_sans_ext(here(file1))
  cyclefile <- paste0(cyclefile, '-cycles.csv')
  write_csv(cycles, cyclefile)
  
  # this will only work on Macs:
  # open the file in Excel
  system2('open', paste0('"', cyclefile, '"'))
  
  print(fig2)
  
  if (!askYesNo('Change the frame column if needed, or delete or add rows, and save the file. Continue?')) {
    break
  }
  
  cycles2 <- read_csv(cyclefile) |> 
    mutate(cyclenum = seq(from=1, by=0.5, length.out=n()),
           peaksign = factor(peaksign)) |> 
    select(frame, peaksign, cyclenum)
  
  data1 <-
    data1 |> 
    left_join(cycles2, by='frame') |> 
    fill(cyclenum)
    
  fig3 <-
    data1 |> 
    mutate(cyclenum = factor(cyclenum)) |> 
    group_by(cyclenum) |> 
    plot_ly() |> 
    add_lines(x = ~frame, y = ~middle_s, color = ~cyclenum, name = 'middle') |> 
    add_markers(data = filter(data1, !is.na(peaksign)),
                x = ~frame, y = ~middle_s, name = 'new tailbeats') |> 
    add_markers(data = cycles,
              x = ~frame, y = ~middle_s_cyc, name = 'old tailbeats') 
    print(fig3)  
}

