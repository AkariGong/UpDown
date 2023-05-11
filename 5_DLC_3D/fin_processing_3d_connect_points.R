require(tidyverse)

connect_points_in_frame <- function(df, connect,
                           coords = c('fwd', 'left', 'up'))
{
  df2 <- data.frame(matrix(vector(), 0, 2 + 2*length(coords)))
  colnames(df2) <- c('from', 'to', str_c(coords, '1'), str_c(coords, '2'))
  
  for (i in seq(1, nrow(connect))) {
    a <- connect$from[i]
    b <- connect$to[i]
    df2[i, 'from'] <- a
    df2[i, 'to'] <- b
    
    for (c in coords) {
      ac <- paste(a, c, sep = '_')
      bc <- paste(b, c, sep = '_')
      
      c1 <- paste0(c, '1')
      c2 <- paste0(c, '2')
      
      df2[i, c1] <- df[1, ac]
      df2[i, c2] <- df[1, bc]
    }    
  }
  df2
}

connect_points <- function(df, connect,
                           coords = c('fwd', 'left', 'up'))
{
  df <-
    df %>%
    select(frame, bodypart, one_of(coords))
  
  df <- df %>% pivot_wider(names_from = bodypart, values_from = one_of(coords), names_glue = "{bodypart}_{.value}")
  
  df %>%
    group_by(frame) %>%
    group_modify(~ connect_points_in_frame(.x, connect))
}

smooth_and_interp_point <- function(t, x, k = 7)
{
  xs <- rep_along(x, NA)
  
  good <- !is.na(x)
  i <- which(good)
  a <- i[1]
  b <- i[length(i)]
  
  if (k == 1) {
    xm <- x
  }
  else {
    xm <- runmed(x, k, endrule = 'median', na.action = 'na.omit')
  }
  
  sp <- smooth.spline(t[good], xm[good])
  val <- predict(sp, t[a:b])
  
  xs[a:b] <- val$y
  xs
}

smooth_and_interp <- function(df, t, x, y, z, 
                              k = 7)
{
  df %>%
    group_by(bodypart) %>%
    mutate("{{x}}_s" := smooth_and_interp_point({{t}}, {{x}}, k = k),
           "{{y}}_s" := smooth_and_interp_point({{t}}, {{y}}, k = k),
           "{{z}}_s" := smooth_and_interp_point({{t}}, {{z}}, k = k))
}

find_jump <- function(df, t, x, y, z,
                      bigjump = 0.98,
                      maxjumpdur = 10,
                      key = NA)
{
  if (!is.na(key)) {
    print(str(key))
  }
  
  df <- df %>%
    mutate(dx = ({{x}} - lag({{x}})) / ({{t}} - lag({{t}})),
           dy = ({{y}} - lag({{y}})) / ({{t}} - lag({{t}})),
           dz = ({{z}} - lag({{z}})) / ({{t}} - lag({{t}})),
           jumpmag = sqrt(dx^2 + dy^2 + dz^2))

  qx <- quantile(df$jumpmag, bigjump, na.rm = TRUE)
  
  matchjumps <-
    df %>%
    filter(jumpmag > qx) %>%
    mutate(jumpdir = (dx*lag(dx) + dy*lag(dy) + dz*lag(dz)) / (jumpmag * lag(jumpmag)),
           jumpsize = jumpmag / lag(jumpmag)) %>%
    mutate(isjumpback = (jumpdir < -0.95) & (jumpmag/lag(jumpmag) - 1 < 0.1),
           jumpdur = ({{t}}) - lag({{t}}))
  
  matchjumps$jumpdir <- 0
  
  if (nrow(matchjumps) < 2) {
    df %>%
      mutate(jumpdir = 0,
             isjump = FALSE)
  } else {
    matchedprev <- FALSE
    for (i in seq(2,nrow(matchjumps))) {
      if (!matchedprev & matchjumps$isjumpback[i] & (matchjumps$jumpdur[i] < maxjumpdur)) {
        matchjumps$jumpdir[i-1] <- 1
        matchjumps$jumpdir[i] <- -1
        matchedprev <- TRUE
      }
      else {
        matchedprev <- FALSE
      }
    }
    
    matchjumps <-
      matchjumps %>%
      select({{t}}, jumpdir, isjumpback)
    
    df %>%
      left_join(matchjumps, by = as_label(enquo(t))) %>%
      mutate(jumpdir = replace_na(jumpdir, 0),
             isjump = cumsum(jumpdir))
  }
}
