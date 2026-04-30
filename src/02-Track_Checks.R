# Libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(purrr)
library(xlsx)
library(tictoc)
library(ggplot2)
library(sf)


# Directories -------------------------------------------------------------


dir.tracks <- "Output_Maps/Track_Checks/"
dir.create(dir.tracks)


# Location Class Time Series ----------------------------------------------

dat <- read.csv("Processed_data/Filtered_Argos_SSH_silky_locations.csv") %>% 
  mutate(
    dawn_dusk = parse_date_time(dawn_dusk, orders = c("ymd HMS", "ymd"), tz = "UTC")
  ) %>% 
  arrange(PTT, Location.date)

l_species <- c("SHH", "silky")

sat_meta <- read.csv("Metadata/sat_meta.csv")


for (i in 1:length(l_species)){
  this.species <- l_species[i]
  
  dat.tmln <- dat %>% 
    dplyr::filter(species == this.species) %>%
    mutate(
      ref_date  = as.Date(Location.date),
      ref_year = year(ref_date),
      season = ifelse(month(ref_date) %in% c(12, 1:5), "Wet", "Dry")) %>% 
    group_by(PTT) %>% 
    mutate(
      yr_diff = as.character(ref_year - min(ref_year) + 2000),
      tmln_date = as.Date(
        paste0(yr_diff, "-", format(ref_date, "%m-%d")))
    ) %>% 
    ungroup() %>% 
    dplyr::select(PTT, season, contains("date"), yr_diff, species, Location.class, dawn_dusk) %>% 
    filter(!is.na(tmln_date)) %>% 
    arrange(PTT, dawn_dusk)
  
  
  tic()
  dat_plot <- dat.tmln %>% 
    mutate(
      Location.class = factor(Location.class, levels = rev(c("3","2","1","0","A","B")))
    )# %>% 
  # filter(PTT %in% unique(dat$PTT)[1:10])
  
  
  base_plot <- ggplot(
    dat_plot,
    aes(x = dawn_dusk, y = Location.class, color = Location.class)
  ) +
    geom_point() +
    scale_color_viridis_d() +
    theme_bw()
  
  n_per_page <- 5
  n_pages <- ceiling(length(unique(dat_plot$PTT)) / n_per_page)
  
  pdf(file = paste0(dir.tracks, paste0("Location_class-",this.species,".pdf")),
      width = 11, height = 8.5, onefile = TRUE)
  
  for (i in seq_len(n_pages)) {
    p <- base_plot +
      ggforce::facet_wrap_paginate(~PTT, ncol = 1, nrow = n_per_page, scales = "free", page = i)
    print(p)
  }
  
  dev.off()
  toc() # 136 sec for SHH
  
}

## Best loc per day ----

for (i in 1:length(l_species)){
  this.species <- l_species[i]
  tic()
  
  dat_qc <- dat %>%
    dplyr::filter(species == this.species) %>%
    mutate(
      ref_date = as.Date(Location.date),
      Location.class = factor(
        Location.class,
        levels = c("B", "A", "0", "1", "2", "3"),
        ordered = TRUE
      )
    )
  
  best_lc_per_day <- dat_qc %>%
    group_by(PTT, ref_date) %>%
    slice_max(Location.class, n = 1, with_ties = FALSE) %>%
    ungroup() %>% 
    arrange(PTT, ref_date)
  
  
  
  base_plot <- ggplot(
    best_lc_per_day,
    aes(x = ref_date, y = Location.class, color = Location.class)
  ) +
    geom_point() +
    scale_color_viridis_d() +
    theme_bw()
  
  n_per_page <- 5
  n_pages <- ceiling(length(unique(dat_plot$PTT)) / n_per_page)
  
  pdf(file = paste0(dir.tracks, paste0("Location_class-Best_loc_per_day-",this.species,".pdf")),
      width = 11, height = 8.5, onefile = TRUE)
  
  for (i in seq_len(n_pages)) {
    p <- base_plot +
      ggforce::facet_wrap_paginate(~PTT, ncol = 1, nrow = n_per_page, scales = "free", page = i)
    print(p)
  }
  
  dev.off()
  toc() # 136 sec
  
}

## Best loc per 12hr Dawn/Dusk ----


for (i in 1:length(l_species)){
  this.species <- l_species[i]
  
  tic()
  
  dat_qc <- dat %>%
    dplyr::filter(species == this.species) %>%
    mutate(
      ref_date = as.Date(Location.date),
      Location.class = factor(
        Location.class,
        levels = c("B", "A", "0", "1", "2", "3"),
        ordered = TRUE
      )
    )
  
  best_lc_per_12hr <- dat_qc %>%
    group_by(PTT, dawn_dusk) %>%
    slice_max(Location.class, n = 1, with_ties = FALSE) %>%
    ungroup() %>% 
    arrange(PTT, dawn_dusk)
  
  
  base_plot <- ggplot(
    best_lc_per_12hr,
    aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
    geom_point() +
    scale_color_viridis_d() +
    theme_bw()
  
  n_per_page <- 5
  n_pages <- ceiling(length(unique(best_lc_per_12hr$PTT)) / n_per_page)
  
  pdf(file = paste0(dir.tracks, paste0("Location_class-Best_loc_per_12hr-",this.species,".pdf")),
      width = 11, height = 8.5, onefile = TRUE)
  
  for (j in seq_len(n_pages)) {
    p <- base_plot +
      ggforce::facet_wrap_paginate(~PTT, ncol = 1, nrow = n_per_page, scales = "free", page = j)
    print(p)
  }
  
  dev.off()
  toc() # 125 sec
}





## Track Plots to compare ----

i <- 1
for (i in 1:length(l_species)){
  this.species <- l_species[i]
  
  tic()
  
  # ptt.short <- unique(dat_qc$PTT)[1:10]
  
  dat_qc <- dat %>%
    dplyr::filter(species == this.species) %>%
    mutate(
      ref_date = as.Date(Location.date),
      Location.class = factor(
        Location.class,
        levels = c("B", "A", "0", "1", "2", "3"),
        ordered = TRUE
      )
    ) %>% 
    arrange(PTT, dawn_dusk)
  
  best_lc_per_12hr <- dat_qc %>%
    # dplyr::filter(PTT %in% ptt.short) %>% 
    group_by(PTT, dawn_dusk) %>%
    slice_max(Location.class, n = 1, with_ties = FALSE) %>%
    ungroup() %>% 
    arrange(PTT, dawn_dusk)
  
  base_plot <- ggplot() +
    geom_sf(data = world, fill = "grey90", color = "grey60") +
    geom_path(
      data = best_lc_per_12hr,
      aes(x = Longitude, y = Latitude, group = PTT),
      linewidth = 0.25,
      color = "black"
    ) +
    geom_point(
      data = best_lc_per_12hr,
      aes(x = Longitude, y = Latitude, color = Location.class),
      size = 0.5, alpha = 0.8
    ) +
    scale_color_viridis_d() +
    theme_bw() +
    coord_sf(xlim = c(min(best_lc_per_12hr$Longitude) - 1, max(best_lc_per_12hr$Longitude) + 1),
             ylim = c(min(best_lc_per_12hr$Latitude) - 1, max(best_lc_per_12hr$Latitude) + 1))
  
  n_per_page <- 3
  n_pages <- ceiling(length(unique(best_lc_per_12hr$PTT)) / n_per_page)
  
  pdf(file = paste0(dir.tracks, paste0("Location_class-Tracks-Best_loc_per_12hr-",this.species,".pdf")),
      width = 8.5, height = 11, onefile = TRUE)

  for (j in seq_len(n_pages)) {
    p <- base_plot +
      ggforce::facet_wrap_paginate(~PTT, ncol = 1, nrow = n_per_page, page = j)
    print(p)
  }

  dev.off()
  toc() # 125 sec
}
dev.off()


