# Libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(purrr)
library(xlsx)
library(tictoc)
library(ggplot2)
library(sf)
source("src/00-Movement_Functions.R")


# Directories -------------------------------------------------------------




# Data --------------------------------------------------------------------


dat <- read.csv("Processed_data/Filtered_Argos_SSH_silky_locations.csv") %>% 
  mutate(
    PTT = as.character(PTT),
    dawn_dusk = parse_date_time(dawn_dusk, orders = c("ymd HMS", "ymd"), tz = "UTC")
  ) 

l_species <- c("SHH", "silky")

sat_meta <- read.csv("Metadata/sat_meta.csv")

dat2 <- dat %>% 
  mutate(
    Location.date = pdt(Location.date),
    ref_date = as.Date(Location.date),
    Location.class = factor(
      Location.class,
      levels = c("B", "A", "0", "1", "2", "3"),
      ordered = TRUE
    )
  ) %>% 
  arrange(PTT, Location.date)

dat_qc <- dat %>%
  # dplyr::filter(species == this.species) %>%
  mutate(
    ref_date = as.Date(Location.date),
    PTT = as.character(PTT),
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

# dat_trim <- read.csv("Processed_data/Cleaned_Argos_SSH_silky_tracks-ND-Date_Trimmed.csv",
dat_trim <- read.csv("Processed_data/Filtered_Argos_SSH_silky_locations.csv",
                     stringsAsFactors = F)

dat_trim <- dat_trim %>% 
  mutate(PTT = as.character(PTT))

dat_tc <- dat_trim %>% 
  mutate(
    across(c(contains("date"), dawn_dusk), pdt),
    ref_date = as.Date(Location.date),
    PTT = as.character(PTT),
    Location.class = factor(
      Location.class,
      levels = c("B", "A", "0", "1", "2", "3"),
      ordered = TRUE
    )
  )

dat_tc_first_20 <- dat_trim %>% 
  mutate(
    across(c(contains("date"), dawn_dusk), pdt),
    ref_date = as.Date(Location.date),
    PTT = as.character(PTT),
    Location.class = factor(
      Location.class,
      levels = c("B", "A", "0", "1", "2", "3"),
      ordered = TRUE
    )
  ) %>% 
  group_by(PTT) %>% 
  slice(1:20) %>% 
  ungroup()


  
# Track Check -------------------------------------------------------------


# this.ptt <- 210138; dat.chk <- best_lc_per_12hr %>% dplyr::filter(PTT == this.ptt); ts.track.chk(this.ptt, dat.chk, sat_meta)

## Time series and track for all filtered locs
this.ptt <- 175256; ts.track.chk.all(this.ptt, dat2, sat_meta)

## Time series and track for dawn/dusk filtered locs
this.ptt <- 215287; ts.track.chk.dd(this.ptt, dat2, sat_meta)

## Time series and track for first 20 filtered locs
this.ptt <- 175256; ts.track.chk.all(this.ptt, dat_tc_first_20, sat_meta)



