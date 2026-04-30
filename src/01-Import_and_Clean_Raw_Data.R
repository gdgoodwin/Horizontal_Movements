## 01-Import_and_Clean_Raw_Data.R
## April 2026
## GDG



# Notes -------------------------------------------------------------------

## Argos data acquired through Brad Weatherbee 


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

# if(!grepl("Silky", getwd())){
#   setwd("../Silky_hammers")
# }

## Input ----

## Argos CD data
# dir.argos <- "Raw_data/ArgosCDs"
# dir.raw.miss <- "Raw_data/Missing_Silky_Data-Pelayo"
dir.meta <- "Metadata/"
dir.maps <- "Map_layers/"



# Import Raw Data ---------------------------------------------------------

## Meta.fn

fn.meta <- paste0(dir.meta,"shh_silky_ptt_list.xlsx")

# fns.cds <- dir(dir.argos, full.names = T, recursive = T, pattern = ".csv")
# fns.miss <- dir(dir.raw.miss, full.names = T, recursive = T, pattern = ".csv")

ptt.list <- read.xlsx(fn.meta, sheetIndex = 1, stringsAsFactors = F) %>% 
  mutate(across(everything(), as.character))



## Import All Argos tracks
# tracks.argosCDs <- lapply(fns.cds, readr::read_csv, 
#                        name_repair = make.names,
#                        col_types = c(PTT = "character"),
#                        col_select = c(1:24)) %>% 
#   purrr::map(
#     .,
#     ~ .x %>% mutate(across(everything(), as.character)) %>% 
#       as.data.frame()
#   ) %>% 
#   dplyr::bind_rows()
# 
# tracks.argos <- lapply(fns.miss, readr::read_csv, 
#                       name_repair = make.names) %>% 
#   purrr::map(
#     .,
#     ~ .x %>% mutate(across(everything(), as.character)) %>%
#       as.data.frame()
#   ) %>%
#   dplyr::bind_rows(tracks.argosCDs, .) %>% 
#   dplyr::select(-ref_date) %>% 
#   arrange(PTT, Location.date)


tracks.argos <- read.csv("Raw_data/argos_subset.csv", stringsAsFactors = F)


## Filter Z locvs, NA loc.date, best point per dttm

tracks.all <- tracks.argos %>% 
  ## Filter PTTs in Jeremy list
  dplyr::filter(!is.na(Location.date),
                Location.class != "Z", 
                !is.na(Location.class)) %>%
  mutate(
    ## Default all column types to character
    across(everything(), as.character),
    ## Set location class to factor for filtering
    Location.class = factor(Location.class, levels = c("3","2","1","0","A","B")),
    ## Parse datetime
    Location.date = pdt(Location.date),
    ## Calculate Ellipse area for chosing best loc
    ellip_area = pi * as.numeric(Semi.major.axis) * as.numeric(Semi.minor.axis)) %>% 
  arrange(PTT, Location.date) %>% 
  mutate(
    ## Determine if sequential locs are within one minute from each other due to mult sats reporting
    ##  This does not work perfectly, specifically if three points happen across ~90 seconds, the mid 
    ##  loc will get rounded up, decoupling it from the first point. Ah well. The aniMotum pre-filter
    ##  chooses one point per minute. This fairly robust filtering alogrithim improves that for the most part.
    ## New col for date time rounded to lowest minute
    dttm_hm = lubridate::floor_date(
      Location.date,
      "minute"),
    ## Checking num of minutes between successive rounded dttms
    dt_min_lead = abs(as.numeric(difftime(dttm_hm, lead(dttm_hm), units = "mins"))),
    dt_min_lag = abs(as.numeric(difftime(dttm_hm, lag(dttm_hm), units = "mins"))),
    ## Setting dttm group. If locs within +/- one minute of each other, rouund to nearest minute group
    dttm_group = case_when(
      dt_min_lead == 1 ~ 
        ceiling_date(
          Location.date,
          "minute"),
      TRUE ~ floor_date(
        Location.date,
        "minute"))) %>% 
  ## Grouping variables
  group_by(PTT, dttm_group) %>%
  ## Filter within each minute group for smallest ellipse, if available
  slice_min(ellip_area) %>% 
  ## Filter for best quality loccation if ellipse unavailable (mostly Pelayo data for 5 missing PTTs for early 2021)
  arrange(Location.class, .by_group = T) %>% 
  ## Filter for just one of best loc quality
  slice_head() %>% 
  ## Arrange for proper data order
  arrange(PTT, Location.date) %>% 
  ungroup() %>% 
  # relocate(dt_min_lead, dt_min_lag, dttm_group, dttm_hm, .after = Satellite)
  ## Get rid of helper variables
  dplyr::select(-c(dt_min_lag, dt_min_lead, dttm_hm, dttm_group, ellip_area))



## Join metatdata to tracks
tracks.all2 <- tracks.all %>% 
  left_join(., ptt.list, by = "PTT", relationship = "many-to-one") %>% 
  mutate(across(contains(c("Latitude","Longitude")), as.numeric))



## Explore Tracking Data
glimpse(tracks.all2)
summary(tracks.all2)

table(tracks.all2$PTT, useNA = "ifany")
table(tracks.all2$Location.class, useNA = "ifany")

ggplot(tracks.all2, aes(x = Location.class)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Argos Location Class Distribution")


# Track Visualization -----------------------------------------------------

world <- rnaturalearth::ne_countries(
  scale = 50, continent = c("North America","South America"),
  returnclass = "sf") %>% 
  st_transform(4326)

ggplot() +
  geom_sf(data = world) +
  geom_path(data = tracks.all2, aes(Longitude, Latitude, group = PTT, color = species), linewidth = 0.25) +
  theme_bw() +
  coord_sf(xlim = c(min(tracks.all2$Longitude) - 1, max(tracks.all2$Longitude) + 1),
           ylim = c(min(tracks.all2$Latitude) - 1, max(tracks.all2$Latitude) + 1))

## Geographic Filters ----

### Getting rid of Washington, inland S America, and Atlantic points ----
tracks.all3 <- tracks.all2 %>% 
  filter(Longitude < -70, Latitude <30)

ggplot() +
  geom_sf(data = world) +
  geom_path(data = tracks.all3, aes(Longitude, Latitude, group = PTT, color = species), linewidth = 0.25) +
  theme_bw() +
  coord_sf(xlim = c(min(tracks.all3$Longitude) - 1, max(tracks.all3$Longitude) + 1),
           ylim = c(min(tracks.all3$Latitude) - 1, max(tracks.all3$Latitude) + 1))

### Land Filter ----

## Import modified rnaturalearth layer. Made in QGIS by removing Galapagos islands
##  from N and S America polygon set.


landfilt.lyr <- st_read(file.path(dir.maps, "landfilt.shp")) %>% 
  st_transform(4326)

tracks.all.sf <- tracks.all3 %>% 
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = F
  )

idx <- st_intersects(tracks.all.sf, landfilt.lyr)

tracks.all.sf$on_land <- lengths(idx) > 0

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = tracks.all.sf, aes(color = on_land)) +
  theme_bw() +
  coord_sf(xlim = c(min(tracks.all.sf$Longitude) - 1, max(tracks.all.sf$Longitude) + 1),
           ylim = c(min(tracks.all.sf$Latitude) - 1, max(tracks.all.sf$Latitude) + 1))

tracks.h2o.sf <- tracks.all.sf %>% 
  dplyr::filter(!on_land)

tracks.all.h2o <- st_drop_geometry(tracks.h2o.sf)



# Dawn/dusk sorting -------------------------------------------------------


tracks.dadu <- tracks.all.h2o %>%
  mutate(
    midnight_prev = floor_date(Location.date, "day"),
    noon          = midnight_prev + hours(12),
    midnight_next = midnight_prev + days(1),
    
    dawn_dusk = case_when(
      abs(Location.date - midnight_prev) <= abs(Location.date - noon) &
        abs(Location.date - midnight_prev) <= abs(Location.date - midnight_next) ~ midnight_prev,
      
      abs(Location.date - noon) <= abs(Location.date - midnight_prev) &
        abs(Location.date - noon) <= abs(Location.date - midnight_next) ~ noon,
      
      TRUE ~ midnight_next
    )
  ) %>%
  rename(deploy_time = `time`) %>% 
  dplyr::select(-midnight_prev, -noon, -midnight_next)


# Deploy Date Filter ------------------------------------------------------


tracks.ddf <- tracks.dadu %>%
  mutate(
    deploy_time = pdt(deploy_time),
    deploy_date = lubridate::floor_date(deploy_time, "day"),

    deploy_time_final = case_when(
      ## 1) deploy_time exists -> deploy_time
      !is.na(deploy_time) ~ deploy_time,

      # 2) no deploy_time, but deploy_date exists -> midnight
      is.na(deploy_time) & !is.na(deploy_date) ~ deploy_date,
      
      TRUE ~ as.POSIXct(NA)
    )
  ) %>%
  dplyr::filter(is.na(deploy_time_final) | Location.date >= deploy_time_final)


# Tracking Counts ---------------------------------------------------------

## Tally tracking summary data, including 30 day threshold
count_ptt_period <- tracks.ddf %>% 
  mutate(
    # dttm = ymd_hms(location_date),
    date_ref = date(Location.date)) %>% 
  group_by(PTT) %>% 
  distinct(date_ref, .keep_all = TRUE) %>% 
  summarise(
    species = first(species),
    start = min(date_ref),
    end   = max(date_ref),
    tracked_period = as.numeric(difftime(end, start, units = "days")),
    deploy_date = first(deploy_date),
    Latitude = first(Latitude),
    Longitude = first(Longitude),
    location = first(location),
    site = first(site),
    lat = first(lat),
    lon = first(lon),
    sex = first(sex),
    TL = first(TL),
    FL = first(FL)
  ) %>% 
  left_join(.,
            tracks.dadu %>% 
              mutate(
                date_ref = date(Location.date)) %>% 
              group_by(PTT) %>% 
              distinct(date_ref)  %>% 
              count(PTT), by = "PTT") %>% 
  rename(tracked_days = n) %>% 
  mutate(thresh_30_day = 
           ifelse(tracked_period < 30, FALSE, TRUE), ## 30 per JV et al. 2026
         tracked_proportion = 
           round(tracked_days/tracked_period * 100, 1),
         tracked_percent = 
           paste0(round(tracked_days/tracked_period * 100, 1),"%")
  ) %>% 
  relocate(tracked_days, tracked_percent, .after = tracked_period)


count_ptt_period_write <- count_ptt_period %>% 
  mutate(across(everything(), as.character),
         across(where(is.character), ~ tidyr::replace_na(.x, "")))

write.csv(count_ptt_period_write, "Processed_data/Filtered_Tracking_Summary.csv", row.names = F)

## Count of sharks cut due to 30 day threshold
count_ptt_period %>% 
  group_by(species, thresh_30_day) %>% 
  summarise(n())

write.csv(count_ptt_period_write, paste0(dir.meta, "sat_meta.csv"), row.names = F)

## Get just 30 day PTT Argos data
ptt.list.short30 <- count_ptt_period %>% 
  dplyr::filter(!thresh_30_day)

## Filter out short PTTs
dat <- tracks.ddf %>% 
  dplyr::filter(!(PTT %in% ptt.list.short30$PTT))



## Write Clean Argos Tracks ----

# write.csv(dat, "Processed_data/Cleaned_Argos_SSH_silky_tracks.csv", row.names = FALSE)
write.csv(dat, "Processed_data/Filtered_Argos_SSH_silky_locations.csv", row.names = FALSE)

