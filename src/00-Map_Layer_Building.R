## 06-Map_Building.R


# rm(list = ls()[c(!(ls() %in% c("ssm_best_mod", "etp_mpas")))])

source("src/00-Movement_Functions.R")
library(aniMotum)
library(patchwork)
library(gridGraphics)

tic("Overall")

# Import Data -------------------------------------------------------------

ssm_best_mod <- read.csv("SSM_Results/SSM_dat_Best_Mod.csv", stringsAsFactors = F) %>% 
  mutate(id = as.character(id),
         `date` = pdt(`date`),
         species = factor(species, 
                          levels = c("silky", "SHH"), 
                          labels = c("Silky", "SHH")))

glimpse(ssm_best_mod)


dir.maps <- "Map_layers/"

## Create MPAs_ETP.shp ----

etp_eez_mpas <- st_read("Map_layers/ETP-MPAs_and_EEZs.shp")


# Corralito --------------------------------------------------------------

# c_xmin <- -110
# c_xmax <- -96
# c_ymin <- -3
# c_ymax <- 4
# 
# corralito <- st_as_sfc(
#   st_bbox(
#     c(
#       xmin = c_xmin,
#       xmax = c_xmax,
#       ymin = c_ymin,
#       ymax = c_ymax
#     ),
#     crs = 4326)
#   ) %>%
#   st_sf(name = "IATTC_Corralito")
# 
# st_write(corralito, "Map_layers/IATTC_corralito.shp", delete_layer = T)

corralito <- st_read("Map_layers/IATTC_corralito.shp")


# GEBCO -------------------------------------------------------------------

bathy <- terra::rast("Map_layers/GEBCO/gebco_2025_ETP.tif")

# Tracks to lines ---------------------------------------------------------

sat_meta <- read.csv("Metadata/sat_meta.csv", stringsAsFactors = F)

dir.create("Map_layers/Shark_tracks")

ssm_best_mod <- read.csv("SSM_Results/SSM_dat_Best_Mod.csv", stringsAsFactors = FALSE) %>%
  mutate(
    id = as.character(id),
    date = pdt(date),             
    species = factor(species, levels = c("silky", "SHH"))
  ) %>%
  dplyr::filter(!(id %in% bad_ptts)) %>%
  arrange(id, date)

## SHH ----

locs_shh <- ssm_best_mod %>% 
  mutate(id_year = paste(id,year(date), sep = "-")) %>% 
  dplyr::filter(species == "SHH") %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

trk_lines <- locs_shh %>%
  # dplyr::group_by(id) %>%
  dplyr::group_by(id_year) %>%
  dplyr::summarise(do_union = FALSE, .groups = "drop") %>%
  sf::st_cast("LINESTRING")

sf::st_write(
  trk_lines,
  "Map_layers/Shark_tracks/SHH_tracks.shp",
  delete_layer = TRUE
)

for(m in c(1:12)){
  
  this_month <- month.abb[m]
  mstr <- paste0(sprintf("%02d", m),"-",this_month)
  
  
  locs_shh <- ssm_best_mod %>% 
    mutate(id_year = paste(id,year(date), sep = "-")) %>% 
    dplyr::filter(species == "SHH", month(date) == m) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  trk_lines <- locs_shh %>%
    # dplyr::group_by(id) %>%
    dplyr::group_by(id_year) %>%
    dplyr::summarise(do_union = FALSE, .groups = "drop") %>%
    sf::st_cast("LINESTRING")
  
  sf::st_write(
    trk_lines,
    paste0("Map_layers/Shark_tracks/SHH_tracks-",mstr,".shp"),
    delete_layer = TRUE
  )
  
}



## Silky ----

tic()

locs_silky <- ssm_best_mod %>% 
  mutate(id_year = paste(id,year(date), sep = "-")) %>% 
  dplyr::filter(species == "silky") %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

trk_lines <- locs_silky %>%
  # dplyr::group_by(id) %>%
  dplyr::group_by(id_year) %>%
  dplyr::summarise(do_union = FALSE, .groups = "drop") %>%
  sf::st_cast("LINESTRING")

sf::st_write(
  trk_lines,
  "Map_layers/Shark_tracks/silky_tracks.shp",
  delete_layer = TRUE
)

for(m in c(1:12)){
  
  this_month <- month.abb[m]
  mstr <- paste0(sprintf("%02d", m),"-",this_month)
  
  
  locs_silky <- ssm_best_mod %>% 
    mutate(id_year = paste(id,year(date), sep = "-")) %>% 
    dplyr::filter(species == "silky", month(date) == m) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  trk_lines <- locs_silky %>%
    # dplyr::group_by(id) %>%
    dplyr::group_by(id_year) %>%
    dplyr::summarise(do_union = FALSE, .groups = "drop") %>%
    sf::st_cast("LINESTRING")
  
  sf::st_write(
    trk_lines,
    paste0("Map_layers/Shark_tracks/silky_tracks-",mstr,".shp"),
    delete_layer = TRUE
  )
  
}

toc()