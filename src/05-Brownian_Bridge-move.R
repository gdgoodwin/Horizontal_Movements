## 05-Brownian_bridge-move.R

keep <- c()
rm(list = setdiff(ls(), keep))


library(dplyr)
library(move)
library(terra)
library(sf)
library(sp)
source("src/00-Movement_Functions.R")

## Load / prep tracking data ----


ssm_best_mod <- read.csv("SSM_Results/SSM_dat_Best_Mod.csv", stringsAsFactors = FALSE) %>%
  mutate(
    id = as.character(id),
    date = pdt(date),             
    species = factor(species, levels = c("silky", "SHH"))
  ) %>%
  arrange(id, date)

# Compute dt and sigma_rms
dat_bb2 <- ssm_best_mod %>%
  group_by(id) %>%
  mutate(dt_h = as.numeric(difftime(date, dplyr::lag(date), units = "hours"))) %>%
  ungroup() %>%
  mutate(
    sigma_rms = sqrt((x.se^2 + y.se^2) / 2)
  ) %>% 
  arrange(species, id) 

dat_bb2 %>% group_by(species) %>% distinct(id) %>% summarise(n = n())

# Clamp sigma_rms: floor + 99th percentile cap (global across all points)
sig_floor_km <- 0.5
sig_hi_prob  <- 0.99
sig_hi <- stats::quantile(dat_bb2$sigma_rms, sig_hi_prob, na.rm = TRUE)

dat_bb2 <- dat_bb2 %>%
  dplyr::mutate(
    sigma_rms = pmin(pmax(sigma_rms, sig_floor_km), as.numeric(sig_hi))
  ) %>% 
  dplyr::filter(!is.na(sigma_rms))

# -------------------------------
# 1) Load MPAs (example)
# -------------------------------
# Replace with your actual MPA file(s).
# IMPORTANT: must be polygons, and should be in SAME CRS as x/y tracks & UD rasters.
# Example:
mpas_sf <- st_read("Map_layers/ETP-MPAs_and_EEZs.shp", quiet = TRUE)

# st_crs(mpas_sf)
# -------------------------------
# 2) Settings for BBMM + raster template
# -------------------------------

res_km <- 5
# buf_km <- 350

ptt_counts <- dat_bb2 %>%
  group_by(species, id) %>%
  summarise(n_days = n())
# 
# ggplot(ptt_counts, aes(x = n_days, fill = species)) +
#   geom_density(alpha = 0.5) +
#   theme_bw() +
#   labs(x = "Gap length (days)",
#        y = "Density",
#        fill = "Species",
#        title = "Density of tracking gaps > 10 days")

window.size <- 15
margin      <- 5



# Provide a CRS string if you want to force one (must match x/y).
# If your x/y are in a projected CRS already and you don't need to set it, leave NA.
proj4 <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"

# -------------------------------
# 3) Run BBMM per id (non-segmented)
# -------------------------------

mod_ids <- unique(dat_bb2$id)
# mod_ids <- unique(dat_bb2$id)[26:28]
# mod_ids <- sort(unique(dat_bb2$id))

i <- 115
## Timing
start_total <- Sys.time()

## Build raster template for all individuals' extent ----
sigmax <- max(dat_bb2$sigma_rms, na.rm = TRUE)
buf_km_i <- max(350, 15 * sigmax)   # 15× is conservative; for 33 km => ~500 km
ext_km_i <- max(500, 15 * sigmax)   # match scale

xmin <- min(dat_bb2$x, na.rm = TRUE) - buf_km_i
xmax <- max(dat_bb2$x, na.rm = TRUE) + buf_km_i
ymin <- min(dat_bb2$y, na.rm = TRUE) - buf_km_i
ymax <- max(dat_bb2$y, na.rm = TRUE) + buf_km_i

r0 <- raster::raster(
  xmn = xmin, xmx = xmax,
  ymn = ymin, ymx = ymax,
  res = res_km
)
raster::crs(r0) <- sp::CRS(proj4)


## BBMM loop ----
# results <- lapply(ids, function(one_id) {
results_move <- lapply(seq_along(mod_ids), function(i) {
  
  one_id <- mod_ids[i]
  
  
  start_id <- Sys.time()
  
  df <- dat_bb2 %>%
    dplyr::filter(id == one_id) %>%
    dplyr::arrange(date)
  
  this_species <- df$species[1]
  
  cat(sprintf("Processing %s %s (%d/%d)...\n",
              this_species, one_id, i, length(mod_ids)))

  # # ---- build raster template for this individual's extent ----
  # sigmax <- max(df$sigma_rms, na.rm = TRUE)
  # buf_km_i <- max(350, 15 * sigmax)   # 15× is conservative; for 33 km => ~500 km
  # ext_km_i <- max(500, 15 * sigmax)   # match scale
  # 
  # xmin <- min(df$x, na.rm = TRUE) - buf_km_i
  # xmax <- max(df$x, na.rm = TRUE) + buf_km_i
  # ymin <- min(df$y, na.rm = TRUE) - buf_km_i
  # ymax <- max(df$y, na.rm = TRUE) + buf_km_i
  # 
  # r0 <- raster::raster(
  #   xmn = xmin, xmx = xmax,
  #   ymn = ymin, ymx = ymax,
  #   res = res_km
  # )
  # raster::crs(r0) <- sp::CRS(proj4)
  
  # ---- build move object ----
  mv <- move::move(
    x = df$x,
    y = df$y,
    time = df$date,
    data = df,
    proj = sp::CRS(proj4)
  )
  
  mv_burst <- thinTrackTime(
    x = mv,
    interval = as.difftime(5, units = "days"),
    tolerance = as.difftime(5, units = "days")
  )
  # plot(mv_burst)

  # ---- run dBBMM (brownian.bridge.dyn) ----
  # location.error must be in same units as x/y (km here)
  ud_bb <- move::brownian.bridge.dyn(
    object = mv_burst,
    location.error = "sigma_rms",
    window.size = window.size,
    margin = margin,
    raster = r0,
    burstType = "selected"
    # ext = ext_km_i
    # ext = .5
  )
  # plot(ud_bb)
  # ud_r <- if(any(mv_burst@burstId == "notSelected")){
  ud_r <- if(nlayers(ud_bb) > 1){
    raster::calc(raster::stack(ud_bb@layers), sum, na.rm = TRUE)
  } else {
    ud_bb
  }
  
  # plot(ud_r)
  # ud_combined <- raster::calc(
  #   raster::stack(ud_bb@layers),
  #   fun = sum,
  #   na.rm = TRUE
  # )
  # 
  # raster::cellStats(ud_combined, sum)
  # 
  # cellStats(ud_bb, sum) 
  # ud_stk <- UDStack(ud_bb) 
  # cellStats(ud_stk, sum)

  # plot(ud_r)
  
  # convert to terra for downstream
  ud <- terra::rast(ud_r)
  
  # ---- normalize UD so sum = 1 ----
  v <- terra::values(ud, mat = FALSE)
  s <- sum(v, na.rm = TRUE)
  ud_n <- ud / s
  
  ## Isopleth extraction (50% + 95%) ----
  # threshold defined by cumulative probability of sorted cell values
  vv <- terra::values(ud_n, mat = FALSE)
  o <- order(vv, decreasing = TRUE, na.last = NA)
  cumv <- cumsum(vv[o])
  
  # thresholds
  thr50 <- vv[o][which(cumv >= 0.50)[1]]
  thr95 <- vv[o][which(cumv >= 0.95)[1]]
  
  # boolean rasters
  m50 <- ud_n >= thr50
  m95 <- ud_n >= thr95
  
  # polygons (dissolve)
  p50 <- terra::as.polygons(m50, dissolve = TRUE)
  p95 <- terra::as.polygons(m95, dissolve = TRUE)
  
  # keep TRUE class
  p50 <- p50[terra::values(p50)[,1] == 1, ]
  p95 <- p95[terra::values(p95)[,1] == 1, ]
  
  p50_sf <- sf::st_as_sf(p50)
  p95_sf <- sf::st_as_sf(p95)
  
  # ---- CRS harmonization with MPAs ----

  
  # ---- MPA overlap + areas in equal-area CRS ----
  ea_crs <- 6933  # meters, equal-area
  
  p50_ea  <- sf::st_transform(p50_sf,  ea_crs)
  p95_ea  <- sf::st_transform(p95_sf,  ea_crs)
  mpa_ea  <- sf::st_transform(mpas_sf, ea_crs)
  
  # union to avoid double-counting overlaps
  mpa_u  <- sf::st_union(mpa_ea)
  ud50_u <- sf::st_union(p50_ea)
  ud95_u <- sf::st_union(p95_ea)
  # plot(ud50_u)
  # areas (m^2)
  a50_m2 <- sf::st_area(ud50_u)
  a95_m2 <- sf::st_area(ud95_u)
  
  a50_in_m2 <- sf::st_area(sf::st_intersection(ud50_u, mpa_u))
  a95_in_m2 <- sf::st_area(sf::st_intersection(ud95_u, mpa_u))
  
  # convert to km^2
  a50_km2    <- as.numeric(a50_m2)    / 1e6
  a95_km2    <- as.numeric(a95_m2)    / 1e6
  a50_in_km2 <- as.numeric(a50_in_m2) / 1e6
  a95_in_km2 <- as.numeric(a95_in_m2) / 1e6
  
  prop50_in_mpa <- a50_in_km2 / a50_km2
  prop95_in_mpa <- a95_in_km2 / a95_km2

  out_summary <- data.frame(
    id = one_id,
    specie = this_species,
    n_locs = nrow(df),
    area50_km2 = a50_km2,
    area95_km2 = a95_km2,
    area50_in_mpa_km2 = if (length(a50_in_km2) == 0) 0 else a50_in_km2,
    area95_in_mpa_km2 = if (length(a95_in_km2) == 0) 0 else a95_in_km2,
    pct50_in_mpa = if (length(prop50_in_mpa) == 0) 0 else prop50_in_mpa * 100,
    pct95_in_mpa = if (length(prop95_in_mpa) == 0) 0 else prop95_in_mpa * 100
  )
  
  # ---- Timing ----
  id_elapsed    <- format_elapsed(start_id)
  total_elapsed <- format_elapsed(start_total)
  
  cat(sprintf("done | This run: %s | Total: %s | Time: %s\n",
              id_elapsed, total_elapsed, hms::round_hms(hms::as_hms(Sys.time()),60)))
  
  list(
    id = one_id,
    species = this_species,
    ud = ud_r,
    ud_n = ud_n,
    p50 = p50_sf,
    p95 = p95_sf,
    summary = out_summary
  )
})

results_move <- Filter(Negate(is.null), results_move)
# results_move

# -------------------------------
# 4) Collect outputs
# -------------------------------

bb_summaries_move <- dplyr::bind_rows(lapply(results_move, `[[`, "summary"))
# bb_summaries_move


out.dir <- "Processed_data/UDs"

results_silky <- purrr::keep(results_move, ~ .x$species == "silky")
results_SHH <- purrr::keep(results_move, ~ .x$species == "SHH")

saveRDS(results_move, file = file.path(out.dir, "Results_Move_BBMM.rds"))
saveRDS(results_silky, file = file.path(out.dir, "Results_Move_BBMM-Silky.rds"))
saveRDS(results_SHH, file = file.path(out.dir, "Results_Move_BBMM-SHH.rds"))

## Silky ----
ud_silky <- raster::stack(lapply(results_silky, function(x) x$ud))

ud_silky_pop_mean <- raster::calc(ud_silky, mean, na.rm = TRUE)
ud_silky_pop_sum <- raster::calc(ud_silky, sum, na.rm = TRUE)


# ud_silky_pop_mean[ud_silky_pop_mean == 0] <- NA
# ud_silky_pop_sum[ud_silky_pop_sum == 0] <- NA

out.dir <- "Processed_data/UDs"

raster::writeRaster(
  ud_silky_pop_mean,
  file.path(out.dir, "UD_silky_Pop-Mean.tif"),
  overwrite = TRUE
)

raster::writeRaster(
  ud_silky_pop_sum,
  file.path(out.dir, "UD_silky_Pop-Sum.tif"),
  overwrite = TRUE
)

## SHH ----
ud_SHH <- raster::stack(lapply(results_SHH, function(x) x$ud))

ud_SHH_pop_mean <- raster::calc(ud_SHH, mean, na.rm = TRUE)
ud_SHH_pop_sum <- raster::calc(ud_SHH, sum, na.rm = TRUE)


# ud_SHH_pop_mean[ud_SHH_pop_mean == 0] <- NA
# ud_SHH_pop_sum[ud_SHH_pop_sum == 0] <- NA


raster::writeRaster(
  ud_SHH_pop_mean,
  file.path(out.dir, "UD_SHH_Pop-Mean.tif"),
  overwrite = TRUE
)

raster::writeRaster(
  ud_SHH_pop_sum,
  file.path(out.dir, "UD_SHH_Pop-Sum.tif"),
  overwrite = TRUE
)


