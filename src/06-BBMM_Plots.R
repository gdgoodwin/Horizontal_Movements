## 06-BBMM_Plots.R


keep <- c("results_silky","results_SHH")
rm(list = setdiff(ls(), keep))


# Libraries and source ----------------------------------------------------


library(dplyr)
library(move)
library(terra)
library(sf)
library(sp)
source("src/00-Movement_Functions.R")

out.dir <- "Processed_data/UDs"



# Load data ---------------------------------------------------------------


results_silky <- readRDS(file.path(out.dir, "Results_Move_BBMM-Silky.rds"))
results_SHH <- readRDS(file.path(out.dir, "Results_Move_BBMM-SHH.rds"))

## CRS from BBMM polygons ----
crs_bb <- st_crs(results_silky[[1]]$p95)

## MPAs ----
mpas_sf_m <- st_read("Map_layers/ETP-MPAs_and_EEZs.shp", quiet = TRUE) %>% 
  st_transform(crs_bb) %>%
  st_make_valid()

corralito <- st_read("Map_layers/IATTC_corralito.shp", quiet = TRUE) %>% 
  st_transform(crs_bb) %>% 
  st_make_valid()

## Land ----

## From 00-Movement_Functions.R source
land_m <- st_transform(land, crs_bb) %>% 
  st_make_valid()



# Create UD rasters -------------------------------------------------------


## Silky ----
ud_silky <- raster::stack(lapply(results_silky, function(x) x$ud))

ud_silky_pop_mean <- raster::calc(ud_silky, mean, na.rm = TRUE)
ud_silky_pop_sum <- raster::calc(ud_silky, sum, na.rm = TRUE)


ud_silky_sum <- raster::calc(ud_silky, sum, na.rm = TRUE)

ud_silky_sum <- ud_silky_sum / raster::cellStats(ud_silky_sum, sum)
all(ud_silky_sum == ud_silky_pop_mean)

ud_silky_pop_mean[ud_silky_pop_mean == 0] <- NA

## SHH ----
ud_SHH <- raster::stack(lapply(results_SHH, function(x) x$ud))

ud_SHH_pop_mean <- raster::calc(ud_SHH, mean, na.rm = TRUE)
ud_SHH_pop_sum <- raster::calc(ud_SHH, sum, na.rm = TRUE)
# sum(ud_silky_pop_mean)
# ud_silky_pop_n <- 

ud_SHH_pop_mean[ud_SHH_pop_mean == 0] <- NA
ud_SHH_pop_sum[ud_SHH_pop_sum == 0] <- NA


# Quantile Maps -----------------------------------------------------------


## Track cols
# cbcols <- ggthemes::colorblind_pal()(8)
# scales::show_col(cbcols)
# mycols <- cbcols[c(8,3,1)]
# scales::show_col(mycols)
# mycols <- cbcols[c(3,4)]
# mycols3 <- cbcols[c(3,4,8)]


## Silky ----


### Convert raster for plotting ----
ud_df_silky <- raster::as.data.frame(ud_silky_pop_mean, xy = TRUE, na.rm = TRUE)
names(ud_df_silky)[3] <- "ud"

### Split quantiles ----
quantile(ud_df_silky$ud, probs = seq(0, 1, by = 0.1))
quantile(ud_df_silky$ud, probs = seq(0.6, 0.65, by = 0.01))
qbrks <- unique(quantile(ud_df_silky$ud, probs = seq(0, 1, by = 0.1)))
# qbrks <- unique(quantile(ud_df_silky$ud, probs = c(0, 0.65, seq(0.7, 1, 0.1))))
# qbrks <- unique(quantile(ud_df_silky$ud, probs = c(0.65, seq(0.7, 1, 0.1))))

### Quantile colors ----

## UD Cols
vcols <- viridis::viridis(length(qbrks)-1)
# vcols2 <- viridisLite::viridis(9)
scales::show_col(vcols)
# scales::show_col(vcols2)
udcols <- c("transparent", vcols)
# scales::show_col(udcols)

ud_df_silky$qbin <- cut(
  ud_df_silky$ud,
  breaks = qbrks,
  include.lowest = TRUE
)


### Bounding box ----
bb <- sf::st_bbox(ud_silky)
# xpad <- as.numeric(bb["xmax"] - bb["xmin"])
# ypad <- as.numeric(bb["ymax"] - bb["ymin"])

### Silky UD plot ----
p_ud_silky <- ggplot() +
  geom_tile(data = ud_df_silky, aes(x = x, y = y, fill = qbin)) +
  geom_sf(data = land_m, fill = "grey85", color = "grey50", linewidth = 0.2) +
  geom_sf(data = mpas_sf_m,
          # aes(linetype = area_type),
          aes(color = area_type,
              linewidth = area_type),
          # color = "#E64B35",
          fill = "transparent",
          # linewidth = 1,
          # linetype = "dashed"
  ) +
  # scale_fill_manual(values = udcols, name = "UD quantile", na.value = NA) +
  scale_fill_viridis_d(name = "UD quantile", na.value = NA) +
  # scale_fill_viridis_c(name = "UD quantile", na.value = NA) +
  # scale_color_manual(values = c("black","grey50"), name = NULL)+
  scale_color_manual(values = c("black","#D55E00"), name = NULL)+
  scale_linewidth_manual(values = c(0.5,1)) +
  guides(linewidth = "none") +
  geom_sf(data = corralito,
          color = "#D55E00",
          # fill = scales::alpha("#F0E442", 0.2),
          fill = "transparent",
          linewidth = 1.2) +
  theme_bw() +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    # xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    # ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    expand = FALSE
  ) +
  labs(
    title = "Silky (n = 109) BBMM (move) UDs",
    x = NULL,
    y = NULL
  )


# p_ud_silky
ggsave("Output_Maps/Silky_UD.png", p_ud_silky, units = "in", 
       width = 12, height = 6)



## SHH ----

### Convert raster for plotting ----
ud_df_SHH <- raster::as.data.frame(ud_SHH_pop_mean, xy = TRUE, na.rm = TRUE)
names(ud_df_SHH)[3] <- "ud"

qbrks <- unique(quantile(ud_df_SHH$ud, probs = seq(0, 1, by = 0.1)))

ud_df_SHH$qbin <- cut(
  ud_df_SHH$ud,
  breaks = qbrks,
  include.lowest = TRUE
)


bb <- sf::st_bbox(ud_SHH_pop_mean)
# xpad <- as.numeric(bb["xmax"] - bb["xmin"])
# ypad <- as.numeric(bb["ymax"] - bb["ymin"])

p_ud_SHH <- ggplot() +
  geom_tile(data = ud_df_SHH, aes(x = x, y = y, fill = qbin)) +
  geom_sf(data = land_m, fill = "grey85", color = "grey50", linewidth = 0.2) +
  geom_sf(data = mpas_sf_m,
          # aes(linetype = area_type),
          aes(color = area_type,
              linewidth = area_type),
          # color = "#E64B35",
          fill = "transparent",
          # linewidth = 1,
          # linetype = "dashed"
  ) +
  scale_fill_viridis_d(name = "UD quantile", na.value = NA) +
  # scale_color_manual(values = c("black","grey50"), name = NULL)+
  scale_color_manual(values = c("black","#D55E00"), name = NULL)+
  scale_linewidth_manual(values = c(0.5,1)) +
  guides(linewidth = "none") +
  geom_sf(data = corralito,
          color = "#D55E00",
          # fill = scales::alpha("#F0E442", 0.2),
          fill = "transparent",
          linewidth = 1) +
  theme_bw() +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Scalloped Hammerhead (n = 94) BBMM (move) UDs",
    x = NULL,
    y = NULL
  )


# p_ud_SHH
ggsave("Output_Maps/SHH_UD.png", p_ud_SHH, units = "in", 
       width = 12, height = 6)

# col.chk <- ggthemes::canva_palettes$`Industrial and in control`
# scales::show_col(col.chk)
# ggthemes::excel_pal(line = TRUE)
# scales::show_col(ggthemes::excel_pal())
# scales::show_col(ggthemes::continuous_tableau())
# ggthemes::scale_fil


# 50% and 95% UD Maps -----------------------------------------------------


## Silky ----

### Get isopleths ----
silky_95 <- get_ud_isopleth(ud = ud_silky_pop_mean, level = 0.95)
silky_50 <- get_ud_isopleth(ud = ud_silky_pop_mean, level = 0.50)

silky_95 <- silky_95 %>%
  dplyr::mutate(isopleth = "95%")

silky_50 <- silky_50 %>%
  dplyr::mutate(isopleth = "50%")

silky_iso <- dplyr::bind_rows(silky_95, silky_50)

### Make map ----


### Silky UD isopleth plot ----

p_ud_silky <- ggplot() +
  geom_sf(data = silky_iso,
          aes(fill = isopleth, 
              color = isopleth)) +
  scale_fill_manual(
    values = c("95%" = scales::alpha("#56B4E9", 0.35),
               "50%" = scales::alpha("#08306B", 0.55)),
    name = "UD isopleth") +
  scale_colour_manual(
    values = c("95%" = "#56B4E9",
               "50%" = "#08306B")) +
  guides(colour = "none") +
  ggnewscale::new_scale_colour() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = land_m, fill = "grey85", color = "grey50", linewidth = 0.2) +
    geom_sf(data = mpas_sf_m,
          aes(color = area_type,
              linewidth = area_type),
          fill = "transparent",
          inherit.aes = FALSE) +
  scale_color_manual(values = c("black", "#D55E00"), name = NULL) +
  scale_linewidth_manual(values = c(0.5, 1)) +
  guides(linewidth = "none") +
  geom_sf(data = corralito,
          color = "#D55E00",
          fill = "transparent",
          linewidth = 1.2) +
  coord_sf(
    xlim = c(bb_proj["xmin"], bb_proj["xmax"]),
    ylim = c(bb_proj["ymin"], bb_proj["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Silky (n = 109) BBMM (move) 50% and 95% UDs",
    x = NULL,
    y = NULL
  ) +
  theme_bw()  +
  theme(
    legend.position = c(0.001, 0.0015),   # adjust as needed
    legend.justification = c(0, 0),   # anchor legend by bottom-left corner
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_blank(),
  )

# p_ud_silky

ggsave("Processed_data/UDs/Silky_95_and_50_UDs.png", p_ud_silky,
       units = "in", width = 12, height = 6)

## SHH ----

### Get isopleths ----
SHH_95 <- get_ud_isopleth(ud = ud_SHH_pop_mean, level = 0.95)
SHH_50 <- get_ud_isopleth(ud = ud_SHH_pop_mean, level = 0.50)

SHH_95 <- SHH_95 %>%
  dplyr::mutate(isopleth = "95%")

SHH_50 <- SHH_50 %>%
  dplyr::mutate(isopleth = "50%")

SHH_iso <- dplyr::bind_rows(SHH_95, SHH_50)

### Make map ----


### SHH UD isopleth plot ----

p_ud_SHH <- ggplot() +
  geom_sf(data = SHH_iso,
          aes(fill = isopleth, 
              color = isopleth)) +
  scale_fill_manual(
    values = c("95%" = scales::alpha("#56B4E9", 0.35),
               "50%" = scales::alpha("#08306B", 0.55)),
    name = "UD isopleth") +
  scale_colour_manual(
    values = c("95%" = "#56B4E9",
               "50%" = "#08306B")) +
  guides(colour = "none") +
  ggnewscale::new_scale_colour() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = land_m, fill = "grey85", color = "grey50", linewidth = 0.2) +
  geom_sf(data = mpas_sf_m,
          aes(color = area_type,
              linewidth = area_type),
          fill = "transparent",
          inherit.aes = FALSE) +
  scale_color_manual(values = c("black", "#D55E00"), name = NULL) +
  scale_linewidth_manual(values = c(0.5, 1)) +
  guides(linewidth = "none") +
  geom_sf(data = corralito,
          color = "#D55E00",
          fill = "transparent",
          linewidth = 1.2) +
  coord_sf(
    xlim = c(bb_proj["xmin"], bb_proj["xmax"]),
    ylim = c(bb_proj["ymin"], bb_proj["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Scalloped Hammerhead (n = 94) BBMM (move) 50% and 95% UDs",
    x = NULL,
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.001, 0.0015),   # adjust as needed
    legend.justification = c(0, 0),   # anchor legend by bottom-left corner
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_blank(),
  )


# p_ud_SHH

ggsave("Processed_data/UDs/SHH_95_and_50_UDs.png", p_ud_SHH,
       units = "in", width = 12, height = 6)
