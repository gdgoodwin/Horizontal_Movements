## 04-Running_SSMs.R

rm(list = ls()[c(!(ls() %in% c("dat_trim")))])

source("src/00-Movement_Functions.R")
library(aniMotum)
library(patchwork)
library(gridGraphics)

tic("Overall")

# Import Data -------------------------------------------------------------

out.dir <- "SSM_Results/SSM_Outputs/"
dir.create(out.dir)

dat_trim <- read.csv("Processed_data/Filtered_Argos_SSH_silky_locations.csv") %>%
  mutate(
    PTT = as.character(PTT),
    across(c(contains("date"), dawn_dusk), pdt),
    Location.class = factor(
      Location.class,
      levels = c("B", "A", "0", "1", "2", "3"),
      ordered = TRUE
    )
  )


dat_mod <- dat_trim %>% 
  mutate(id = as.character(PTT)) %>% 
  arrange(desc(species), PTT) %>% 
  dplyr::select(id, Location.date, `date` = Location.date, lc = Location.class, 
                lon = Longitude, lat = Latitude, species, eor = Ellipse.orientation, 
                smaj = Semi.major.axis, smin = Semi.minor.axis) 

ptts <-unname(unique(dat_mod$id))
  

pred_times <- dat_trim %>% 
  dplyr::select(id = PTT, date = dawn_dusk) %>% 
  group_by(id, date) %>% 
  slice(1) %>% 
  ungroup()


sat_meta <- read.csv("Metadata/sat_meta.csv", stringsAsFactors = F) %>% 
  mutate(
    across(contains(c("date","time")), pdt),
    PTT = as.character(PTT)
  )



# RW ----------------------------------------------------------------------
tic("All Models")

tic("RW SSM")
ssm_rw <- aniMotum::fit_ssm(dat_mod, vmax = 2.5,
                            model = "rw", 
                            time.step = pred_times,
                            ang      = c(15, 25),            # deg: complements of 165 & 155
                            distlim  = c(10000, 20000),      # m: 10 km and 20 km
                            control = ssm_control(verbose = 1),
                            pf = F)

ssm_rw <- ssm_rw %>% 
  arrange(id)
toc() ## 184 seconds



# CRW ---------------------------------------------------------------------


tic("CRW SSM")
ssm_crw <- aniMotum::fit_ssm(dat_mod, vmax = 2.5,
                            model = "crw", 
                            time.step = pred_times,
                            ang      = c(15, 25),            # deg: complements of 165 & 155
                            distlim  = c(10000, 20000),      # m: 10 km and 20 km
                            control = ssm_control(verbose = 1),
                            pf = F,
                            map = list(psi = factor(NA)))

ssm_crw <- ssm_crw %>% 
  arrange(id)
toc() ## 262 seconds



# MP ----------------------------------------------------------------------


tic("MP SSM")
ssm_mp <- aniMotum::fit_ssm(dat_mod, vmax = 2.5,
                            model = "mp", 
                            time.step = pred_times,
                            ang      = c(15, 25),            # deg: complements of 165 & 155
                            distlim  = c(10000, 20000),      # m: 10 km and 20 km
                            control = ssm_control(verbose = 1),
                            pf = F,
                            map = list(psi = factor(NA)))
ssm_mp <- ssm_mp %>% 
  arrange(id)
toc() ## 589 sec

toc() ## All models: 983 sec, ~16 min


# Write/Read SSM Obj to Clear Env -----------------------------------------

saveRDS(ssm_rw,  file = file.path(out.dir, "ssm_rw.rds"),  compress = "xz")
saveRDS(ssm_crw, file = file.path(out.dir, "ssm_crw.rds"), compress = "xz")
saveRDS(ssm_mp,  file = file.path(out.dir, "ssm_mp.rds"),  compress = "xz")

ssm_rw  <- readRDS(file.path(out.dir, "ssm_rw.rds"))
ssm_crw <- readRDS(file.path(out.dir, "ssm_crw.rds"))
ssm_mp  <- readRDS(file.path(out.dir, "ssm_mp.rds"))


# Check Model Success -----------------------------------------------------

chk_hess <- bind_rows(ssm_rw, ssm_crw, ssm_mp) %>% 
  # filter(!pdHess) %>% 
  dplyr::select(-ssm) %>% 
  left_join(.,
            sat_meta %>% dplyr::select(id = PTT, species))

chk_converge <- bind_rows(ssm_rw, ssm_crw, ssm_mp) %>%
  # filter(!converged) %>%
  dplyr::select(-ssm) %>%
  left_join(.,
            sat_meta %>% dplyr::select(id = PTT, species))

chk_converge_all <- bind_rows(ssm_rw, ssm_crw, ssm_mp) %>%
  dplyr::select(-ssm) %>% 
  rename(model = pmodel) %>% 
  mutate(model = toupper(model))

this.ssm <- ssm_crw %>% dplyr::filter(id == chk_converge$id[1])

# ## Testing data
# 
# dat_tc <- dat_trim %>% 
#   mutate(
#     across(c(contains("date"), dawn_dusk), pdt),
#     ref_date = as.Date(Location.date),
#     PTT = as.character(PTT),
#     Location.class = factor(
#       Location.class,
#       levels = c("B", "A", "0", "1", "2", "3"),
#       ordered = TRUE
#     )
#   )
# 
# ## CRW
# this.ptt <- 175256
# ts.track.chk.all(this.ptt, dat_tc, sat_meta)
# this.ssm <- ssm_crw %>% dplyr::filter(id == this.ptt); plot(this.ssm, what = "fitted", type = 1, ask = F)[[1]]
# this.ssm <- ssm_crw %>% dplyr::filter(id == this.ptt); plot(this.ssm, what = "fitted", type = 2, ask = F)[[1]]
# 
# ## MP
# this.ptt <- 215285
# ts.track.chk.all(this.ptt, dat_tc, sat_meta)
# this.ssm <- ssm_mp %>% dplyr::filter(id == this.ptt); plot(this.ssm, what = "fitted", type = 1, ask = F)[[1]]
# this.ssm <- ssm_mp %>% dplyr::filter(id == this.ptt); plot(this.ssm, what = "fitted", type = 2, ask = F)[[1]]
# plot(ssm_mp, ids = this.ptt, what = "fitted", type = 2)
# 
# ssm_ptts <- ssm_rw %>% dplyr::select(PTT = id)
# # ssm_ptts <- data.frame(id = ptts)


## AIC Best ----



`%||%` <- function(x, y) if (!is.null(x)) x else y


aicc_all <- dplyr::bind_rows(
  ssm_rw  %>% dplyr::mutate(model = "RW",  AICc = purrr::map_dbl(ssm, ~ .x$AICc %||% NA_real_)) %>% dplyr::select(id, model, AICc),
  ssm_crw %>% dplyr::mutate(model = "CRW", AICc = purrr::map_dbl(ssm, ~ .x$AICc %||% NA_real_)) %>% dplyr::select(id, model, AICc),
  ssm_mp  %>% dplyr::mutate(model = "MP",  AICc = purrr::map_dbl(ssm, ~ .x$AICc %||% NA_real_)) %>% dplyr::select(id, model, AICc)
)

nrow(aicc_all)



aicc_rank <- aicc_all %>%
  left_join(chk_converge_all, by = c("id", "model")) %>% 
  # dplyr::filter(converged) %>% 
  group_by(id) %>%
  mutate(
    deltaAICc  = AICc - min(AICc, na.rm = TRUE),
    lowest_delta = {
      sorted <- sort(AICc)
      if (length(sorted) < 2) NA_real_ else sorted[2] - sorted[1]
    },
    best_model = model[which.min(AICc)]
  ) %>%
  ungroup() %>%
  left_join(sat_meta %>% dplyr::select(id = PTT, species), by = "id") %>% 
  arrange(desc(species), id)

nrow(aicc_rank)


aicc_rank_converge <- aicc_all %>%
  left_join(chk_converge_all, by = c("id", "model")) %>% 
  dplyr::filter(converged) %>%
  group_by(id) %>%
  mutate(
    deltaAICc  = AICc - min(AICc, na.rm = TRUE),
    lowest_delta = {
      sorted <- sort(AICc)
      if (length(sorted) < 2) NA_real_ else sorted[2] - sorted[1]
    },
    best_model = model[which.min(AICc)]
  ) %>%
  ungroup() %>%
  left_join(sat_meta %>% dplyr::select(id = PTT, species), by = "id") %>% 
  arrange(desc(species), id)

table(aicc_rank$best_model)
table(aicc_rank_converge$best_model)
chk_converge_all %>% 
  group_by(model, converged) %>% 
  summarise(n = n())

aicc_best <- aicc_rank_converge %>% 
  mutate(keep = model == best_model) %>% 
  filter(keep)

aicc_best %>% 
  group_by(species, model) %>% 
  summarise(n = n())

write.csv(aicc_best, "SSM_Results/Model_Comparison.csv", row.names = F)

# Visualize Results -------------------------------------------------------


## 3 Mods Combined ----


tic("Write combo results PDF")

# pdf(file = "SSM_Results/SSM_Combo_res-tracks-Fix-2.pdf",
#     width = 16, height = 9, onefile = TRUE)

grDevices::cairo_pdf(
  filename = "SSM_Results/SSM_Combo_res-tracks-Convergence.pdf",
  width = 16, height = 9,
  onefile = TRUE
)


failed   <- character(0)
fail_log <- list()



# ---- main loop ----
k_ellipse <- 2

for (i in seq_along(ptts)) {
# for (i in c(7,8)) {
  
  tic(paste0(i, " of ", length(ptts)))
  
  this.ptt <- ptts[i]
  
  this.ssm.rw  <- ssm_rw  %>% dplyr::filter(id == this.ptt)
  this.ssm.crw <- ssm_crw %>% dplyr::filter(id == this.ptt)
  this.ssm.mp  <- ssm_mp  %>% dplyr::filter(id == this.ptt)
  
  # RW row
  p.rw  <- safe_plot(this.ssm.rw,  this.ptt, "RW",  type = 1, k = k_ellipse)
  t1.rw <- safe_plot(this.ssm.rw,  this.ptt, "RW",  type = 2, k = k_ellipse)
  t2.rw <- safe_plot(this.ssm.rw,  this.ptt, "RW",  type = 2, outlier = FALSE, k = k_ellipse)
  
  # CRW row
  p.crw  <- safe_plot(this.ssm.crw, this.ptt, "CRW", type = 1, k = k_ellipse)
  t1.crw <- safe_plot(this.ssm.crw, this.ptt, "CRW", type = 2, k = k_ellipse)
  t2.crw <- safe_plot(this.ssm.crw, this.ptt, "CRW", type = 2, outlier = FALSE, k = k_ellipse)
  
  # MP row
  p.mp  <- safe_plot(this.ssm.mp,  this.ptt, "MP",  type = 1, k = k_ellipse)
  t1.mp <- safe_plot(this.ssm.mp,  this.ptt, "MP",  type = 2, k = k_ellipse)
  t2.mp <- safe_plot(this.ssm.mp,  this.ptt, "MP",  type = 2, outlier = FALSE, k = k_ellipse)
  
  # assemble 3x3 grid
  pw <- (p.rw  | t1.rw  | t2.rw) /
    (p.crw | t1.crw | t2.crw) /
    (p.mp  | t1.mp  | t2.mp)
  
  print(pw)
  
  toc()
}

# ---- failure summaries ----
if (length(fail_log) > 0) {
  fail_df <- dplyr::bind_rows(fail_log)
  
  fail_by_model <- fail_df %>%
    dplyr::distinct(ptt, model) %>%
    dplyr::count(model, name = "n_ptts_failed") %>%
    dplyr::arrange(model)
  
} else {
  fail_df <- data.frame(
    ptt = character(0),
    model = character(0),
    what = character(0),
    type = integer(0),
    outlier = logical(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
  
  fail_by_model <- data.frame(
    model = c("RW", "CRW", "MP"),
    n_ptts_failed = 0
  )
  
}

# ---- final summary page ----
plot.new()
text(0.5, 0.92, "SSM combo plotting summary (RW / CRW / MP)", cex = 1.5)

if (nrow(fail_df) == 0) {
  
  text(0.5, 0.75, "All PTTs plotted successfully (no aniMotum plot errors).", cex = 1.1)
  
} else {
  
  failed_u <- sort(unique(fail_df$ptt))
  
  text(0.5, 0.82,
       paste0("PTTs with ≥1 panel where aniMotum plot errored (n = ", length(failed_u), "):"),
       cex = 1.1)
  text(0.5, 0.72, paste(failed_u, collapse = ", "), cex = 0.8)
  
  text(0.5, 0.58, "Count of PTTs with ≥1 errored panel, by model:", cex = 1.2)
  
  y <- 0.50
  for (j in seq_len(nrow(fail_by_model))) {
    text(0.5, y,
         paste0(fail_by_model$model[j], ": ", fail_by_model$n_ptts_failed[j], " PTT(s)"),
         cex = 1.0)
    y <- y - 0.07
  }
  
  text(0.5, y - 0.05, "See CSV log for panel-level details.", cex = 0.9)
}

# dev.off()
grDevices::dev.off()


# ---- CSV log (optional) ----
if (length(fail_log) > 0) {
  fail_df_out <- dplyr::bind_rows(fail_log) %>%
    dplyr::group_by(model, ptt) %>%
    dplyr::distinct(ptt, type, outlier, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model, ptt, type)
  
  utils::write.csv(fail_df_out,
                   "SSM_Results/SSM_Combo_res-tracks_fail-log.csv",
                   row.names = FALSE)
}

toc() ## 871 sec, 14.5 min




# Extract Best SSMs -------------------------------------------------------


# aicc_choice <- read.csv("SSM_Results/Model_Comparison-Model_Changes.csv", 
#                         stringsAsFactors = F) %>% 
#   mutate(id = as.character(id),
#          model_choice = case_when(
#            !agreed ~ model_vis,
#            TRUE ~ model
#          ))
#   

ssm_best_mod <- bind_rows(ssm_rw, ssm_crw, ssm_mp) %>%
  # dplyr::select(-ssm) %>% 
  # left_join(aicc_choice %>% dplyr::select(id, model_choice),
  #           by = "id") %>%
  left_join(aicc_best %>% dplyr::select(id, model_choice = best_model),
            by = "id") %>%
  mutate(keep = toupper(pmodel) == model_choice) %>% 
  filter(keep) %>% 
  aniMotum::grab(., what = "predicted") %>% 
  left_join(., sat_meta %>% dplyr::select(id = PTT, species),
            by = "id")

write.csv(ssm_best_mod, "SSM_Results/SSM_dat_Best_Mod.csv", row.names = F)



# Plot SSM Tracks ---------------------------------------------------------

## ETP MPAs and EEZs
etp_eez_mpas <- st_read("Map_layers/ETP-MPAs_and_EEZs.shp")

## Bounding box ----

bbox_tracks <- with(ssm_best_mod, 
                    c(xmin = min(lon, na.rm = TRUE),
                      xmax = max(lon, na.rm = TRUE),
                      ymin = min(lat, na.rm = TRUE),
                      ymax = max(lat, na.rm = TRUE)))

pad_x <- diff(bbox_tracks[1:2]) * 0.05
pad_y <- diff(bbox_tracks[3:4]) * 0.05

bbox_tracks["xmin"] <- bbox_tracks["xmin"] - pad_x
bbox_tracks["xmax"] <- bbox_tracks["xmax"] + pad_x
bbox_tracks["ymin"] <- bbox_tracks["ymin"] - pad_y
bbox_tracks["ymax"] <- bbox_tracks["ymax"] + pad_y

## Plot ----

p.all <- ggplot() +
  geom_sf(data = world) +
  geom_path(data = ssm_best_mod, 
            aes(lon, lat, group = id, color = species), 
            linewidth = 0.5,
            alpha = 0.8) +
  scale_color_viridis_d(name = "Species") +
  geom_sf(data = etp_eez_mpas,
          # color = "blue",
          fill = "transparent",
          linewidth = 0.3) +
  coord_sf(xlim = c(bbox_tracks["xmin"], bbox_tracks["xmax"]),
           ylim = c(bbox_tracks["ymin"], bbox_tracks["ymax"]),
           expand = FALSE) +
  scale_x_continuous(
    breaks = pretty(bbox_tracks[c("xmin","xmax")], n = 5)
  ) +
  scale_y_continuous(
    breaks = pretty(bbox_tracks[c("ymin","ymax")], n = 5)
  ) +
  guides(color = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text  = element_text(size = 9),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9),
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0, "pt"),
        
        plot.margin = margin(t = 0, r = 5, b = 5, l = 5))

plotly::ggplotly(p.all)

ggsave("Output_Maps/All_traks.png", p.all, units = "in", 
       width = 12, height = 6)

### Silky ----
p.silky <-   ggplot() +
  geom_sf(data = land) +
  geom_path(data = ssm_best_mod %>% dplyr::filter(species == "Silky"), 
            aes(lon, lat, group = id, color = id), 
            linewidth = 0.5,
            alpha = 0.8) +
  scale_color_viridis_d() +
  guides(color = "none") +
  geom_sf(data = etp_eez_mpas,
          # color = "blue",
          fill = "transparent",
          linewidth = 0.3) +
  coord_sf(xlim = c(bbox_tracks["xmin"], bbox_tracks["xmax"]),
           ylim = c(bbox_tracks["ymin"], bbox_tracks["ymax"]),
           expand = FALSE) +
  scale_x_continuous(
    breaks = pretty(bbox_tracks[c("xmin","xmax")], n = 5)
  ) +
  scale_y_continuous(
    breaks = pretty(bbox_tracks[c("ymin","ymax")], n = 5)
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text  = element_text(size = 9)) +
  ggtitle("Silky")

p.silky
ggsave("Output_Maps/Silky_traks.png", p.silky, units = "in", 
       width = 12, height = 6)

### SHH ----
p.SHH <-   ggplot() +
  geom_sf(data = land) +
  geom_path(data = ssm_best_mod %>% dplyr::filter(species == "SHH"), 
            aes(lon, lat, group = id, color = id), 
            linewidth = 0.5,
            alpha = 0.8) +
  scale_color_viridis_d() +
  guides(color = "none") +
  geom_sf(data = etp_eez_mpas,
          # color = "blue",
          fill = "transparent",
          linewidth = 0.3) +
  coord_sf(xlim = c(bbox_tracks["xmin"], bbox_tracks["xmax"]),
           ylim = c(bbox_tracks["ymin"], bbox_tracks["ymax"]),
           expand = FALSE) +
  scale_x_continuous(
    breaks = pretty(bbox_tracks[c("xmin","xmax")], n = 5)
  ) +
  scale_y_continuous(
    breaks = pretty(bbox_tracks[c("ymin","ymax")], n = 5)
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.text  = element_text(size = 9)) +
  ggtitle("SHH")

p.SHH
ggsave("Output_Maps/SHH_traks.png", p.SHH, units = "in", 
       width = 12, height = 6)

