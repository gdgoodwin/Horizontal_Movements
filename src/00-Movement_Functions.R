## 00-Movement_Functions.R ----

# Libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(purrr)
library(xlsx)
library(tictoc)
library(ggplot2)
library(sf)


options(scipen = 999)

# Directories -------------------------------------------------------------

# if(!grepl("Silky", getwd())){
#   setwd("../Silky_hammers_Galapagos")
# }


# Layers ------------------------------------------------------------------


world <- rnaturalearth::ne_countries(
  scale = 50, continent = c("North America","South America"),
  returnclass = "sf") %>% 
  st_transform(4326)

land <- st_read("Map_layers/landfilt.shp") %>%
  st_as_sfc()



# Defaults ----------------------------------------------------------------

## CRSs ----

crs_proj <- sf::st_crs(
  "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
)

crs_proj4 <- crs_proj$wkt  # for sf/terra (modern)
crs_proj4_sp_rast <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"

## Bounding box ----
bb_proj <- sf::st_bbox(c(
  xmin = -15448.802,
  xmax = -8148.802,
  ymin = -1834.195,
  ymax = 2270.805
))

etp_crop <- sf::st_bbox(c(
  xmin = -160,
  xmax = -70,
  ymin = -10,
  ymax = 50),
  crs = sf::st_crs(4326))

# Functions ---------------------------------------------------------------

## Truncated full parse_date_time() function

pdt <- function(dt_char, tm.zn = "UTC"){
    parse_date_time(dt_char, 
                    orders = 
                      # c("mdy", "ymd", "mdy HM", "mdy HMS",  "ymd HMS", "ymd HM", "Ymd HMS", "Ymd HM"),
                      c("mdy", "ymd", "mdy HM", "ymd HM", "mdy HMS", "ymd HMS"),
                    train = F,
                    tz = tm.zn)
}


## Timeseries Track Comparison Combo

## Test variables

# tp <- this.ptt
# 
# dc <- dat_tc %>%
#   dplyr::filter(PTT == this.ptt)
# 
# 
# sm <- sat.meta


world_ll <- sf::st_transform(world, 4326)

fmt_lon <- function(x) {
  paste0(abs(x), "°", ifelse(x < 0, "W", "E"))
}

fmt_lat <- function(x) {
  paste0(abs(x), "°", ifelse(x < 0, "S", "N"))
}

ts.track.chk.dd <- function(tp, dc, sm){
  
  dc <- dc %>% 
    dplyr::filter(PTT == tp)
  
  pal_disc <- viridisLite::viridis(6)   # for Location.class (B,A,0,1,2,3)
  pal_cont <- viridisLite::viridis(256) # for time gradient
  
  dc <- dc %>%
    dplyr::mutate(
      dawn_dusk      = as.POSIXct(dawn_dusk, tz = "UTC"),
      Location.date = parse_date_time(Location.date, orders = c("ymd HMS", "ymd"), tz = "UTC"),
      # Location.date  = as.POSIXct(Location.date, tz = "UTC"),
      Location.class = factor(Location.class, levels = c("B","A","0","1","2","3"), ordered = TRUE),
      time_num       = as.numeric(Location.date)
    )
  
  sm_tp <- sm %>%
    dplyr::filter(PTT == tp) %>%
    dplyr::mutate(
      lon = if_else(!is.na(lon), as.numeric(lon), 90),
      lat = if_else(!is.na(lat), as.numeric(lat), -1)
    )
  
  
  
  # --- Top: time series (discrete colors) ---
  g_top <- plotly::plot_ly(
    data = dc,
    x = ~dawn_dusk,
    y = ~Location.class,
    type = "scatter",
    mode = "markers",
    color = ~Location.class,
    colors = pal_disc,
    marker = list(size = 7)
  ) %>%
    plotly::layout(
      title = paste0(tp, " — Best quality per 12 hr"),
      xaxis = list(title = ""),
      yaxis = list(title = "Location.class")
    )
  
  pad_x <- 1
  pad_y <- 1
  xlim <- range(dc$Longitude, na.rm = TRUE) + c(-pad_x, pad_x)
  ylim <- range(dc$Latitude,  na.rm = TRUE) + c(-pad_y, pad_y)
  
  lon_ticks <- pretty(xlim, n = 5)
  lat_ticks <- pretty(ylim, n = 5)
  
  bbox <- sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                        ymin = ylim[1], ymax = ylim[2]),
                      crs = sf::st_crs(4326))
  # world_crop <- sf::st_crop(world_ll, bbox)
  world_crop <- sf::st_crop(world_ll, bbox) %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%     # drop non-polygons
    sf::st_cast("MULTIPOLYGON", warn = FALSE)     # ensure one geometry type
  
  if("loc_id" %in% colnames(dc)){
    dc <- dc %>%
      mutate(
        hover_txt = paste0(
          "<b>Date:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%Y-%m-%d"), "<br>",
          "<b>Time:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%H:%M:%S"), "<br>",
          "<b>LC:</b> ", Location.class, "<br>",
          "<b>loc_id:</b> ", loc_id, "<br>",
          "<b>loc_num:</b> ", loc_num
        )
      )
  } else{
    dc <- dc %>%
      mutate(
        hover_txt = paste0(
          "<b>Date:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%Y-%m-%d"), "<br>",
          "<b>Time:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%H:%M:%S"), "<br>",
          "<b>LC:</b> ", Location.class
        )
      )
  }
  
  
  # --- track line (black, no hover) ---
  g_bot <- plotly::plot_ly() %>%
    plotly::add_sf(
      data = world_crop, inherit = FALSE,
      fillcolor = "grey90",
      line = list(color = "grey60", width = 0.5),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    
    plotly::add_trace(
      data = dc,
      x = ~Longitude, y = ~Latitude,
      type = "scatter", mode = "lines",
      line = list(color = "black", width = 1),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    
    # --- track points (colored by time, with hover text) ---
    plotly::add_trace(
      data = dc,
      x = ~Longitude, y = ~Latitude,
      type = "scatter", mode = "markers",
      text = ~hover_txt,
      hoverinfo = "text",
      marker = list(size = 6, showscale = TRUE),
      color = ~time_num,
      colors = pal_cont,
      showlegend = FALSE
    ) %>%
    
    # --- deployment point ---
    plotly::add_markers(
      data = sm_tp,
      x = ~lon, y = ~lat,
      marker = list(color = "red", size = 10),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    
    plotly::layout(
      xaxis = list(title = "Longitude", range = xlim),
      yaxis = list(title = "Latitude", range = ylim,
                   scaleanchor = "x", scaleratio = 1)
    )
  
  g_bot <- g_bot %>%
    plotly::layout(
      xaxis = list(
        title = "Longitude",
        range = xlim,
        tickmode = "array",
        tickvals = lon_ticks,
        ticktext = fmt_lon(lon_ticks),
        showticklabels = TRUE,
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis = list(
        title = "Latitude",
        range = ylim,
        tickmode = "array",
        tickvals = lat_ticks,
        ticktext = fmt_lat(lat_ticks),
        showticklabels = TRUE,
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside",
        scaleanchor = "x",
        scaleratio = 1
      )
    )
  
  
  
  plotly::subplot(g_top, g_bot, nrows = 2, shareX = FALSE, shareY = FALSE) %>%
    plotly::layout(
      hovermode = "closest",
      
      # bottom panel (map) axes are xaxis2/yaxis2
      xaxis2 = list(
        title = "Longitude",
        range = xlim,
        tickmode = "array",
        tickvals = lon_ticks,
        ticktext = fmt_lon(lon_ticks),
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis2 = list(
        title = "Latitude",
        range = ylim,
        tickmode = "array",
        tickvals = lat_ticks,
        ticktext = fmt_lat(lat_ticks),
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside",
        scaleanchor = "x2",   # IMPORTANT: anchor to xaxis2
        scaleratio = 1
      )
    )
}

## Time-series and track check not using dawn_dusk

ts.track.chk.all <- function(tp, dc, sm){
  
  dc <- dc %>% 
    dplyr::filter(PTT == tp)
  
  pal_disc <- viridisLite::viridis(6)   # for Location.class (B,A,0,1,2,3)
  pal_cont <- viridisLite::viridis(256) # for time gradient
  
  dc <- dc %>%
    dplyr::mutate(
      # dawn_dusk      = as.POSIXct(dawn_dusk, tz = "UTC"),
      # Location.date = parse_date_time(Location.date, orders = c("ymd HMS", "ymd"), tz = "UTC"),
      # # Location.date  = as.POSIXct(Location.date, tz = "UTC"),
      # Location.class = factor(Location.class, levels = c("B","A","0","1","2","3"), ordered = TRUE),
      time_num       = as.numeric(Location.date)
    )
  
  sm_tp <- sm %>%
    dplyr::filter(PTT == tp) %>%
    dplyr::mutate(
      lon = if_else(!is.na(lon), as.numeric(lon), 90),
      lat = if_else(!is.na(lat), as.numeric(lat), -1)
    )
  
  
  
  # --- Top: time series (discrete colors) ---
  g_top <- plotly::plot_ly(
    data = dc,
    x = ~Location.date,
    y = ~Location.class,
    type = "scatter",
    mode = "markers",
    color = ~Location.class,
    colors = pal_disc,
    marker = list(size = 7)
  ) %>%
    plotly::layout(
      title = paste0(tp, " — All data"),
      xaxis = list(title = ""),
      yaxis = list(title = "Location.class")
    )
  
  pad_x <- 1
  pad_y <- 1
  xlim <- range(dc$Longitude, na.rm = TRUE) + c(-pad_x, pad_x)
  ylim <- range(dc$Latitude,  na.rm = TRUE) + c(-pad_y, pad_y)
  
  lon_ticks <- pretty(xlim, n = 5)
  lat_ticks <- pretty(ylim, n = 5)
  
  bbox <- sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                        ymin = ylim[1], ymax = ylim[2]),
                      crs = sf::st_crs(4326))
  # world_crop <- sf::st_crop(world_ll, bbox)
  world_crop <- sf::st_crop(world_ll, bbox) %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%     # drop non-polygons
    sf::st_cast("MULTIPOLYGON", warn = FALSE)     # ensure one geometry type
  
  if("loc_id" %in% colnames(dc)){
    dc <- dc %>%
      mutate(
        hover_txt = paste0(
          "<b>Date:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%Y-%m-%d"), "<br>",
          "<b>Time:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%H:%M:%S"), "<br>",
          "<b>LC:</b> ", Location.class, "<br>",
          "<b>loc_id:</b> ", loc_id, "<br>",
          "<b>loc_num:</b> ", loc_num
        )
      )
  } else{
    dc <- dc %>%
      mutate(
        hover_txt = paste0(
          "<b>Date:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%Y-%m-%d"), "<br>",
          "<b>Time:</b> ", format(as.POSIXct(Location.date, tz = "UTC"), "%H:%M:%S"), "<br>",
          "<b>LC:</b> ", Location.class
        )
      )
  }
  
  
  # --- track line (black, no hover) ---
  g_bot <- plotly::plot_ly() %>%
    plotly::add_sf(
      data = world_crop, inherit = FALSE,
      fillcolor = "grey90",
      line = list(color = "grey60", width = 0.5),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    
    plotly::add_trace(
      data = dc,
      x = ~Longitude, y = ~Latitude,
      type = "scatter", mode = "lines",
      line = list(color = "black", width = 1),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    
    # --- track points (colored by time, with hover text) ---
    plotly::add_trace(
      data = dc,
      x = ~Longitude, y = ~Latitude,
      type = "scatter", mode = "markers",
      text = ~hover_txt,
      hoverinfo = "text",
      marker = list(size = 6, showscale = TRUE),
      color = ~time_num,
      colors = pal_cont,
      showlegend = FALSE
    ) %>%
    
    # --- deployment point ---
    plotly::add_markers(
      data = sm_tp,
      x = ~lon, y = ~lat,
      marker = list(color = "red", size = 10),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    
    plotly::layout(
      xaxis = list(title = "Longitude", range = xlim),
      yaxis = list(title = "Latitude", range = ylim,
                   scaleanchor = "x", scaleratio = 1)
    )
  
  g_bot <- g_bot %>%
    plotly::layout(
      xaxis = list(
        title = "Longitude",
        range = xlim,
        tickmode = "array",
        tickvals = lon_ticks,
        ticktext = fmt_lon(lon_ticks),
        showticklabels = TRUE,
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis = list(
        title = "Latitude",
        range = ylim,
        tickmode = "array",
        tickvals = lat_ticks,
        ticktext = fmt_lat(lat_ticks),
        showticklabels = TRUE,
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside",
        scaleanchor = "x",
        scaleratio = 1
      )
    )
  
  
  
  plotly::subplot(g_top, g_bot, nrows = 2, shareX = FALSE, shareY = FALSE) %>%
    plotly::layout(
      hovermode = "closest",
      
      # bottom panel (map) axes are xaxis2/yaxis2
      xaxis2 = list(
        title = "Longitude",
        range = xlim,
        tickmode = "array",
        tickvals = lon_ticks,
        ticktext = fmt_lon(lon_ticks),
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis2 = list(
        title = "Latitude",
        range = ylim,
        tickmode = "array",
        tickvals = lat_ticks,
        ticktext = fmt_lat(lat_ticks),
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        showline = TRUE,
        ticks = "outside",
        scaleanchor = "x2",   # IMPORTANT: anchor to xaxis2
        scaleratio = 1
      )
    )
}



ts.chk <- function(tp, dc, sm){
  plotly::ggplotly(
    base_plot <- ggplot(
      dc,
      aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_bw() + 
      ggtitle(tp)
  )
}


ts.chk.comp <- function(tp, dc, dt, sm){
  
  
  p_top <- ggplot(dc, aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
    geom_point() +
    scale_color_viridis_d(drop = FALSE) +
    theme_bw() +
    labs(title = paste0(tp, " — Best quality per 12 hr"))
  
  p_bot <- ggplot(dt, aes(x = dttm, y = Location.class, color = Location.class)) +
    geom_point(alpha = 0.7, size = 0.9) +
    scale_color_viridis_d(drop = FALSE) +
    theme_bw() +
    labs(title = paste0(tp, " — Full dataset"))
  
  g_top <- plotly::ggplotly(p_top)
  g_bot <- plotly::ggplotly(p_bot)
  
  plotly::subplot(
    g_top, g_bot,
    nrows = 2,
    shareX = FALSE,     # set TRUE if both x are same variable/class
    shareY = TRUE,
    titleY = TRUE
  ) %>%
    plotly::layout(showlegend = T)  # change to TRUE if you want legend
}

## Track

track.chk <- function(tp, dc, sm){
  plotly::ggplotly(
    base_plot <- ggplot() +
      geom_sf(data = world, fill = "grey90", color = "grey60") +
      geom_path(
        data = dc,
        aes(x = Longitude, y = Latitude, group = PTT),
        linewidth = 0.25,
        color = "black"
      ) +
      geom_point(
        data = dc,
        aes(x = Longitude, y = Latitude, color = Location.date),
        size = 0.85, alpha = 0.8
      ) +
      scale_color_viridis_d() +
      geom_point(data = sat_meta %>% dplyr::filter(PTT == tp),
                 aes(x = as.numeric(lon), y = as.numeric(lat)), color = "red",
                 size = 0.75, alpha = 0.8) +
      theme_bw() +
      coord_sf(xlim = c(min(dc$Longitude) - 1, max(dc$Longitude) + 1),
               ylim = c(min(dc$Latitude) - 1, max(dc$Latitude) + 1)) + 
      ggtitle(tp)
  )
}


# Combo Plot for Track Checks ---------------------------------------------

# ---- helper: error tile (last resort) ----
err_tile <- function(ptt, model, err) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 0, y = 0.25,
                      label = paste0("PTT Plot Failed\n",
                                     "PTT: ", ptt, "\n",
                                     "Model: ", model),
                      size = 5, fontface = "bold") +
    ggplot2::annotate("text", x = 0, y = -0.25,
                      label = err, size = 3.2) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
}

# ---- helper: add x/y columns from sf geometry ----
sf_xy <- function(sfobj, geom = "geometry") {
  xy <- sf::st_coordinates(sfobj[[geom]])
  dplyr::mutate(sfobj, x = xy[, 1], y = xy[, 2])
}

# ---- helper: build fallback panels (ggplot) ----
# type = 2 -> track panel (xy)
# type = 1 -> time-series panel ("x" or "y") selected by axis = "x"/"y"
fallback_panel <- function(ssm_obj, ptt, model, type = 1, axis = "x",
                           outlier = NULL, k = 2, bad.ptts = chk_converge) {
  
  # Map display model to chk_converge$pmodel values
  model_key <- c(RW = "rw", CRW = "crw", MP = "mp")[[model]]
  if (is.null(model_key) || is.na(model_key)) model_key <- tolower(model)
  
  # ---- Build plot title to match your aniMotum panels ----
  plot.title <- ""
  title_col  <- "black"
  title_face <- "plain"
  this_species <- sat_meta$species[match(as.character(ptt), sat_meta$PTT)]
  
  if (type == 1) {
    # time-series column
    plot.title <- paste0(model, " - ", this_species)
    best_mod <- aicc_best$best_model[match(as.character(ptt), aicc_best$id)]
    title_col <- if (!is.na(best_mod) && model == best_mod) "green3" else "black"
    title_face <- if (type == 1 && title_info$is_best) "bold" else "plain"
    
    
  } else if (type == 2 && is.null(outlier)) {
    # track panel WITH outliers (default)
    plot.title <- as.character(ptt)
    
  } else if (type == 2 && identical(outlier, FALSE)) {
    # track panel WITHOUT outliers (3rd column): blank unless fail flags exist
    bad_row <- bad.ptts %>%
      dplyr::filter(id == as.character(ptt), pmodel == model_key) %>%
      dplyr::slice(1)
    
    if (nrow(bad_row) > 0) {
      fail_bits <- character(0)
      if (isFALSE(bad_row$converged[1])) fail_bits <- c(fail_bits, "converge")
      if (isFALSE(bad_row$pdHess[1]))    fail_bits <- c(fail_bits, "Hess")
      
      if (length(fail_bits) > 0) {
        plot.title <- paste0("FAILED: ", paste(fail_bits, collapse = " + "))
        title_col  <- "red"
        title_face <- "bold"
      } else {
        plot.title <- ""
      }
    } else {
      plot.title <- ""
    }
  }
  
  # raw obs (with keep flag)
  dat_sf <- aniMotum::grab(ssm_obj, what = "data", as_sf = TRUE) %>%
    tidyr::drop_na(geometry) %>%
    dplyr::filter(id == as.character(ptt)) %>%
    dplyr::arrange(date) %>%
    sf_xy()
  
  obs_keep <- dat_sf %>% dplyr::filter(keep)
  obs_out  <- dat_sf %>% dplyr::filter(!keep)
  
  # fitted states + SEs
  fit_sf <- aniMotum::grab(ssm_obj, what = "fitted", as_sf = TRUE) %>%
    tidyr::drop_na(date, x.se, y.se, geometry) %>%
    dplyr::filter(id == as.character(ptt)) %>%
    dplyr::arrange(date) %>%
    sf_xy()
  
  # mimic outlier suppression (only affects plotting of outlier obs)
  if (!is.null(outlier) && identical(outlier, FALSE)) {
    obs_out <- obs_out[0, ]
  }
  
  if (type == 2) {
    ggplot2::ggplot() +
      ggforce::geom_ellipse(
        data = fit_sf,
        ggplot2::aes(x0 = x, y0 = y, a = k * x.se, b = k * y.se, angle = 0),
        fill = "goldenrod2", color = NA, alpha = 0.35
      ) +
      ggplot2::geom_sf(data = obs_keep, color = "steelblue4", size = 2, alpha = 0.4) +
      ggplot2::geom_sf(data = obs_out,  shape = 4, color = "black", size = 1, stroke = 1) +
      ggplot2::geom_path(data = fit_sf, ggplot2::aes(x = x, y = y),
                         color = "darkorange", linewidth = 0.1) +
      ggplot2::geom_sf(data = fit_sf, color = "darkorange", size = 0.5) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(plot.title) +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(size = 10, colour = title_col, face = title_face),
        axis.text.x = ggplot2::element_text(size = 7.5)
      )
    
  } else {
    
    if (axis == "x") {
      ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = fit_sf,
          ggplot2::aes(x = date, ymin = x - k * x.se, ymax = x + k * x.se),
          fill = "goldenrod2", alpha = 0.35
        ) +
        ggplot2::geom_point(data = obs_keep, ggplot2::aes(x = date, y = x),
                            color = "steelblue4", size = 2, alpha = 0.4) +
        ggplot2::geom_line(data = fit_sf, ggplot2::aes(x = date, y = x),
                           color = "darkorange", linewidth = 0.1) +
        ggplot2::geom_point(data = fit_sf, ggplot2::aes(x = date, y = x),
                            color = "darkorange", size = 0.5) +
        ggplot2::geom_point(data = obs_out, ggplot2::aes(x = date, y = x),
                            shape = 4, color = "black", size = 1, stroke = 0.15) +
        ggplot2::geom_rug(data = obs_keep, ggplot2::aes(x = date),
                          sides = "b", color = "steelblue4", alpha = 0.4) +
        ggplot2::labs(title = "x", x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(plot.title) +
        ggplot2::theme(
          plot.title  = ggplot2::element_text(size = 10, colour = title_col, face = title_face),
          axis.text.x = ggplot2::element_text(size = 7.5)
        )
      
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = fit_sf,
          ggplot2::aes(x = date, ymin = y - k * y.se, ymax = y + k * y.se),
          fill = "goldenrod2", alpha = 0.35
        ) +
        ggplot2::geom_point(data = obs_keep, ggplot2::aes(x = date, y = y),
                            color = "steelblue4", size = 2, alpha = 0.4) +
        ggplot2::geom_line(data = fit_sf, ggplot2::aes(x = date, y = y),
                           color = "darkorange", linewidth = 0.1) +
        ggplot2::geom_point(data = fit_sf, ggplot2::aes(x = date, y = y),
                            color = "darkorange", size = 0.5) +
        ggplot2::geom_point(data = obs_out, ggplot2::aes(x = date, y = y),
                            shape = 4, color = "black", size = 1, stroke = 0.15) +
        ggplot2::geom_rug(data = obs_keep, ggplot2::aes(x = date),
                          sides = "b", color = "steelblue4", alpha = 0.4) +
        ggplot2::labs(title = "y", x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(plot.title) +
        ggplot2::theme(
          plot.title  = ggplot2::element_text(size = 10, colour = title_col, face = title_face),
          axis.text.x = ggplot2::element_text(size = 7.5)
        )
    }
  }
}

# ---- helper: safely get a panel; if aniMotum plot fails -> fallback -> else err tile ----
safe_plot <- function(ssm_obj, ptt, model, what = "fitted",
                      type = 1, outlier = NULL, k = 2, bad.ptts = chk_converge) {
  
  # map model for chk_converge matching
  model_key <- c(RW = "rw", CRW = "crw", MP = "mp")[[model]]
  if (is.null(model_key) || is.na(model_key)) model_key <- tolower(model)
  
  # Title logic: only show FAILED title for the 3rd column (outlier = FALSE)
  make_title <- function() {
    
    # default
    title_text <- ""
    is_fail <- FALSE
    is_best <- FALSE
    this_species <- sat_meta$species[match(as.character(ptt), sat_meta$PTT)]
    
    if (is.null(outlier)) {
      if (type == 1) {
        title_text <- paste0(model, " - ", this_species)
        best_mod <- aicc_best$best_model[match(as.character(ptt), aicc_best$id)]
        is_best  <- !is.na(best_mod) && isTRUE(model == best_mod)
      } else {
        title_text <- as.character(ptt)
      }
      
    } else if (identical(outlier, FALSE)) {
      
      bad_row <- bad.ptts %>%
        dplyr::filter(id == as.character(ptt), pmodel == model_key) %>%
        dplyr::slice(1)
      
      if (nrow(bad_row) > 0) {
        
        fail_bits <- c()
        if (isFALSE(bad_row$converged[1])) fail_bits <- c(fail_bits, "converge")
        if (isFALSE(bad_row$pdHess[1]))    fail_bits <- c(fail_bits, "Hess")
        
        if (length(fail_bits) > 0) {
          title_text <- paste0("FAILED: ", paste(fail_bits, collapse = " + "))
          is_fail    <- TRUE
        }
      }
    }
    
    list(text = title_text, is_fail = is_fail, is_best = is_best)
  }
  
  
  out <- tryCatch({
    
    title_info <- make_title()
    plot.title <- title_info$text
    title_col  <- if (title_info$is_fail) "red" else "black"
    title_face <- if (title_col == "red") "bold" else "plain"
    title_col <- if (type == 1 && title_info$is_best) "green3" else title_col
    title_face <- if (type == 1 && title_info$is_best) "bold" else title_face
    
    
    
    
    if (is.null(outlier)) {
      plot(ssm_obj, what = what, type = type, ask = FALSE)[[1]] +
        ggplot2::ggtitle(plot.title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10,  colour = title_col, face = title_face),
                       axis.text.x = ggplot2::element_text(size = 7.5))
    } else {
      plot(ssm_obj, what = what, type = type, outlier = outlier, ask = FALSE)[[1]] +
        ggplot2::ggtitle(plot.title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10,  colour = title_col, face = title_face),
                       axis.text.x = ggplot2::element_text(size = 7.5))
    }
    
  }, error = function(e1) {
    
    err1 <- conditionMessage(e1)
    
    fb <- tryCatch({
      
      if (type == 1) {
        fallback_panel(ssm_obj, ptt = ptt, model = model, type = 1, axis = "x",
                       outlier = outlier, k = k, bad.ptts = bad.ptts)
      } else {
        fallback_panel(ssm_obj, ptt = ptt, model = model, type = 2, axis = "x",
                       outlier = outlier, k = k, bad.ptts = bad.ptts)
      }
      
    }, error = function(e2) NULL)
    
    if (!is.null(fb)) {
      
      message("⚠️  aniMotum plot failed for PTT ", ptt, " (", model, ", type=", type,
              if (!is.null(outlier)) paste0(", outlier=", outlier) else "",
              "). Using fallback ggplot. Error: ", err1)
      
      failed <<- c(failed, ptt)
      fail_log[[length(fail_log) + 1]] <<- data.frame(
        ptt = ptt,
        model = model,
        what = what,
        type = type,
        outlier = if (is.null(outlier)) NA else outlier,
        error = paste0("aniMotum plot failed; fallback used. ", err1),
        stringsAsFactors = FALSE
      )
      
      return(fb)
    }
    
    message("⚠️  Plot failed for PTT ", ptt, " (", model, ", type=", type,
            if (!is.null(outlier)) paste0(", outlier=", outlier) else "",
            "): ", err1)
    
    failed <<- c(failed, ptt)
    fail_log[[length(fail_log) + 1]] <<- data.frame(
      ptt = ptt,
      model = model,
      what = what,
      type = type,
      outlier = if (is.null(outlier)) NA else outlier,
      error = err1,
      stringsAsFactors = FALSE
    )
    
    err_tile(ptt, model, err1)
  })
  
  # consistent final formatting
  # out + ggplot2::theme(
  #   plot.title  = ggplot2::element_text(size = 10),
  #   axis.text.x = ggplot2::element_text(size = 7.5))
  out + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 7.5)
  )
}



## Recursive unzip function ----
unzip_recursive <- function(zip_files) {
  
  for (z in zip_files) {
    
    message("Unzipping: ", z)
    
    # Extract into its parent directory
    unzip(
      zipfile = z,
      exdir   = stringr::str_remove(z, ".zip")
    )
  }
  
  # Check again for any newly created zips (nested)
  new_zips <- list.files(
    dir.lyrs,
    pattern = "\\.zip$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  # If new zip files exist, unzip again
  if (length(new_zips) > length(zip_files)) {
    unzip_recursive(new_zips)
  }
}

## Elapsed time for progress tracking
format_elapsed <- function(start_time, end_time = Sys.time()) {
  
  secs <- round(as.numeric(difftime(end_time, start_time, units = "secs")))
  
  if (secs < 60) {
    return(sprintf("%d sec", secs))
  }
  
  mins <- secs %/% 60
  rem_secs <- secs %% 60
  
  if (mins < 60) {
    return(sprintf("%d min %d sec", mins, rem_secs))
  }
  
  hrs <- mins %/% 60
  rem_mins <- mins %% 60
  
  return(sprintf("%d hr %d min", hrs, rem_mins))
}


# Plotting stuff ----------------------------------------------------------

get_ud_isopleth <- function(ud, level) {
  
  # values and cell area
  vals <- raster::getValues(ud)
  ok <- !is.na(vals) & vals > 0
  
  v <- vals[ok]
  cell_area <- prod(raster::res(ud))   # km^2 if raster is in km units
  # raster::res(ud_silky_pop_mean)
  
  # order cells from highest to lowest UD
  ord <- order(v, decreasing = TRUE)
  v_ord <- v[ord]
  
  # cumulative probability mass
  cum_mass <- cumsum(v_ord)
  
  # threshold value giving desired isopleth
  thr <- v_ord[max(which(cum_mass <= level))]
  
  # binary raster of included cells
  r_bin <- ud >= thr
  
  # raster to polygons
  p <- raster::rasterToPolygons(r_bin, fun = function(x) x == 1, dissolve = TRUE)
  sf::st_as_sf(p)
}


get_pres_abs_isopleth <- function(ud, level) {
  
  # values and cell area
  vals <- raster::getValues(ud)
  ok <- !is.na(vals) & vals > 0
  
  v <- vals[ok]
  cell_area <- prod(raster::res(ud))   # km^2 if raster is in km units
  # raster::res(ud_silky_pop_mean)
  
  # order cells from highest to lowest UD
  ord <- order(v, decreasing = TRUE)
  v_ord <- v[ord]
  
  # cumulative probability mass
  cum_mass <- cumsum(v_ord)
  
  # threshold value giving desired isopleth
  thr <- v_ord[which(cum_mass >= level)[1]]
  
  # binary raster of included cells
  r_bin <- ud
  r_bin[] <- 0
  r_bin[ud >= thr] <- 1
  
  return(r_bin)
}

# Defunct -----------------------------------------------------------------

# 
# 
# ts.track.chk <- function(tp, dc, sm){
#   
#   dc <- dc %>%
#     dplyr::mutate(
#       dawn_dusk     = as.POSIXct(dawn_dusk, tz = "UTC"),
#       Location.date = as.POSIXct(Location.date, tz = "UTC")
#     )
#   
#   sm_tp <- sm %>%
#     dplyr::filter(PTT == tp) %>%
#     dplyr::mutate(lon = as.numeric(lon), lat = as.numeric(lat))
#   
#   xlim <- range(dc$Longitude, na.rm = TRUE) + c(-1, 1)
#   ylim <- range(dc$Latitude,  na.rm = TRUE) + c(-1, 1)
#   
#   p_top <- ggplot(dc, aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
#     geom_point() +
#     scale_color_viridis_d(drop = FALSE) +
#     theme_bw() +
#     labs(title = paste0(tp, " — Best quality per 12 hr"))
#   
#   p_bot <- ggplot() +
#     geom_sf(data = world, fill = "grey90", color = "grey60") +
#     geom_path(data = dc, aes(Longitude, Latitude, group = PTT),
#               linewidth = 0.25, color = "black") +
#     geom_point(data = dc, aes(Longitude, Latitude, color = Location.date),
#                size = 0.85, alpha = 0.8) +
#     scale_color_viridis_c(name = "Time", guide = "none") +
#     geom_point(data = sm_tp, aes(lon, lat),
#                inherit.aes = FALSE, color = "red", size = 2) +
#     theme_bw() +
#     coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
#   
#   g_top <- plotly::ggplotly(p_top) %>% plotly::layout(showlegend = FALSE)
#   g_bot <- plotly::ggplotly(p_bot)
#   
#   plotly::subplot(g_top, g_bot, nrows = 2, shareX = FALSE, shareY = FALSE) %>%
#     plotly::layout(
#       # force subplot axes to stay independent
#       xaxis  = list(title = "",  domain = c(0, 1), anchor = "y"),
#       yaxis  = list(title = "Location.class", domain = c(0.55, 1), anchor = "x"),
#       
#       xaxis2 = list(title = "Longitude", domain = c(0, 1), anchor = "y2"),
#       yaxis2 = list(title = "Latitude",  domain = c(0, 0.45), anchor = "x2",
#                     scaleanchor = "x2", scaleratio = 1),  # keep map-ish aspect
#       
#       margin = list(l = 60, r = 160, t = 50, b = 50),
#       showlegend = TRUE
#     )
# }
# 
# ts.track.chk <- function(tp, dc, sm){
#   
#   # ensure proper types for ggplotly
#   dc <- dc %>%
#     dplyr::mutate(
#       dawn_dusk = as.POSIXct(dawn_dusk, tz = "UTC"),
#       Location.date = as.POSIXct(Location.date, tz = "UTC")
#     )
#   
#   sm_tp <- sm %>%
#     dplyr::filter(PTT == tp) %>%
#     dplyr::mutate(
#       lon = as.numeric(lon),
#       lat = as.numeric(lat)
#     )
#   
#   # TOP: time series
#   p_top <- ggplot(dc, aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
#     geom_point() +
#     scale_color_viridis_d(drop = FALSE, name = "Loc class") +
#     theme_bw() +
#     labs(title = paste0(tp, " — Best quality per 12 hr"))
#   
#   # extents from dc
#   xlim <- range(dc$Longitude, na.rm = TRUE) + c(-1, 1)
#   ylim <- range(dc$Latitude,  na.rm = TRUE) + c(-1, 1)
#   
#   # BOTTOM: map  (IMPORTANT: use viridis_c + coord_quickmap)
#   p_bot <- ggplot() +
#     geom_sf(data = world, fill = "grey90", color = "grey60") +
#     geom_path(
#       data = dc,
#       aes(x = Longitude, y = Latitude, group = PTT),
#       linewidth = 0.25,
#       color = "black"
#     ) +
#     geom_point(
#       data = dc,
#       aes(x = Longitude, y = Latitude, color = Location.date),
#       size = 0.85, alpha = 0.8
#     ) +
#     scale_color_viridis_c(name = "Time") +
#     geom_point(
#       data = sm_tp,
#       aes(x = lon, y = lat),
#       inherit.aes = FALSE,
#       color = "red",
#       size = 2
#     ) +
#     theme_bw() +
#     coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
#   
#   g_top <- plotly::ggplotly(p_top) %>% plotly::layout(showlegend = FALSE)
#   g_bot <- plotly::ggplotly(p_bot)
#   
#   plotly::subplot(
#     g_top, g_bot,
#     nrows = 2,
#     shareX = FALSE,
#     shareY = FALSE,
#     titleY = FALSE
#   ) %>%
#     plotly::layout(showlegend = TRUE)
# }
# 
# 
# ts.track.chk <- function(tp, dc, sm){
#   
#   # ensure proper types for ggplotly
#   dc <- dc %>%
#     dplyr::mutate(
#       dawn_dusk = as.POSIXct(dawn_dusk, tz = "UTC"),
#       Location.date = as.POSIXct(Location.date, tz = "UTC")
#     )
#   
#   sm_tp <- sm %>%
#     dplyr::filter(PTT == tp) %>%
#     dplyr::mutate(
#       lon = as.numeric(lon),
#       lat = as.numeric(lat)
#     )
#   
#   # TOP: time series
#   p_top <- ggplot(dc, aes(x = dawn_dusk, y = Location.class, color = Location.class)) +
#     geom_point() +
#     scale_color_viridis_d(drop = FALSE, name = "Loc class") +
#     theme_bw() +
#     labs(title = paste0(tp, " — Best quality per 12 hr"))
#   
#   # extents from dc
#   xlim <- range(dc$Longitude, na.rm = TRUE) + c(-1, 1)
#   ylim <- range(dc$Latitude,  na.rm = TRUE) + c(-1, 1)
#   
#   # BOTTOM: map  (IMPORTANT: use viridis_c + coord_quickmap)
#   p_bot <- ggplot() +
#     geom_sf(data = world, fill = "grey90", color = "grey60") +
#     geom_path(
#       data = dc,
#       aes(x = Longitude, y = Latitude, group = PTT),
#       linewidth = 0.25,
#       color = "black"
#     ) +
#     geom_point(
#       data = dc,
#       aes(x = Longitude, y = Latitude, color = Location.date),
#       size = 0.85, alpha = 0.8
#     ) +
#     scale_color_viridis_c(name = "Time") +
#     geom_point(
#       data = sm_tp,
#       aes(x = lon, y = lat),
#       inherit.aes = FALSE,
#       color = "red",
#       size = 2
#     ) +
#     theme_bw() +
#     coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
#   
#   g_top <- plotly::ggplotly(p_top) #%>% plotly::layout(showlegend = FALSE)
#   g_bot <- plotly::ggplotly(p_bot)
#   
#   plotly::subplot(
#     g_top, g_bot,
#     nrows = 2,
#     shareX = FALSE,
#     shareY = FALSE,
#     titleY = FALSE
#   ) %>%
#     plotly::layout(showlegend = FALSE)
# }
# 
