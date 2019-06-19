tide_plot <- function(h, vs = c("habitat", "location"), tide_dat = NULL, tide_range = NULL,
                      det_dat, # Needed for all
                      # Needed for "habitat"
                      hab_dat = NULL,  hrly_summary = NULL,
                      # Needed for "location"
                      site_locs = NULL, bg_rast = NULL, hab_df = NULL) {
  theme_set(theme_bw(base_size = 14))
  vs <- match.arg(vs)
  hr_range <- h + as.difftime(c(-14, 14), units = "hours")
  hr_breaks <- h + as.difftime(seq(-12, 12, by = 3), units = "hours")
  tmp <- filter(tide_dat, between(ts_hour, hr_range[1], hr_range[2]))
  current <- filter(tide_dat, ts_hour == h)
  
  if (vs == "habitat") {
    # Tide plot
    tide_gg <- ggplot(tmp, aes(ts_hour, tideHt)) +
      geom_line(color = "gray50", size = 1.5) +
      geom_point(data = current, size = 2.5, stroke = 2, shape = 21) +
      geom_hline(aes(yintercept = tide_range[2]), lty = "dashed", size = 2, col = "grey20") +
      geom_hline(aes(yintercept = tide_range[1]), lty = "dashed", size = 2, col = "grey20") +
      scale_y_continuous("Tide height (m)", limits = c(floor(tide_range[1]), ceiling(tide_range[2]))) +
      scale_x_datetime("", breaks = hr_breaks, labels = date_format("%d %b %H:%M"),
                       limits = hr_range, expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
            axis.title.x = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"))

    h_hrly_summary <- filter(hourly_summary, ts_hour == h)
    hourly_indiv <- h_hrly_summary$hourly_indiv
    hourly_det_time <- h_hrly_summary$hourly_det_time
    h_det <- filter(det_dat, ts_hour == h) %>%
      ungroup() %>%
      complete(ts_hour = h, ant_avail, 
               fill = list(tot_det_h = 0)) %>%
      group_by(recvSiteName, antBin) %>%
      summarize(tot_det_h = sum(tot_det_h)) %>% ungroup() %>%
      mutate(p_det = ifelse(tot_det_h > 0, tot_det_h/sum(tot_det_h), 0)) %>%
      left_join(hab_dat, by = c("recvSiteName", "antBin")) %>%
      filter(recvSiteName != "Punta Catalina") %>% droplevels() %>%
      mutate(rel_area = p_det * area_ha) %>%
      group_by(habitat_gen) %>%
      summarize(rel_area = sum(rel_area)) %>%
      mutate(rel_prop = ifelse(rel_area > 0, rel_area/sum(rel_area), 0))
    vs_gg <- ggplot(h_det, aes(x = habitat_gen, y = rel_prop)) + 
      geom_bar(aes(fill = habitat_gen), col = "black", stat = "identity") + 
      annotate("text", label = paste(round(hourly_det_time, 2), "detection hours"), 
               x = 6.5, y = 0.8, hjust = 1, vjust = 1) +
      annotate("text", label = paste(hourly_indiv, "individuals"), 
               x = 6.5, y = 0.75, hjust = 1, vjust = 1) +
      scale_fill_viridis_d("Intertidal habitat", option = "inferno", direction = -1) + 
      scale_x_discrete("", limits = rev(levels(h_det$habitat_gen))) + 
      scale_y_continuous("Relative intertidal habitat (weighted by detection time)",
                         limits = c(0, 0.8),
                         breaks = seq(0, 0.8, length.out = 5)) +
      guides(fill = "none") +
      theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
            axis.title.x = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
    out_gg <- cowplot::plot_grid(tide_gg, vs_gg, ncol = 1, axis = "lr", align = "h", rel_heights = c(1,1.5))
    
  } else {
    # Modify tide plot for overlay
    # Tide plot
    tide_gg <- ggplot(tmp, aes(ts_hour, tideHt)) +
      geom_line(color = "gray50", size = 1.5) +
      geom_point(data = current, color = "white", size = 2, stroke = 1.5, shape = 21) +
      geom_hline(aes(yintercept = tide_range[2]), lty = "dashed", size = 1.5, col = "grey80") +
      geom_hline(aes(yintercept = tide_range[1]), lty = "dashed", size = 1.5, col = "grey80") +
      scale_y_continuous("Predicted\nttide height (m)", limits = c(floor(tide_range[1]), ceiling(tide_range[2]))) +
      scale_x_datetime("", breaks = hr_breaks, labels = date_format("%d %b %H:%M"),
                       limits = hr_range, expand = c(0, 0)) +
      theme(panel.border = element_rect(fill = NA, color = "white"),
            panel.background = element_rect(fill = NA), # bg of the panel
            plot.background = element_rect(fill = NA, color = NA), # bg of the plot
            axis.title = element_text(color = "white", size = 9),
            axis.text = element_text(color = "white", size = 6),
            axis.line = element_line(color = "white"),
            axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0, color = "white"),
            axis.title.x = element_blank(),
            axis.ticks = element_line(color = "white"),
            panel.grid.major = element_line(color = "white"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"))
    
    h_det <- filter(det_dat, ts_hour == h) %>%
      ungroup() %>%
      complete(ts_hour = h, ant_avail, 
               fill = list(tot_det_h = 0.004, n_ind = 0)) %>%
      filter(recvSiteName != "Punta Catalina") %>% droplevels() 
    vs_gg <- ggRGB(bg_rast) +
      coord_sf(crs = 4326) +
      geom_tile(data = hab_df, aes(x, y, fill = habitat_gen)) + 
      geom_point(data = h_det, aes(x = det_lon, y = det_lat,
                                   size = tot_det_h, color = tot_det_h), 
                 shape = 21, fill = NA, stroke = 3) +
      scale_size_continuous(guide = "none", limits = c(0.004,54), range = c(4,12)) +
      scale_fill_viridis_d("Intertidal habitat", direction = -1, option = "inferno") + 
      scale_color_viridis_c("Cumulative detection time:", trans = "log2",
                            breaks = c(0.0166666, 0.25, 1, 4, 12, 36, 60),
                            labels = c("1min", "15min", "1h", "4h", "12h", "36h", "60h"),
                            limits = c(0.004, 54),
                            guide = guide_colorbar(barwidth = grid::unit(0.4, "npc"), 
                                                   title.vjust = 0.8,
                                                   title.position = "left")) +
      guides(fill = "none") +
      # Receiving stations locations
      geom_point(data = site_locs, aes(x = recvLon, y = recvLat),
                 pch = 7, color = "white", size = 2.5) +
      # Number of individuals detected
      geom_text(data = h_det, aes(x = det_lon, y = det_lat+0.001, label = n_ind), 
                color = "white", fontface = "bold", size = 3) + 
      annotate("point", x = -69.376211, y = -52.479484, color = "red", pch = 10, size = 3) +
      xlim(c(-69.45, -68.7)) + ylim(c(-52.7, -52.4)) +
      theme_nothing() + 
      theme(legend.position = "bottom")
    
    out_gg <- ggdraw(vs_gg) +
      draw_plot(tide_gg, x = 0.3, y = 0.66, width = 0.7, height = 0.325)
    
  }
  out_file <- sprintf("tmp/gg-tide-%s-%s.png", vs, format(h, format = "%d-%b-%H00"))
  ggsave(out_file, out_gg, width=ifelse(vs == "habitat", 6.5, 5.5), height=ifelse(vs == "habitat", 9, 4.5), dpi=150)
  out_file
}
