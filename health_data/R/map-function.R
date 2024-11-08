#' Plot General Health Category maps
#'
#' Plots multiple maps of metrics associated with LSOA level ONS General Health
#' Category responses at the level of an individual Local Authority District.
#' @param plot_data data.frame containing LSOA level plot data subsets for a individual Local Authority.
#' @param out_dir path to the output directory where PNGs of maps will be written to.
#'
#' @return the function is used primarily for it's side effects of plotting a
#' number of plots, combining them and saving as an A4 png to the directory specified
#' by `output_dir`. The output file name for each PNG matches the pattern:
#' `<LSOA_code>_<LSOA_name>_<date_created>.png`
plot_lad_map <- function(plot_data, out_dir) {
  tictoc::tic()

  lad_code <- unique(plot_data$lad_code)
  lad_name <- unique(plot_data$lad_name)

  pid <- Sys.getpid()
  node <- system2("hostname", stdout = TRUE)

  # Create facet plot maps of percentage occurrence of each category across each LSOA.
  plt_1 <- plot_data %>%
    ggplot() +
    geom_sf(aes(fill = obs_perc), colour = "white") +
    facet_wrap(~gen_health_cat, ncol = 1) +
    scale_fill_viridis_c(name = "", option = "C") +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.key.height = unit(5, units = "native"),
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 10),
      strip.background = element_rect(fill = "white", colour = "white"),
      strip.text.x = element_text(colour = "gray40", face = "bold", size = 10, hjust = 0)
    )

  # Create deviation from national mean percentage occurrence (z-score) facet plot maps
  # of each category across each LSOA.
  plt_2 <- plot_data %>%
    ggplot() +
    geom_sf(aes(fill = z_score), colour = "gray") +
    scale_fill_continuous_diverging(
      palette = "Blue-Red 3",
      name = ""
    ) +
    facet_wrap(~gen_health_cat, ncol = 1) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.key.height = unit(5, units = "native"),
      strip.background = element_rect(fill = "white", colour = "white"),
      strip.text.x = element_text(colour = "white", face = "bold", size = 10, hjust = 0)
    )


  # Create map of category of maximum occurrence across each LSOA and scale opacity
  # according to maximum category occurence.
  plot_main <- plot_data %>%
    group_by(lsoa_code) %>%
    summarise(
      max_cat = head(gen_health_cat[obs_perc == max(obs_perc)], 1),
      obs_max_perc = max(obs_perc)
    ) %>%
    ggplot() +
    geom_sf(aes(fill = max_cat, alpha = obs_max_perc), colour = "white") +
    scale_fill_viridis_d(name = "", option = "C") +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(alpha = "none")


  # Arrange plots
  plot <- ggarrange(plt_1, plt_2, plot_main,
    labels = c("A", "B", "C"),
    ncol = 3, nrow = 1, widths = c(1, 1, 4)
  )

  # Add plot annotation
  plot <- annotate_figure(plot,
    top = text_grob(glue::glue("General Health Category Data in LDA {unique(plot_data$lad_name)} ({unique(plot_data$lad_code)})"),
      face = "bold",
      size = 20,
      just = c("left", "bottom"), x = 0, y = 0
    ),
    bottom = text_grob(
      paste("A) Percentage occurrence of General health category",
        "B) Deviation of percentage occurrence from General Health Category National Mean (z-score)",
        "C) Most common health category (opacity reflects percentage occurrence of top category)",
        sep = "\n"
      ),
      just = c("left", "top"), x = 0.01, y = 0.99, face = "italic", size = 10, lineheight = 1.15
    )
  )
  file_name <- glue::glue("{lad_code}_{janitor::make_clean_names(lad_name)}_{Sys.Date()}.png")
  out_path <- fs::path(file_name, ext = "png")

  # Save combined plot
  ggsave(
    filename = file_name,
    plot = plot,
    device = "png",
    path = out_dir,
    width = 29.7, height = 21, units = "cm",
    bg = "white"
  )

  cli::cli_alert_success(c(
    "Map for LAD {.val {lad_code} {lad_name}} completed ",
    "successfuly on PID {.val {pid}} on {node} ",
    "({tictoc::toc(quiet = TRUE)$callback_msg})"
  ))
}
