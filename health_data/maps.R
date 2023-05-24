library(sf)
library(dplyr)
library(assertr)
library(ggplot2)
library(colorspace)
library(ggpubr)
library(furrr)
library(future)


health_data <- readr::read_csv(
    here::here("health_data", "data",
               "lsoa-general_health.csv"))

look_up <- readr::read_csv(
    here::here("health_data", "data",
               "output_area_lookup.csv"))

boundaries <- read_sf(
    here::here("health_data", "data",
               "lsoa_boundaries.geojson")
)

health_cat_levels <- health_data %>%
    select(gen_health_code, gen_health_cat) %>%
    distinct() %>%
    arrange(gen_health_code) %>%
    pull(gen_health_cat)

all_data <- left_join(health_data, look_up,
                      relationship = "many-to-one") %>%
    left_join(boundaries, relationship = "many-to-one") %>%
    mutate(gen_health_cat = factor(gen_health_cat,
                                   levels = health_cat_levels)) %>%
    st_as_sf() %>%
    assert(not_na, lsoa_code, lsoa_name, lad_name, lad_code, geometry) %>%
    verify(inherits(., "sf")) %>%
    group_by(lsoa_code) %>%
    mutate(obs_perc = observation / sum(observation) * 100) %>%
    ungroup() %>%
    group_by(gen_health_cat) %>%
    mutate(z_score = (obs_perc - mean(obs_perc))/sd(obs_perc)) %>%
    ungroup()

out_dir <- here::here("health_data", "outputs", "maps")
fs::dir_create(out_dir)





split_data <- split(all_data, f = all_data$lad_code)

tictoc::tic()
plan(sequential)
furrr::future_walk(
    split_data[1:20],
    ~ plot_lad_map(.x, out_dir),
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
)
tictoc::toc()


tictoc::tic()
plan(multisession)
furrr::future_walk(
    split_data[1:20],
    ~ plot_lad_map(.x, out_dir),
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
)
tictoc::toc()




lad_codes <- unique(all_data$lad_code)

lad_code <- lad_codes[1]
lad_code <- "E06000002"


plot_data <- filter(all_data, .data$lad_code == .env$lad_code)

plot_lad_map <- function(plot_data, out_dir) {

    lad_code <- unique(plot_data$lad_code)
    lad_name <- unique(plot_data$lad_name)

    plt_1 <- plot_data %>%
        ggplot() +
        geom_sf(aes(fill = obs_perc), colour = "white") +
        facet_wrap(~gen_health_cat, ncol = 1) +
        scale_fill_viridis_c(name = "", option = "C") +
        theme_light() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.key.height = unit(5, units = "native"),
              legend.position = "bottom",
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 10),
              strip.background = element_rect(fill = "white", colour = "white"),
              strip.text.x = element_text(colour = "gray40", face = "bold", size = 10, hjust = 0))

    plt_2 <- plot_data %>%
        ggplot() +
        geom_sf(aes(fill = z_score), colour = "gray") +
        scale_fill_continuous_diverging(palette = "Blue-Red 3",
                                        name = ""
        ) +
        facet_wrap(~gen_health_cat, ncol = 1) +
        theme_light() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position="bottom",
              legend.text = element_text(size = 6),
              legend.key.height = unit(5, units = "native"),
              strip.background = element_rect(fill = "white", colour = "white"),
              strip.text.x = element_text(colour = "white", face = "bold", size = 10, hjust = 0))



    plot_main <- plot_data %>%
        group_by(lsoa_code) %>%
        summarise(max_cat = head(gen_health_cat[obs_perc == max(obs_perc)], 1),
                  obs_max_perc = max(obs_perc)) %>%
        ggplot() +
        geom_sf(aes(fill = max_cat, alpha = obs_max_perc), colour = "white") +
        scale_fill_viridis_d(name = "",  option = "C") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position="bottom") +
        guides(alpha = "none")


    plot <- ggarrange(plt_1, plt_2, plot_main,
                      labels = c("A", "B", "C"),
                      ncol = 3, nrow = 1, widths = c(1, 1, 4))

    plot <- annotate_figure(plot,
                            top = text_grob(glue::glue("General Health Category Data in LDA {unique(plot_data$lad_name)} ({unique(plot_data$lad_code)})"),
                                            face = "bold",
                                            size = 20,
                                            just = c("left", "bottom"), x = 0, y = 0),
                            bottom = text_grob(
                                paste("A) Percentage occurrence of General health category",
                                      "B) Deviation of percentage occurence from General Health Category National Mean (z-score)",
                                      "C) Most common health category (opacity reflects percentage occurence of top category)",
                                      sep = ", "),
                                just = c("left", "top"), x = 0, y = 1, face = "italic", size = 10, lineheight = 1.15)
    )
    file_name <- glue::glue("{lad_code}_{janitor::make_clean_names(lad_name)}_{Sys.Date()}.png")
    out_path <- fs::path(file_name, ext = "png")

    ggsave(filename = file_name,
           plot = plot,
           device = "png",
           path = out_dir,
           width = 29.7, height = 21, units = "cm",
           bg = "white")

    cli::cli_alert_success("Map for LAD {.val {lad_code} {lad_name}} completed successfuly on PID {.val {Sys.getpid()}}")
}


for (lsoa in plot_data$lsoa_code) {
    groub_data <- filter(plot_data, lsoa_code == lsoa) %>%
        arrange(lsoa_code)

    max_cat <- groub_data$gen_health_cat[groub_data$obs_perc == max(groub_data$obs_perc)]
    max_perc <- max(groub_data$obs_perc)

    cli::cli_alert_info("{.var {lsoa}}: max_cat: {.var {max_cat}} max_perc: {.var {max_perc}}")
    tictoc::toc()

}
