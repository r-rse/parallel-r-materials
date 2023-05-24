# Load Libraries ----
library(sf)
library(dplyr)
library(assertr)
library(ggplot2)
library(colorspace)
library(ggpubr)
library(furrr)

# Source function ----
source(here::here("health_data", "R", "map-function.R"))

# Prepare output directory ----
out_dir <- here::here("health_data", "outputs", "maps")
fs::dir_create(out_dir)

# Load data ----
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

# Merge data and validate ----
all_data <- left_join(health_data, look_up,
                      relationship = "many-to-one") %>%
    left_join(boundaries, relationship = "many-to-one") %>%
    st_as_sf() %>%
    assert(not_na, lsoa_code, lsoa_name, lad_name, lad_code, geometry) %>%
    verify(inherits(., "sf"))

# Process data ----
# Get ordered health category levels
health_cat_levels <- health_data %>%
    select(gen_health_code, gen_health_cat) %>%
    distinct() %>%
    arrange(gen_health_code) %>%
    pull(gen_health_cat)

# Create addition variables obs_perc & z_score, cast gen_health_cat to factor
all_data <- all_data %>%
    mutate(gen_health_cat = factor(gen_health_cat,
                                   levels = health_cat_levels)) %>%
    group_by(lsoa_code) %>%
    mutate(obs_perc = observation / sum(observation) * 100) %>%
    ungroup() %>%
    group_by(gen_health_cat) %>%
    mutate(z_score = (obs_perc - mean(obs_perc))/sd(obs_perc)) %>%
    ungroup()

# Split data
split_data <- split(all_data, f = all_data$lad_code)



# Create maps ----
# Create iteration indexes
idx_start <- 1
idx_end <- 5

# Iterating plotting function over each LAD plot datasets in idx_start:idx_end
# range using purrr::walk. Time it with tictoc package
#
# ITERATE USING purrr::walk
tictoc::tic()
purrr::walk(
    split_data[idx_start:idx_end],
    ~ plot_lad_map(.x, out_dir)
)
tictoc::toc()