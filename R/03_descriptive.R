# =============================================================================
# 03_descriptive.R
# Exploratory analysis: summary statistics, population pyramids, growth rates
# Produces: outputs/figures/descriptive_*.png, outputs/tables/descriptive_*.tex
# =============================================================================

library(tidyverse)
library(arrow)
library(ggplot2)
library(patchwork)
library(scales)
library(viridis)
library(here)
library(logger)

source(here("R/functions/utils.R"))
source(here("R/06_visualization.R"))

PROC_DIR <- here("data/processed")
FIG_DIR  <- here("outputs/figures")
TAB_DIR  <- here("outputs/tables")

# ── 1. Load harmonized data ───────────────────────────────────────────────────
load_data <- function() {
  list(
    micro      = arrow::read_parquet(file.path(PROC_DIR, "census_microdata_harmonized.parquet")),
    dept       = arrow::read_parquet(file.path(PROC_DIR, "census_dept_counts.parquet")),
    spatial    = arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))
  )
}

# ── 2. Population growth rates ───────────────────────────────────────────────
#' Compute annual growth rate between census rounds
compute_growth_rates <- function(dept) {
  dept |>
    group_by(geo_dept_code, census_year) |>
    summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop") |>
    arrange(geo_dept_code, census_year) |>
    group_by(geo_dept_code) |>
    mutate(
      pop_lag    = lag(pop),
      year_lag   = lag(census_year),
      years_diff = census_year - year_lag,
      agr        = (pop / pop_lag)^(1 / years_diff) - 1  # Annual growth rate
    ) |>
    filter(!is.na(agr)) |>
    ungroup()
}

# ── 3. Population pyramid ────────────────────────────────────────────────────
plot_pyramid <- function(micro, year, title = NULL) {
  pyramid_data <- micro |>
    filter(census_year == year, !is.na(sex_label), !is.na(age_group5)) |>
    group_by(sex_label, age_group5) |>
    summarise(pop = sum(hh_weight, na.rm = TRUE), .groups = "drop") |>
    mutate(
      pop = if_else(sex_label == "Male", -pop, pop)
    )

  ggplot(pyramid_data, aes(x = pop, y = age_group5, fill = sex_label)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(
      labels = function(x) comma(abs(x)),
      name = "Population"
    ) +
    scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#d6604d")) +
    labs(
      title = title %||% glue::glue("Population Pyramid — Argentina {year}"),
      y     = "Age Group",
      fill  = NULL
    ) +
    theme_industrial() +
    theme(legend.position = "bottom")
}

# ── 4. Treatment intensity map ────────────────────────────────────────────────
plot_treatment_map <- function(spatial, boundaries, year) {
  data <- spatial |>
    filter(census_year == year) |>
    left_join(boundaries |> select(geo_dept_code), by = "geo_dept_code") |>
    sf::st_as_sf()

  ggplot(data, aes(fill = as.factor(treated))) +
    geom_sf(color = "white", size = 0.1) +
    scale_fill_manual(
      values = c("FALSE" = "#f7f7f7", "TRUE" = "#d73027"),
      labels = c("Not treated", "Industrial zone"),
      name   = NULL
    ) +
    labs(
      title    = glue::glue("Industrial Promotion Zones — {year}"),
      subtitle = "Departments with active industrial promotion legislation"
    ) +
    theme_industrial_map()
}

# ── 5. Growth rate distribution by treatment status ──────────────────────────
plot_growth_by_treatment <- function(growth_rates, spatial) {
  dat <- growth_rates |>
    left_join(
      spatial |> select(geo_dept_code, census_year, treated),
      by = c("geo_dept_code", "census_year")
    )

  ggplot(dat, aes(x = agr, fill = treated)) +
    geom_density(alpha = 0.6, color = NA) +
    facet_wrap(~ census_year, scales = "free_y") +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_fill_manual(
      values = c("FALSE" = "#4393c3", "TRUE" = "#d73027"),
      labels = c("Control", "Treated"),
      name   = "Industrial Zone"
    ) +
    labs(
      title    = "Annual Population Growth Rate Distribution by Treatment Status",
      x        = "Annual Growth Rate",
      y        = "Density"
    ) +
    theme_industrial()
}

# ── 6. Summary statistics table ──────────────────────────────────────────────
make_summary_table <- function(growth_rates, spatial) {
  growth_rates |>
    left_join(
      spatial |> select(geo_dept_code, census_year, treated, n_zones, treecover_pct),
      by = c("geo_dept_code", "census_year")
    ) |>
    group_by(census_year, treated) |>
    summarise(
      n_depts         = n(),
      mean_agr        = mean(agr, na.rm = TRUE),
      sd_agr          = sd(agr, na.rm = TRUE),
      median_agr      = median(agr, na.rm = TRUE),
      mean_pop        = mean(pop, na.rm = TRUE),
      mean_treecover  = mean(treecover_pct, na.rm = TRUE),
      .groups = "drop"
    )
}

# ── 7. Master function ────────────────────────────────────────────────────────
run_descriptive <- function() {
  log_info("Running descriptive analysis.")
  dat <- load_data()

  growth_rates <- compute_growth_rates(dat$dept)

  # Pyramids for all rounds
  walk(c(1970, 1991, 2001, 2010, 2022), function(yr) {
    p <- plot_pyramid(dat$micro, yr)
    ggsave_industrial(p, file.path(FIG_DIR, glue::glue("fig_pyramid_{yr}.png")))
  })

  # Growth distribution
  p_growth <- plot_growth_by_treatment(growth_rates, dat$spatial)
  ggsave_industrial(p_growth, file.path(FIG_DIR, "fig_growth_distribution.png"))

  # Summary table
  summ_tab <- make_summary_table(growth_rates, dat$spatial)
  readr::write_csv(summ_tab, file.path(TAB_DIR, "tbl_summary_stats.csv"))

  log_info("✓ Descriptive analysis complete.")
  list(growth_rates = growth_rates, summary = summ_tab)
}

if (sys.nframe() == 0L) run_descriptive()
