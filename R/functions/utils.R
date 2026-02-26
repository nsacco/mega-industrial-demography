# =============================================================================
# functions/utils.R
# Shared utility functions across the entire pipeline
# =============================================================================

library(tidyverse)
library(here)
library(glue)

# ── 1. NULL-coalescing operator ───────────────────────────────────────────────
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── 2. Geographic crosswalk helpers ─────────────────────────────────────────
#' Load Argentina department-to-province mapping
load_prov_names <- function() {
  tribble(
    ~geo_prov_code, ~prov_name,
    "02", "Ciudad Autónoma de Buenos Aires",
    "06", "Buenos Aires",
    "10", "Catamarca",
    "14", "Córdoba",
    "18", "Corrientes",
    "22", "Chaco",
    "26", "Chubut",
    "30", "Entre Ríos",
    "34", "Formosa",
    "38", "Jujuy",
    "42", "La Pampa",
    "46", "La Rioja",
    "50", "Mendoza",
    "54", "Misiones",
    "58", "Neuquén",
    "62", "Río Negro",
    "66", "Salta",
    "70", "San Juan",
    "74", "San Luis",
    "78", "Santa Cruz",
    "82", "Santa Fe",
    "86", "Santiago del Estero",
    "90", "Tucumán",
    "94", "Tierra del Fuego"
  )
}

#' Pad province/department codes to standard width
pad_geo_code <- function(x, width = 2L) {
  stringr::str_pad(as.character(x), width = width, pad = "0")
}

# ── 3. Census interval utilities ─────────────────────────────────────────────
#' Map census years to their interval labels
census_interval_label <- function(years) {
  intervals <- c(
    `1970` = "pre-1991",
    `1991` = "1991",
    `2001` = "1991-2001",
    `2010` = "2001-2010",
    `2022` = "2010-2022"
  )
  intervals[as.character(years)]
}

#' Compute annual growth rate between two population values
agr <- function(pop_t, pop_t0, t_diff) {
  (pop_t / pop_t0)^(1 / t_diff) - 1
}

# ── 4. Uncertainty quantification helpers ─────────────────────────────────────
#' Compute 95% credible interval from a vector of draws
ci95 <- function(x, ...) {
  quantile(x, probs = c(0.025, 0.500, 0.975), na.rm = TRUE, ...)
}

#' Summarize posterior draws to mean + 95% CI
summarize_posterior <- function(draws, var_name = "theta") {
  tibble(
    parameter = var_name,
    mean      = mean(draws, na.rm = TRUE),
    sd        = sd(draws, na.rm = TRUE),
    q2.5      = quantile(draws, 0.025, na.rm = TRUE),
    q50       = quantile(draws, 0.500, na.rm = TRUE),
    q97.5     = quantile(draws, 0.975, na.rm = TRUE),
    prob_pos  = mean(draws > 0, na.rm = TRUE)
  )
}

# ── 5. Table formatting ───────────────────────────────────────────────────────
#' Format a regression coefficient table for publication
fmt_coef_table <- function(model, digits = 3L, ...) {
  broom::tidy(model, conf.int = TRUE, ...) |>
    mutate(
      across(where(is.double), ~ round(.x, digits)),
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      ),
      estimate_fmt = paste0(estimate, stars)
    )
}

#' Significance stars
sig_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE      ~ ""
  )
}

# ── 6. File I/O helpers ───────────────────────────────────────────────────────
#' Safely read a parquet file, returning NULL if not found
safe_read_parquet <- function(path, ...) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(NULL)
  }
  arrow::read_parquet(path, ...)
}

#' Print a human-readable file size
human_size <- function(path) {
  utils::object.size(readBin(path, "raw", file.info(path)$size)) |>
    format(units = "auto")
}

# ── 7. Logging helpers ────────────────────────────────────────────────────────
log_section <- function(title) {
  rule <- paste(rep("─", 60L), collapse = "")
  message("\n", rule)
  message("  ", title)
  message(rule)
}

# ── 8. Spatial utilities ─────────────────────────────────────────────────────
#' Compute pairwise great-circle distances between province centroids (km)
compute_pairwise_distances <- function(boundaries) {
  centroids <- sf::st_centroid(boundaries)
  dist_mat  <- sf::st_distance(centroids)
  units::set_units(dist_mat, "km") |>
    as.matrix() |>
    `dimnames<-`(list(boundaries$geo_prov_code, boundaries$geo_prov_code)) |>
    as_tibble(rownames = "origin") |>
    pivot_longer(-origin, names_to = "destination", values_to = "dist_km") |>
    mutate(dist_km = as.numeric(dist_km))
}

#' Test if a point (lat/lon) falls within any industrial zone polygon
within_zone <- function(pts_sf, zones_sf) {
  sf::st_within(pts_sf, zones_sf, sparse = FALSE) |>
    apply(1, any)
}

# ── 9. Reproducibility helpers ─────────────────────────────────────────────────
set_seed_all <- function(seed = 42L) {
  set.seed(seed)
  if (requireNamespace("brms", quietly = TRUE)) {
    options(brms.seed = seed)
  }
}

session_snapshot <- function(path = here("outputs/session_info.txt")) {
  writeLines(capture.output(sessionInfo()), path)
  message("Session info saved to: ", path)
}
