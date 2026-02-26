# =============================================================================
# 04_models/spatial_models.R
# Spatial econometrics: SAR, SEM, SDM, SARAR; spillover decomposition
# =============================================================================

library(tidyverse)
library(spdep)
library(spatialreg)
library(sf)
library(arrow)
library(here)
library(logger)

source(here("R/functions/utils.R"))

PROC_DIR <- here("data/processed")

# ── 1. Load data and spatial weights ─────────────────────────────────────────
load_spatial_data <- function() {
  panel   <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))
  census  <- arrow::read_parquet(file.path(PROC_DIR, "census_dept_counts.parquet")) |>
    group_by(geo_dept_code, census_year) |>
    summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop")

  lw <- readRDS(file.path(PROC_DIR, "spatial_weights.rds"))

  dat <- panel |>
    left_join(census, by = c("geo_dept_code", "census_year")) |>
    mutate(
      log_pop      = log(pop + 1),
      log_pop_lag  = lag(log_pop),
      treated_num  = as.integer(treated)
    )

  list(dat = dat, lw = lw)
}

# ── 2. Moran's I tests ───────────────────────────────────────────────────────
#' Test for spatial autocorrelation in outcomes and residuals
run_morans_tests <- function(dat, lw) {
  results <- map_dfr(unique(dat$census_year), function(yr) {
    dat_yr <- dat |> filter(census_year == yr) |> arrange(geo_dept_code)
    if (nrow(dat_yr) < 10) return(NULL)

    moran_pop <- spdep::moran.test(dat_yr$log_pop, lw, zero.policy = TRUE)

    tibble(
      census_year     = yr,
      moran_I         = moran_pop$estimate["Moran I statistic"],
      moran_expected  = moran_pop$estimate["Expectation"],
      moran_variance  = moran_pop$estimate["Variance"],
      moran_p         = moran_pop$p.value
    )
  })
  results
}

# ── 3. Spatial lag/error model selection (LM tests) ─────────────────────────
run_lm_tests <- function(dat, lw, formula_str, yr) {
  dat_yr <- dat |> filter(census_year == yr) |> arrange(geo_dept_code)
  ols    <- lm(as.formula(formula_str), data = dat_yr)
  spdep::lm.LMtests(ols, lw, test = "all", zero.policy = TRUE)
}

# ── 4. Spatial Autoregressive Model (SAR / Spatial Lag) ─────────────────────
#' Spatial lag model: y = ρWy + Xβ + ε
estimate_sar <- function(dat, lw, yr) {
  dat_yr <- dat |> filter(census_year == yr) |> arrange(geo_dept_code)
  log_info("Estimating SAR model for year {yr}")

  spatialreg::lagsarlm(
    formula    = log_pop ~ treated_num + treecover_pct + log_pop_lag,
    data       = dat_yr,
    listw      = lw,
    zero.policy = TRUE
  )
}

# ── 5. Spatial Error Model (SEM) ─────────────────────────────────────────────
#' Spatial error model: y = Xβ + u; u = λWu + ε
estimate_sem <- function(dat, lw, yr) {
  dat_yr <- dat |> filter(census_year == yr) |> arrange(geo_dept_code)
  log_info("Estimating SEM model for year {yr}")

  spatialreg::errorsarlm(
    formula    = log_pop ~ treated_num + treecover_pct + log_pop_lag,
    data       = dat_yr,
    listw      = lw,
    zero.policy = TRUE
  )
}

# ── 6. Spatial Durbin Model (SDM) ────────────────────────────────────────────
#' Spatial Durbin: y = ρWy + Xβ + WXθ + ε
#' Used for indirect (spillover) effect identification
estimate_sdm <- function(dat, lw, yr) {
  dat_yr <- dat |> filter(census_year == yr) |> arrange(geo_dept_code)
  log_info("Estimating SDM model for year {yr}")

  spatialreg::lagsarlm(
    formula    = log_pop ~ treated_num + treecover_pct + log_pop_lag,
    data       = dat_yr,
    listw      = lw,
    type       = "Durbin",
    zero.policy = TRUE
  )
}

# ── 7. Spillover decomposition ────────────────────────────────────────────────
#' Decompose total effect into direct, indirect (spillover), and total
#'
#' @param model spatialreg model object (SAR or SDM)
#' @param lw listw object
#' @param R Integer. Simulations for variance estimation
decompose_spillovers <- function(model, lw, R = 200L) {
  log_info("Decomposing direct/indirect/total effects (R = {R} simulations).")
  spatialreg::impacts(model, listw = lw, R = R, zstats = TRUE, short = FALSE)
}

# ── 8. Spatial panel (fixed effects + spatial autocorrelation) ───────────────
#' Panel spatial model across all years (Elhorst 2014 specification)
#' Uses splm package
estimate_spatial_panel <- function(dat, lw) {
  if (!requireNamespace("splm", quietly = TRUE)) {
    log_warn("splm not installed. Install with install.packages('splm').")
    return(NULL)
  }

  log_info("Estimating spatial panel model (SAR + time/entity FE).")
  splm::spml(
    formula = log_pop ~ treated_num + treecover_pct,
    data    = dat,
    index   = c("geo_dept_code", "census_year"),
    listw   = lw,
    model   = "within",
    spatial.error = "b",   # Baltagi et al.
    lag     = TRUE,
    zero.policy = TRUE
  )
}

# ── 9. Master function ───────────────────────────────────────────────────────
run_spatial_models <- function() {
  spdat  <- load_spatial_data()
  dat    <- spdat$dat
  lw     <- spdat$lw

  years  <- c(1991, 2001, 2010, 2022)

  # Moran tests
  morans <- run_morans_tests(dat, lw)
  log_info("Moran's I summary:\n{capture.output(print(morans))}")

  # Cross-sectional models per census round
  models <- map(
    set_names(years, as.character(years)),
    ~ list(
      sar = estimate_sar(dat, lw, .x),
      sem = estimate_sem(dat, lw, .x),
      sdm = estimate_sdm(dat, lw, .x)
    )
  )

  # Spillover decomposition for SDM (most recent year)
  spillovers <- decompose_spillovers(models[["2022"]]$sdm, lw)

  # Save
  saveRDS(list(morans = morans, models = models, spillovers = spillovers),
          file.path(PROC_DIR, "spatial_model_results.rds"))

  log_info("✓ Spatial models complete.")
  list(morans = morans, models = models, spillovers = spillovers)
}

if (sys.nframe() == 0L) run_spatial_models()
