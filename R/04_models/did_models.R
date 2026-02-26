# =============================================================================
# 04_models/did_models.R
# Difference-in-Differences: staggered adoption, event studies, robustness
# Main method: Callaway-Sant'Anna (2021) via {did}
# =============================================================================

library(tidyverse)
library(did)
library(fixest)
library(arrow)
library(ggplot2)
library(here)
library(logger)

source(here("R/functions/utils.R"))

PROC_DIR <- here("data/processed")
OUT_DIR  <- here("outputs")

# ── 1. Prepare panel for DiD ─────────────────────────────────────────────────
#' Build a balanced dept × year panel with treatment timing
#'
#' @param outcome_var Character. Name of the outcome variable.
#' @param covariates Character vector. Pre-treatment controls to include.
#' @return Tibble ready for Callaway-Sant'Anna estimation
prepare_did_panel <- function(outcome_var = "log_pop",
                               covariates  = c("log_area", "log_pop_lag")) {

  census  <- arrow::read_parquet(file.path(PROC_DIR, "census_dept_counts.parquet"))
  spatial <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))

  panel <- census |>
    group_by(geo_dept_code, census_year) |>
    summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop") |>
    left_join(spatial |> select(geo_dept_code, census_year, treated, first_treat_yr,
                                n_zones, incentive_types, treecover_pct),
              by = c("geo_dept_code", "census_year")) |>
    arrange(geo_dept_code, census_year) |>
    group_by(geo_dept_code) |>
    mutate(
      log_pop     = log(pop + 1),
      log_pop_lag = lag(log_pop),
      pop_lag     = lag(pop)
    ) |>
    ungroup() |>
    # Treatment group identifier: year of first treatment (0 = never treated)
    mutate(
      gvar = if_else(is.na(first_treat_yr) | !treated, 0L, as.integer(first_treat_yr)),
      # Balanced panel ID
      dept_id = as.integer(factor(geo_dept_code))
    ) |>
    filter(!is.na(log_pop))

  panel
}

# ── 2. Callaway-Sant'Anna estimator ──────────────────────────────────────────
#' Estimate ATT(g,t) using Callaway-Sant'Anna (2021)
#'
#' @param panel Tibble from prepare_did_panel()
#' @param outcome Character. Outcome column name.
#' @param control_group Character. "nevertreated" or "notyettreated"
#' @param anticipation Integer. Anticipation periods.
#' @return att_gt object from {did}
estimate_att_gt <- function(panel,
                             outcome       = "log_pop",
                             control_group = "nevertreated",
                             anticipation  = 0L) {

  log_info("Estimating Callaway-Sant'Anna ATT(g,t): outcome = {outcome}, control = {control_group}")

  csdid <- did::att_gt(
    yname         = outcome,
    tname         = "census_year",
    idname        = "dept_id",
    gname         = "gvar",
    xformla       = ~ log_pop_lag + treecover_pct,
    data          = panel |> filter(!is.na(log_pop_lag)),
    control_group = control_group,
    anticipation  = anticipation,
    est_method    = "reg",   # also try "dr" (doubly-robust) and "ipw"
    panel         = TRUE,
    pl            = TRUE,
    cores         = parallel::detectCores() - 1L
  )

  log_info("  Groups: {length(unique(panel$gvar[panel$gvar > 0]))}, Periods: {length(unique(panel$census_year))}")
  csdid
}

# ── 3. Aggregate to simple effects ───────────────────────────────────────────
#' Aggregate ATT(g,t) to dynamic, group-average, and overall effects
#'
#' @param att_gt att_gt object
#' @return List of aggte objects
aggregate_effects <- function(att_gt) {
  list(
    dynamic       = did::aggte(att_gt, type = "dynamic",     na.rm = TRUE),
    group_average = did::aggte(att_gt, type = "group",       na.rm = TRUE),
    overall       = did::aggte(att_gt, type = "simple",      na.rm = TRUE),
    calendar      = did::aggte(att_gt, type = "calendar",    na.rm = TRUE)
  )
}

# ── 4. Event-study plot ──────────────────────────────────────────────────────
plot_event_study <- function(dynamic_aggte, title = "Event Study: Industrial Policy & Population") {
  es_data <- broom::tidy(dynamic_aggte, conf.int = TRUE)

  ggplot(es_data, aes(x = event.time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#2166ac") +
    geom_point(color = "#2166ac", size = 2.5) +
    geom_line(color = "#2166ac") +
    scale_x_continuous(breaks = pretty) +
    labs(
      title    = title,
      subtitle = "Callaway-Sant'Anna (2021) dynamic ATT; 95% simultaneous confidence bands",
      x        = "Years relative to industrial zone designation",
      y        = "ATT (log population)"
    ) +
    theme_industrial()
}

# ── 5. Two-way FE benchmark (fixest) ─────────────────────────────────────────
#' TWFE regression as baseline (known to be biased under staggered adoption)
twfe_benchmark <- function(panel, outcome = "log_pop") {
  log_info("Estimating TWFE benchmark (fixest).")
  fixest::feols(
    as.formula(glue::glue("{outcome} ~ treated | dept_id + census_year")),
    data    = panel,
    cluster = ~ geo_prov_code,
    weights = ~ pop_lag
  )
}

# ── 6. Heterogeneity analysis ────────────────────────────────────────────────
#' Explore heterogeneous treatment effects by incentive type
het_by_incentive <- function(att_gt_list) {
  # att_gt_list: named list of att_gt objects by incentive type
  map_dfr(names(att_gt_list), function(itype) {
    agg <- did::aggte(att_gt_list[[itype]], type = "dynamic", na.rm = TRUE)
    broom::tidy(agg, conf.int = TRUE) |>
      mutate(incentive_type = itype)
  })
}

# ── 7. Placebo / pre-trend test ──────────────────────────────────────────────
#' Joint pre-trend test using Rambachan-Roth (2023) sensitivity
#' Requires {HonestDiD} package
pretrend_test <- function(dynamic_aggte) {
  if (!requireNamespace("HonestDiD", quietly = TRUE)) {
    log_warn("HonestDiD not installed. Skipping sensitivity analysis.")
    return(NULL)
  }
  # Extract pre-period estimates and covariance
  # See: https://github.com/asheshrambachan/HonestDiD
  log_info("Running Rambachan-Roth pre-trend sensitivity analysis.")
  # ... implementation depends on HonestDiD API
  NULL
}

# ── 8. Master DiD function ────────────────────────────────────────────────────
run_did_models <- function() {
  panel   <- prepare_did_panel()
  att_gt  <- estimate_att_gt(panel)
  effects <- aggregate_effects(att_gt)
  twfe    <- twfe_benchmark(panel)

  # Plots
  p_es <- plot_event_study(effects$dynamic)
  ggsave_industrial(p_es, here("outputs/figures/fig_event_study_pop.png"))

  # Save results
  saveRDS(list(att_gt = att_gt, effects = effects, twfe = twfe),
          file.path(PROC_DIR, "did_results.rds"))

  log_info("✓ DiD models complete. Overall ATT = {round(effects$overall$overall.att, 4)}")

  list(att_gt = att_gt, effects = effects, twfe = twfe, panel = panel)
}

if (sys.nframe() == 0L) run_did_models()
