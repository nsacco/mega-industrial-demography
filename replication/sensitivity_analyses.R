# =============================================================================
# replication/sensitivity_analyses.R
# Sensitivity analyses: measurement error, prior sensitivity (Bayesian),
# bandwidth sensitivity, and Rambachan-Roth pre-trend sensitivity
# =============================================================================

library(tidyverse)
library(did)
library(here)
library(logger)

source(here("R/functions/utils.R"))
source(here("R/04_models/did_models.R"))

PROC_DIR <- here("data/processed")

# ── 1. Measurement error propagation ──────────────────────────────────────────
#' Population counts are imprecise in small departments; simulate
#' sampling error and propagate through DiD estimates.
sensitivity_measurement_error <- function(panel, n_sims = 200L, cv = 0.05) {
  log_info("Running measurement error sensitivity (n = {n_sims} simulations, CV = {cv}).")

  set.seed(42L)
  results <- map_dfr(seq_len(n_sims), function(i) {
    # Perturb log_pop by additive measurement noise
    panel_noise <- panel |>
      mutate(
        log_pop = log_pop + rnorm(n(), mean = 0, sd = cv * abs(log_pop))
      )

    att_gt  <- estimate_att_gt(panel_noise)
    effects <- did::aggte(att_gt, type = "simple", na.rm = TRUE)

    tibble(sim_id = i, att = effects$overall.att, se = effects$overall.se)
  })

  summary_me <- results |>
    summarise(
      mean_att  = mean(att),
      sd_att    = sd(att),
      q2.5      = quantile(att, 0.025),
      q97.5     = quantile(att, 0.975),
      .groups   = "drop"
    )

  log_info("  ME sensitivity: mean ATT = {round(summary_me$mean_att, 4)}, SD = {round(summary_me$sd_att, 4)}")
  list(draws = results, summary = summary_me)
}

# ── 2. Bayesian prior sensitivity ──────────────────────────────────────────────
#' Test sensitivity of posterior treatment effect estimate to prior specification
sensitivity_priors <- function() {
  if (!requireNamespace("brms", quietly = TRUE)) {
    log_warn("brms not installed. Skipping prior sensitivity.")
    return(NULL)
  }

  prior_specs <- list(
    weakly_informative = c(
      brms::prior(normal(0, 1), class = b),
      brms::prior(normal(8, 2), class = Intercept)
    ),
    vague = c(
      brms::prior(normal(0, 10), class = b),
      brms::prior(normal(0, 10), class = Intercept)
    ),
    regularizing = c(
      brms::prior(normal(0, 0.5), class = b),
      brms::prior(normal(8, 1),   class = Intercept)
    )
  )

  log_info("Running prior sensitivity analysis ({length(prior_specs)} specifications).")

  # Results would be compared on posterior mean and 95% CrI for b_treated_num
  # Full implementation requires loaded data — placeholder structure:
  tibble(
    prior_spec = names(prior_specs),
    note       = "Run with: fit_bayesian_model(dat, priors = prior_specs[[spec]])"
  )
}

# ── 3. Rambachan-Roth pre-trend sensitivity ───────────────────────────────────
#' Assess sensitivity of DiD estimates to violations of parallel trends
#' using the HonestDiD framework (Rambachan & Roth 2023)
sensitivity_honest_did <- function(panel) {
  if (!requireNamespace("HonestDiD", quietly = TRUE)) {
    if (requireNamespace("remotes", quietly = TRUE)) {
      remotes::install_github("asheshrambachan/HonestDiD")
    } else {
      log_warn("HonestDiD not installed. Skipping sensitivity.")
      return(NULL)
    }
  }
  library(HonestDiD)

  log_info("Running Rambachan-Roth sensitivity (HonestDiD).")

  # Load DiD results
  did_res <- readRDS(file.path(PROC_DIR, "did_results.rds"))
  dynamic <- did_res$effects$dynamic

  # Extract pre- and post-period estimates and covariance matrix
  betas  <- dynamic$att.egt
  vcov_m <- dynamic$V.analytical

  # Sensitivity to departures from parallel trends (Mbar = 0.5 → 2)
  sensitivity_results <- map_dfr(
    c(0.5, 1.0, 1.5, 2.0),
    function(mbar) {
      tryCatch({
        cs <- HonestDiD::createSensitivityResults(
          betahat   = betas,
          sigma     = vcov_m,
          numPrePeriods  = sum(dynamic$egt < 0),
          numPostPeriods = sum(dynamic$egt >= 0),
          Mvec      = mbar
        )
        tibble(
          Mbar      = mbar,
          lb        = cs$lb,
          ub        = cs$ub,
          robust_ci = cs$lb < 0 & cs$ub > 0    # contains zero?
        )
      }, error = function(e) {
        tibble(Mbar = mbar, lb = NA, ub = NA, robust_ci = NA)
      })
    }
  )

  log_info("  Sensitivity complete. Estimates remain significant for Mbar up to: {
    max(sensitivity_results$Mbar[!sensitivity_results$robust_ci], na.rm = TRUE)
  }")

  sensitivity_results
}

# ── 4. Bandwidth sensitivity for land-use analysis ───────────────────────────
#' Test forest cover effects at different buffer distances from industrial zones
sensitivity_spatial_bandwidth <- function() {
  bandwidths_km <- c(10, 25, 50, 100, 200)

  spatial <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))

  results <- map_dfr(bandwidths_km, function(bw) {
    # Filter to departments within bandwidth of nearest zone
    if (!"dist_nearest_zone" %in% names(spatial)) {
      return(tibble(bandwidth_km = bw, note = "Distance variable not computed"))
    }

    dat_bw <- spatial |>
      filter(is.na(dist_nearest_zone) |
             (treated & dist_nearest_zone <= bw) |
             (!treated & dist_nearest_zone <= bw * 2))

    # Simple comparison of forest cover means
    summary <- dat_bw |>
      group_by(treated, census_year) |>
      summarise(mean_forest = mean(treecover_pct, na.rm = TRUE), .groups = "drop") |>
      mutate(bandwidth_km = bw)

    summary
  })

  readr::write_csv(results, here("outputs/tables/tbl_sensitivity_bandwidth.csv"))
  results
}

# ── 5. Master sensitivity function ─────────────────────────────────────────────
run_sensitivity_analyses <- function() {
  panel <- prepare_did_panel()

  me_sens     <- sensitivity_measurement_error(panel, n_sims = 200L)
  honest_sens <- sensitivity_honest_did(panel)
  bw_sens     <- sensitivity_spatial_bandwidth()

  # Save
  if (!is.null(me_sens))
    readr::write_csv(me_sens$draws, here("outputs/tables/tbl_sensitivity_me.csv"))
  if (!is.null(honest_sens))
    readr::write_csv(honest_sens, here("outputs/tables/tbl_sensitivity_honest.csv"))

  log_info("✓ Sensitivity analyses complete.")
  list(
    measurement_error = me_sens,
    honest_did        = honest_sens,
    spatial_bandwidth = bw_sens
  )
}

if (sys.nframe() == 0L) run_sensitivity_analyses()
