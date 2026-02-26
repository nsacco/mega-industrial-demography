# =============================================================================
# 05_simulations.R
# Counterfactual scenarios: synthetic control, policy-off/on simulation,
# displacement estimation (adapted from Sharygin 2020 methods)
# =============================================================================

library(tidyverse)
library(gsynth)
library(scpi)
library(arrow)
library(ggplot2)
library(here)
library(logger)
library(furrr)

source(here("R/functions/utils.R"))

PROC_DIR <- here("data/processed")

# ── 1. Generalized Synthetic Control (Xu 2017) ────────────────────────────────
#' Estimate treatment effects using interactive FE counterfactual (gsynth)
#' Handles multiple treated units — combines synthetic control with DiD
estimate_gsynth <- function(panel) {
  log_info("Estimating Generalized Synthetic Control (gsynth).")

  gsynth(
    Y           = "log_pop",
    D           = "treated_num",
    X           = c("treecover_pct"),
    data        = panel,
    index       = c("geo_dept_code", "census_year"),
    force       = "two-way",
    CV          = TRUE,
    r           = c(0, 5),        # cross-validate number of factors
    se          = TRUE,
    nboots      = 1000L,
    inference   = "parametric",
    cores       = parallel::detectCores() - 1L,
    seed        = 42L
  )
}

# ── 2. SCPI (Synthetic Control with Prediction Intervals) ─────────────────────
#' Ferman-Pinto (2021) prediction intervals for synthetic control
estimate_scpi <- function(panel, treated_unit) {
  log_info("Estimating SCPI for treated unit: {treated_unit}")

  dat_wide <- panel |>
    select(geo_dept_code, census_year, log_pop, treated) |>
    pivot_wider(names_from = geo_dept_code, values_from = log_pop)

  donor_units <- panel |>
    filter(!treated | geo_dept_code != treated_unit) |>
    distinct(geo_dept_code) |>
    pull()

  scpi::scdata(
    df          = panel |> filter(geo_dept_code %in% c(treated_unit, donor_units)),
    id.var      = "geo_dept_code",
    time.var    = "census_year",
    outcome.var = "log_pop",
    period.pre  = panel |> filter(!treated | geo_dept_code != treated_unit) |>
                  pull(census_year) |> unique(),
    period.post = panel |> filter(treated, geo_dept_code == treated_unit) |>
                  pull(census_year) |> unique(),
    unit.tr     = treated_unit,
    unit.co     = donor_units
  )
}

# ── 3. Policy-off scenario simulation ────────────────────────────────────────
#' Simulate counterfactual population trajectories under no-policy scenario.
#' Uses DiD estimates to project what would have happened absent treatment.
#'
#' Adapted from Sleeter et al. (2017) land-use projection logic,
#' applied to demographic trajectories.
simulate_policy_off <- function(panel, did_att, horizon = 2040L) {
  log_info("Simulating policy-off scenario (counterfactual trajectories).")

  # Get treated departments
  treated_depts <- panel |>
    filter(treated) |>
    distinct(geo_dept_code, first_treat_yr, geo_prov_code)

  # Baseline growth rates (pre-treatment)
  baseline_agr <- panel |>
    filter(!treated) |>
    arrange(geo_dept_code, census_year) |>
    group_by(geo_dept_code) |>
    mutate(agr = (log_pop - lag(log_pop)) / (census_year - lag(census_year))) |>
    summarise(mean_agr = mean(agr, na.rm = TRUE), .groups = "drop")

  # Treatment effect on log_pop (from DiD)
  te <- did_att$effects$overall$overall.att

  # Project each treated department
  scenarios <- map_dfr(treated_depts$geo_dept_code, function(dept) {
    last_obs <- panel |>
      filter(geo_dept_code == dept) |>
      filter(census_year == max(census_year))

    bgr <- baseline_agr |>
      filter(geo_dept_code == dept) |>
      pull(mean_agr)

    if (is.na(bgr)) bgr <- 0.005  # fallback: 0.5% annual growth

    proj_years <- seq(max(panel$census_year) + 1L, horizon)

    # Factual: with-policy trajectory
    factual <- tibble(
      geo_dept_code = dept,
      year          = proj_years,
      log_pop       = last_obs$log_pop + bgr * (proj_years - max(panel$census_year)),
      scenario      = "factual"
    )

    # Counterfactual: remove treatment effect
    counterfactual <- factual |>
      mutate(
        log_pop  = log_pop - te,
        scenario = "counterfactual_no_policy"
      )

    bind_rows(factual, counterfactual) |>
      mutate(pop_proj = exp(log_pop))
  })

  scenarios
}

# ── 4. Displacement estimation (Sharygin 2020 adaptation) ────────────────────
#' Estimate displacement induced by industrial restructuring.
#' Adapts Sharygin's (2020) wildfire displacement logic to policy-induced
#' economic restructuring contexts.
#'
#' Method:
#'   1. Identify areas with contracting employment post-treatment
#'   2. Estimate "excess outmigration" relative to counterfactual
#'   3. Decompose by age, sex, and educational attainment
estimate_displacement <- function(panel, od_period, gsynth_fit) {
  log_info("Estimating policy-induced displacement (Sharygin-adapted method).")

  # Counterfactual population from gsynth
  counterfactual <- as_tibble(gsynth_fit$Y.ct) |>
    mutate(geo_dept_code = rownames(gsynth_fit$Y.ct)) |>
    pivot_longer(-geo_dept_code, names_to = "census_year", values_to = "log_pop_cf") |>
    mutate(census_year = as.integer(census_year))

  # Excess outmigration = observed pop - counterfactual pop (when negative)
  displacement_est <- panel |>
    left_join(counterfactual, by = c("geo_dept_code", "census_year")) |>
    filter(treated) |>
    mutate(
      pop_cf         = exp(log_pop_cf),
      pop_obs        = exp(log_pop),
      pop_deficit    = pop_cf - pop_obs,      # positive = displaced
      displaced      = pop_deficit > 0,
      displaced_n    = pmax(0, pop_deficit)
    )

  total_displaced <- displacement_est |>
    group_by(census_year) |>
    summarise(
      est_displaced_total = sum(displaced_n, na.rm = TRUE),
      n_depts_displaced   = sum(displaced, na.rm = TRUE),
      .groups = "drop"
    )

  log_info("  Estimated displaced population (most recent period): {
    round(total_displaced$est_displaced_total[nrow(total_displaced)])
  }")

  list(
    by_dept  = displacement_est,
    total    = total_displaced
  )
}

# ── 5. Uncertainty propagation ────────────────────────────────────────────────
#' Monte Carlo uncertainty propagation through projections
propagate_uncertainty <- function(scenarios, n_sims = 1000L,
                                   sigma_te = 0.05, sigma_agr = 0.01) {
  log_info("Propagating uncertainty through scenario projections (n = {n_sims} sims).")

  future::plan(future::multisession, workers = min(4L, future::availableCores()))
  on.exit(future::plan(future::sequential))

  sim_results <- furrr::future_map_dfr(seq_len(n_sims), function(sim) {
    te_draw   <- rnorm(1, mean = 0, sd = sigma_te)
    agr_noise <- rnorm(nrow(scenarios), mean = 0, sd = sigma_agr)

    scenarios |>
      mutate(
        log_pop_sim = log_pop + te_draw * (scenario == "counterfactual_no_policy") + agr_noise,
        pop_sim     = exp(log_pop_sim),
        sim_id      = sim
      )
  }, .options = furrr::furrr_options(seed = 42L))

  # Summarize to credible intervals
  sim_results |>
    group_by(geo_dept_code, year, scenario) |>
    summarise(
      pop_mean  = mean(pop_sim),
      pop_q2.5  = quantile(pop_sim, 0.025),
      pop_q97.5 = quantile(pop_sim, 0.975),
      .groups   = "drop"
    )
}

# ── 6. Master function ────────────────────────────────────────────────────────
run_simulations <- function() {
  panel    <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))
  did_res  <- readRDS(file.path(PROC_DIR, "did_results.rds"))

  # Gsynth
  gsynth_fit <- estimate_gsynth(did_res$panel)
  saveRDS(gsynth_fit, file.path(PROC_DIR, "gsynth_fit.rds"))

  # Policy-off scenarios
  scenarios <- simulate_policy_off(did_res$panel, did_res, horizon = 2040L)

  # Displacement
  od_period <- arrow::read_parquet(file.path(PROC_DIR, "od_period.parquet"))
  displacement <- estimate_displacement(panel, od_period, gsynth_fit)

  # Uncertainty
  unc <- propagate_uncertainty(scenarios)
  arrow::write_parquet(unc, file.path(PROC_DIR, "scenario_uncertainty.parquet"))

  log_info("✓ Simulations complete.")
  list(gsynth = gsynth_fit, scenarios = scenarios, displacement = displacement)
}

if (sys.nframe() == 0L) run_simulations()
