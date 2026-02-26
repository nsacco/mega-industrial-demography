# =============================================================================
# 04_models/migration_models.R
# Migration analysis: gravity models, OD flow estimation, mediation analysis
# =============================================================================

library(tidyverse)
library(arrow)
library(fixest)
library(here)
library(logger)
library(glue)

source(here("R/functions/utils.R"))

PROC_DIR <- here("data/processed")

# ── 1. Build origin-destination migration matrix ──────────────────────────────
#' Construct origin × destination flow matrix from census microdata
#'
#' Uses birth province vs. current province (lifetime migration)
#' or 5-year residence question (period migration)
#' @param micro Tibble. Harmonized census microdata.
#' @param type "lifetime" or "period"
#' @return Tibble: origin, destination, census_year, flow counts
build_od_matrix <- function(micro, type = "period") {
  log_info("Building OD matrix ({type} migration).")

  if (type == "lifetime") {
    migrants <- micro |>
      filter(!is.na(birth_prov), geo_prov_code != birth_prov) |>
      rename(origin = birth_prov, destination = geo_prov_code)
  } else {
    # 5-year period migration
    migrants <- micro |>
      filter(!is.na(res5yr_prov), geo_prov_code != res5yr_prov) |>
      rename(origin = res5yr_prov, destination = geo_prov_code)
  }

  od_matrix <- migrants |>
    group_by(origin, destination, census_year, sex_label, educ_level) |>
    summarise(
      flow          = sum(hh_weight, na.rm = TRUE),
      n_records     = n(),
      .groups = "drop"
    )

  # Add diagonal (stayers)
  stayers <- micro |>
    mutate(variable = if (type == "lifetime") birth_prov else res5yr_prov) |>
    filter(!is.na(variable), geo_prov_code == variable) |>
    group_by(geo_prov_code, census_year) |>
    summarise(stayers = sum(hh_weight, na.rm = TRUE), .groups = "drop")

  list(od = od_matrix, stayers = stayers)
}

# ── 2. Gravity model ─────────────────────────────────────────────────────────
#' PPML gravity model for bilateral migration flows
#'
#' Standard specification:
#'   flow_ij = exp(α + β₁ log(pop_i) + β₂ log(pop_j) + β₃ log(dist_ij)
#'                 + β₄ treated_j + μ_i + μ_j + μ_t) * ε_ij
#'
#' Uses PPML (Santos-Silva & Tenreyro 2006) to handle zeros and overdispersion.
estimate_gravity <- function(od, population, distances, treatment) {
  log_info("Estimating PPML gravity model.")

  # Merge all covariates
  gravity_dat <- od |>
    left_join(population |> rename(origin = geo_prov_code, pop_o = pop),
              by = c("origin", "census_year")) |>
    left_join(population |> rename(destination = geo_prov_code, pop_d = pop),
              by = c("destination", "census_year")) |>
    left_join(distances, by = c("origin", "destination")) |>
    left_join(treatment |> rename(destination = geo_prov_code),
              by = c("destination", "census_year")) |>
    filter(!is.na(flow), !is.na(dist_km), !is.na(pop_o), !is.na(pop_d)) |>
    mutate(
      log_pop_o   = log(pop_o + 1),
      log_pop_d   = log(pop_d + 1),
      log_dist    = log(dist_km + 1),
      treated_d   = as.integer(treated)
    )

  # PPML via fixest (Poisson)
  ppml <- fixest::fepois(
    flow ~ log_pop_o + log_pop_d + log_dist + treated_d |
      origin + destination + census_year,
    data    = gravity_dat,
    cluster = ~ origin + destination
  )

  log_info("  Gravity model: N = {nobs(ppml)}, pseudo-R² ≈ {round(1 - ppml$deviance/ppml$null.deviance, 3)}")
  ppml
}

# ── 3. Migration flow decomposition ──────────────────────────────────────────
#' Decompose aggregate migration change into:
#'   (a) Composition effect (who moves)
#'   (b) Rate effect (how many move)
#'   (c) Structural effect (network structure)
decompose_migration_change <- function(od_t1, od_t2) {
  log_info("Decomposing migration change between periods.")

  # Total in-flow to each destination
  inflows <- bind_rows(
    od_t1 |> mutate(period = "t1"),
    od_t2 |> mutate(period = "t2")
  ) |>
    group_by(destination, period) |>
    summarise(total_inflow = sum(flow, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = total_inflow,
                values_fill = 0) |>
    mutate(
      change     = t2 - t1,
      pct_change = (t2 - t1) / t1 * 100
    )

  inflows
}

# ── 4. Mediation analysis: migration as mechanism ─────────────────────────────
#' Estimate the share of industrial policy effect on population mediated
#' by migration. Uses the product-of-coefficients method (Baron & Kenny).
#'
#' Direct path:  Treatment → log_pop
#' Indirect path: Treatment → net_migration → log_pop
#'
#' For proper causal mediation with sensitivity analysis, see:
#'   mediation::mediate()
run_mediation <- function(panel_did_results, od_summary) {
  if (!requireNamespace("mediation", quietly = TRUE)) {
    install.packages("mediation")
  }
  library(mediation)

  log_info("Running mediation analysis (treatment → migration → population).")

  dat <- panel_did_results$panel |>
    left_join(
      od_summary |> rename(geo_dept_code = destination, net_migration = net_inflow),
      by = c("geo_dept_code", "census_year")
    )

  # Mediator model: treatment → net migration
  mediator_model <- lm(
    net_migration ~ treated_num + log_pop_lag,
    data = dat
  )

  # Outcome model: treatment + net migration → log population
  outcome_model <- lm(
    log_pop ~ treated_num + net_migration + log_pop_lag,
    data = dat
  )

  # Causal mediation
  med <- mediation::mediate(
    model.m   = mediator_model,
    model.y   = outcome_model,
    treat     = "treated_num",
    mediator  = "net_migration",
    boot      = TRUE,
    sims      = 1000L,
    boot.ci.type = "perc"
  )

  log_info("  ACME = {round(med$d.avg, 4)}, ADE = {round(med$z.avg, 4)}, Prop. mediated = {round(med$n.avg, 3)}")
  med
}

# ── 5. Migrant skill decomposition ───────────────────────────────────────────
#' Compare educational composition of migrants vs. non-migrants
#' Test for skill-selective migration induced by industrial policy
skill_decomposition <- function(micro, treatment) {
  micro |>
    mutate(
      migrant_5yr = !is.na(res5yr_prov) & geo_prov_code != res5yr_prov,
      educ_high   = educ_level == "tertiary_plus"
    ) |>
    left_join(
      treatment |> rename(geo_dept_code = geo_dept_code),
      by = c("geo_dept_code", "census_year")
    ) |>
    group_by(census_year, treated, migrant_5yr) |>
    summarise(
      n             = sum(hh_weight, na.rm = TRUE),
      pct_educ_high = weighted.mean(educ_high, w = hh_weight, na.rm = TRUE),
      .groups = "drop"
    )
}

# ── 6. Master function ────────────────────────────────────────────────────────
run_migration_models <- function() {
  micro   <- arrow::read_parquet(file.path(PROC_DIR, "census_microdata_harmonized.parquet"))
  spatial <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))

  # OD matrices
  od_life   <- build_od_matrix(micro, type = "lifetime")
  od_period <- build_od_matrix(micro, type = "period")

  # Skill decomposition
  skill_decomp <- skill_decomposition(micro, spatial)
  readr::write_csv(skill_decomp, here("outputs/tables/tbl_skill_decomposition.csv"))

  # Save OD matrices
  arrow::write_parquet(od_period$od, file.path(PROC_DIR, "od_period.parquet"))
  arrow::write_parquet(od_life$od,   file.path(PROC_DIR, "od_lifetime.parquet"))

  log_info("✓ Migration models complete.")
  list(od_period = od_period, od_life = od_life, skill_decomp = skill_decomp)
}

if (sys.nframe() == 0L) run_migration_models()
