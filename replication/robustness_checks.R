# =============================================================================
# replication/robustness_checks.R
# Alternative specifications, placebo tests, control group sensitivity
# =============================================================================

library(tidyverse)
library(did)
library(fixest)
library(here)

source(here("R/functions/utils.R"))
source(here("R/04_models/did_models.R"))

PROC_DIR <- here("data/processed")

# ── 1. Load base panel ─────────────────────────────────────────────────────────
panel <- prepare_did_panel()

# ── 2. Control group sensitivity ──────────────────────────────────────────────
message(">> Robustness: Control group comparison")

did_nevertreated <- estimate_att_gt(panel, control_group = "nevertreated")
did_notyettreated <- estimate_att_gt(panel, control_group = "notyettreated")

effects_never   <- aggregate_effects(did_nevertreated)
effects_notyet  <- aggregate_effects(did_notyettreated)

comparison <- tibble(
  control_group = c("never_treated", "not_yet_treated"),
  overall_att   = c(effects_never$overall$overall.att,
                    effects_notyet$overall$overall.att),
  overall_se    = c(effects_never$overall$overall.se,
                    effects_notyet$overall$overall.se)
)

message("Control group ATT comparison:")
print(comparison)
readr::write_csv(comparison, here("outputs/tables/tbl_robustness_control_group.csv"))

# ── 3. Alternative outcomes ────────────────────────────────────────────────────
message(">> Robustness: Alternative outcomes")

outcomes <- c("log_pop", "agr")

did_by_outcome <- map(
  set_names(outcomes),
  ~ estimate_att_gt(panel, outcome = .x)
)

outcome_comparison <- map_dfr(names(did_by_outcome), function(out) {
  agg <- aggregate_effects(did_by_outcome[[out]])
  tibble(
    outcome     = out,
    overall_att = agg$overall$overall.att,
    overall_se  = agg$overall$overall.se
  )
})

message("Outcome robustness:")
print(outcome_comparison)
readr::write_csv(outcome_comparison, here("outputs/tables/tbl_robustness_outcomes.csv"))

# ── 4. Anticipation effects ────────────────────────────────────────────────────
message(">> Robustness: Anticipation effects (1 period)")

did_anticipation1 <- estimate_att_gt(panel, anticipation = 1L)
eff_ant1 <- aggregate_effects(did_anticipation1)

anticipation_check <- tibble(
  anticipation = c(0L, 1L),
  overall_att  = c(effects_never$overall$overall.att,
                   eff_ant1$overall$overall.att),
  overall_se   = c(effects_never$overall$overall.se,
                   eff_ant1$overall$overall.se)
)
readr::write_csv(anticipation_check, here("outputs/tables/tbl_robustness_anticipation.csv"))

# ── 5. Spatial bandwidth restriction ──────────────────────────────────────────
message(">> Robustness: Geographic bandwidth (< 200 km from treatment)")
# Filter to departments within a bandwidth of treated zones
# This requires spatial_panel to be loaded and merged
spatial <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))

# Departments within 200km of any treated zone (requires pre-computed distance)
if ("dist_nearest_zone" %in% names(spatial)) {
  panel_bw <- panel |>
    left_join(
      spatial |> select(geo_dept_code, census_year, dist_nearest_zone),
      by = c("geo_dept_code", "census_year")
    ) |>
    filter(is.na(dist_nearest_zone) | dist_nearest_zone < 200)

  did_bw <- estimate_att_gt(panel_bw)
  eff_bw <- aggregate_effects(did_bw)
  message("Bandwidth (200km) ATT: ", round(eff_bw$overall$overall.att, 4))
} else {
  message("Distance to nearest zone not available. Skipping bandwidth check.")
}

# ── 6. Estimation method sensitivity (IPW vs regression vs doubly robust) ─────
message(">> Robustness: Estimation method")

est_methods <- c("reg", "ipw", "dr")

did_by_method <- map(
  set_names(est_methods),
  function(method) {
    did::att_gt(
      yname         = "log_pop",
      tname         = "census_year",
      idname        = "dept_id",
      gname         = "gvar",
      data          = panel |> filter(!is.na(log_pop_lag)),
      control_group = "nevertreated",
      est_method    = method,
      panel         = TRUE
    )
  }
)

method_comparison <- map_dfr(names(did_by_method), function(m) {
  agg <- did::aggte(did_by_method[[m]], type = "simple", na.rm = TRUE)
  tibble(
    method      = m,
    overall_att = agg$overall.att,
    overall_se  = agg$overall.se
  )
})

message("Estimation method comparison:")
print(method_comparison)
readr::write_csv(method_comparison, here("outputs/tables/tbl_robustness_methods.csv"))

message("\n✓ Robustness checks complete.")
