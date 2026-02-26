# =============================================================================
# _targets.R
# Pipeline orchestration using {targets}
# Run with: targets::tar_make()
# Inspect: targets::tar_visnetwork()
# =============================================================================

library(targets)
library(tarchetypes)
library(here)

# ── Options ───────────────────────────────────────────────────────────────────
tar_option_set(
  packages   = c("tidyverse", "arrow", "sf", "terra", "spdep", "spatialreg",
                 "did", "fixest", "brms", "migest", "gsynth", "here", "logger"),
  format     = "parquet",   # default format for data targets
  memory     = "transient", # free memory between targets
  garbage_collection = TRUE,
  seed       = 42L,
  controller = crew::crew_controller_local(workers = 4L)
)

# ── Source all R modules ──────────────────────────────────────────────────────
tar_source("R/functions/utils.R")
tar_source("R/01_harmonization.R")
tar_source("R/02_spatial_linkage.R")
tar_source("R/03_descriptive.R")
tar_source("R/04_models/did_models.R")
tar_source("R/04_models/spatial_models.R")
tar_source("R/04_models/bayesian_models.R")
tar_source("R/04_models/migration_models.R")
tar_source("R/05_simulations.R")
tar_source("R/06_visualization.R")

# =============================================================================
# PIPELINE DEFINITION
# =============================================================================
list(

  # ── 0. Raw data tracking ───────────────────────────────────────────────────
  # Track raw file timestamps so targets re-run when data changes
  tar_target(raw_census_files,
    command = list.files(here("data/raw"), pattern = "\\.(sav|dta|csv)$",
                         recursive = TRUE, full.names = TRUE),
    format = "file"
  ),
  tar_target(raw_zone_files,
    command = here("data/raw/industrial_zones/industrial_zones.gpkg"),
    format  = "file"
  ),

  # ── 1. Census harmonization ────────────────────────────────────────────────
  tar_target(census_harmonized,
    command = harmonize_census(rounds = c(1970, 1991, 2001, 2010, 2022)),
    format  = "rds"
  ),

  tar_target(census_microdata,
    command = census_harmonized$microdata
  ),

  tar_target(census_dept_counts,
    command = census_harmonized$dept_counts
  ),

  # ── 2. Spatial linkage ─────────────────────────────────────────────────────
  tar_target(boundaries,
    command = load_boundaries(),
    format  = "rds"
  ),

  tar_target(industrial_zones,
    command = load_industrial_zones(),
    format  = "rds"
  ),

  tar_target(treatment_panel,
    command = assign_treatment(
      boundaries = boundaries,
      zones      = industrial_zones,
      years      = c(1970, 1991, 2001, 2010, 2022)
    )
  ),

  tar_target(forest_cover,
    command = extract_forest_cover(boundaries)
  ),

  tar_target(spatial_panel,
    command = {
      treatment_panel |>
        left_join(forest_cover, by = c("geo_dept_code", "census_year")) |>
        left_join(
          boundaries |> sf::st_drop_geometry() |>
            select(geo_dept_code, geo_prov_code),
          by = "geo_dept_code"
        )
    }
  ),

  tar_target(spatial_weights,
    command = build_spatial_weights(boundaries),
    format  = "rds"
  ),

  # ── 3. Descriptive analysis ────────────────────────────────────────────────
  tar_target(growth_rates,
    command = compute_growth_rates(census_dept_counts)
  ),

  tar_target(summary_table,
    command = make_summary_table(growth_rates, spatial_panel)
  ),

  tarchetypes::tar_map(
    values = tibble::tibble(yr = c(1970L, 1991L, 2001L, 2010L, 2022L)),
    tar_target(fig_pyramid,
      command = {
        p <- plot_pyramid(census_microdata, year = yr)
        ggsave_industrial(p, here(glue::glue("outputs/figures/fig_pyramid_{yr}.png")))
        p
      },
      format = "rds"
    )
  ),

  # ── 4. Models ─────────────────────────────────────────────────────────────

  # 4a. DiD (Callaway-Sant'Anna)
  tar_target(did_panel,
    command = prepare_did_panel()
  ),

  tar_target(did_att_gt,
    command = estimate_att_gt(did_panel),
    format  = "rds"
  ),

  tar_target(did_effects,
    command = aggregate_effects(did_att_gt),
    format  = "rds"
  ),

  tar_target(did_twfe,
    command = twfe_benchmark(did_panel),
    format  = "rds"
  ),

  tar_target(fig_event_study,
    command = {
      p <- plot_event_study_pub(broom::tidy(did_effects$dynamic, conf.int = TRUE))
      ggsave_industrial(p, here("outputs/figures/fig_event_study.png"))
      p
    },
    format = "rds"
  ),

  # 4b. Spatial models
  tar_target(spatial_morans,
    command = {
      full_dat <- census_dept_counts |>
        group_by(geo_dept_code, census_year) |>
        summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop") |>
        left_join(spatial_panel, by = c("geo_dept_code", "census_year")) |>
        mutate(log_pop = log(pop + 1), treated_num = as.integer(treated))
      run_morans_tests(full_dat, spatial_weights)
    }
  ),

  tar_target(sdm_2022,
    command = {
      dat_yr <- census_dept_counts |>
        group_by(geo_dept_code, census_year) |>
        summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop") |>
        left_join(spatial_panel, by = c("geo_dept_code", "census_year")) |>
        mutate(log_pop = log(pop + 1), treated_num = as.integer(treated)) |>
        filter(census_year == 2022)
      estimate_sdm(dat_yr, spatial_weights, yr = 2022)
    },
    format = "rds"
  ),

  tar_target(spillover_decomposition,
    command = decompose_spillovers(sdm_2022, spatial_weights),
    format  = "rds"
  ),

  # 4c. Bayesian model (slow; cached to disk)
  tar_target(bayesian_fit,
    command = {
      dat <- census_dept_counts |>
        group_by(geo_dept_code, census_year) |>
        summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop") |>
        left_join(spatial_panel, by = c("geo_dept_code", "census_year")) |>
        mutate(log_pop = log(pop + 1))
      fit_bayesian_model(dat)
    },
    format = "rds",
    cue    = tar_cue(mode = "never")   # only re-run if cue is changed
  ),

  tar_target(bayesian_sae,
    command = extract_small_area_estimates(bayesian_fit)
  ),

  # 4d. Migration models
  tar_target(od_period,
    command = build_od_matrix(census_microdata, type = "period")$od
  ),

  tar_target(od_lifetime,
    command = build_od_matrix(census_microdata, type = "lifetime")$od
  ),

  tar_target(skill_decomp,
    command = skill_decomposition(census_microdata, spatial_panel)
  ),

  # ── 5. Simulations ─────────────────────────────────────────────────────────
  tar_target(gsynth_fit,
    command = estimate_gsynth(did_panel),
    format  = "rds"
  ),

  tar_target(scenarios,
    command = simulate_policy_off(did_panel, list(effects = did_effects), horizon = 2040L),
    format  = "rds"
  ),

  tar_target(displacement_est,
    command = estimate_displacement(spatial_panel, od_period, gsynth_fit),
    format  = "rds"
  ),

  tar_target(scenario_uncertainty,
    command = propagate_uncertainty(scenarios)
  ),

  tar_target(fig_scenarios,
    command = {
      p <- plot_scenarios(scenario_uncertainty)
      ggsave_industrial(p, here("outputs/figures/fig_scenarios.png"))
      p
    },
    format = "rds"
  ),

  # ── 6. Quarto renders ─────────────────────────────────────────────────────
  tarchetypes::tar_quarto(
    paper_1,
    path = here("papers/paper_1_population_growth/paper_1_population_growth.qmd")
  ),

  tarchetypes::tar_quarto(
    paper_2,
    path = here("papers/paper_2_migration/paper_2_migration.qmd")
  ),

  tarchetypes::tar_quarto(
    paper_3,
    path = here("papers/paper_3_land_use/paper_3_land_use.qmd")
  ),

  tarchetypes::tar_quarto(
    book_chapter,
    path = here("papers/book_chapter/book_chapter.qmd")
  )
)
