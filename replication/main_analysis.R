# =============================================================================
# replication/main_analysis.R
# Master replication script: runs the complete analysis pipeline from raw data
# to all published outputs (tables, figures, models).
#
# USAGE:
#   Rscript replication/main_analysis.R
#   # OR via targets:
#   targets::tar_make()
# =============================================================================

message("=" , strrep("=", 60))
message(" mega-industrial-demography: Replication Package")
message(" Starting full pipeline at: ", Sys.time())
message("=" , strrep("=", 60))

# ── 0. Environment ─────────────────────────────────────────────────────────────
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(prompt = FALSE)

library(here)
source(here("R/00_setup.R"))

# ── 1. Check raw data availability ───────────────────────────────────────────
required_dirs <- c(
  "data/raw/census_1970",
  "data/raw/census_1991",
  "data/raw/census_2001",
  "data/raw/census_2010",
  "data/raw/census_2022",
  "data/raw/industrial_zones",
  "data/external/boundaries",
  "data/external/Hansen_GFW"
)

missing <- required_dirs[!dir.exists(here(required_dirs))]
if (length(missing) > 0) {
  warning(
    "The following raw data directories are missing:\n",
    paste0("  ", missing, collapse = "\n"), "\n",
    "See data/documentation/data_dictionary.md for download instructions.\n",
    "Proceeding with available data..."
  )
}

# ── 2. Run pipeline via targets ───────────────────────────────────────────────
if (!requireNamespace("targets", quietly = TRUE)) install.packages("targets")

message("\n>> Running targets pipeline...")
targets::tar_make(
  callr_function    = callr::r,
  reporter          = "verbose",
  garbage_collection = TRUE
)

# ── 3. Verify outputs ──────────────────────────────────────────────────────────
message("\n>> Verifying output files...")

expected_outputs <- list(
  figures = c(
    "fig_event_study.png",
    "fig_scenarios.png",
    "fig_pyramid_2022.png"
  ),
  tables  = c(
    "tbl_summary_stats.csv",
    "tbl_skill_decomposition.csv"
  )
)

for (dir_name in names(expected_outputs)) {
  for (f in expected_outputs[[dir_name]]) {
    path <- here("outputs", dir_name, f)
    if (file.exists(path)) {
      message("  ✓ ", f)
    } else {
      message("  ✗ MISSING: ", f)
    }
  }
}

# ── 4. Session and environment snapshot ──────────────────────────────────────
source(here("R/functions/utils.R"))
session_snapshot(here("outputs/session_info.txt"))

message("\n✓ Replication complete at: ", Sys.time())
message("  All outputs saved to: ", here("outputs"))
