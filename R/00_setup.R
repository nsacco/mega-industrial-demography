# =============================================================================
# 00_setup.R
# Environment setup: package installation, renv, global options, paths
# =============================================================================

# ── 1. renv activation ─────────────────────────────────────────────────────
# renv::restore() is called automatically when renv is activated via the
# project's .Rprofile. Run renv::restore() manually if first-time setup.

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# ── 2. CRAN packages ────────────────────────────────────────────────────────
cran_pkgs <- c(
  # Core tidyverse
  "tidyverse", "lubridate", "glue", "janitor", "here",

  # Spatial
  "sf", "terra", "stars", "tmap", "leaflet",
  "spdep", "spatialreg",

  # Causal inference & econometrics
  "did",           # Callaway-Sant'Anna DiD
  "fixest",        # Fast fixed effects
  "did2s",         # Two-stage DiD
  "synthdid",      # Synthetic DiD
  "MatchIt",       # Matching
  "WeightIt",      # Weighting
  "cobalt",        # Balance tables

  # Synthetic control
  "Synth",
  "gsynth",
  "scpi",

  # Bayesian
  "brms",
  "rstanarm",
  "posterior",
  "bayesplot",
  # INLA: installed separately (see below)

  # Demographic & migration
  "migest",
  "demography",
  "DemoTools",
  "MortalityLaws",

  # Land-use change
  "lulcc",

  # Visualization
  "ggplot2",
  "ggthemes",
  "scales",
  "patchwork",
  "viridis",
  "RColorBrewer",
  "gt",           # Tables
  "gtsummary",    # Summary tables
  "knitr",
  "kableExtra",

  # Pipeline
  "targets",
  "tarchetypes",

  # Utilities
  "arrow",        # Parquet I/O
  "vroom",        # Fast CSV
  "haven",        # Stata/SPSS files
  "readxl",
  "openxlsx",
  "furrr",        # Parallel purrr
  "progressr",
  "logger"
)

# Install missing packages
missing <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  message("Installing missing CRAN packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# ── 3. GitHub packages ───────────────────────────────────────────────────────
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

github_pkgs <- list(
  list(pkg = "rdinter/ustracker",        check = "ustracker"),    # example
  list(pkg = "wpgp/wopr",               check = "wopr")           # population data
)

for (gp in github_pkgs) {
  if (!requireNamespace(gp$check, quietly = TRUE)) {
    remotes::install_github(gp$pkg)
  }
}

# ── 4. INLA (special repository) ─────────────────────────────────────────────
if (!requireNamespace("INLA", quietly = TRUE)) {
  message("Installing INLA from r-inla.org...")
  install.packages(
    "INLA",
    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
    dep = TRUE
  )
}

# ── 5. Global options ────────────────────────────────────────────────────────
options(
  scipen      = 999,
  digits      = 4,
  warn        = 1,          # show warnings immediately
  timeout     = 300,        # network timeout
  mc.cores    = parallel::detectCores() - 1L
)

# ── 6. Directory scaffolding ─────────────────────────────────────────────────
dirs <- c(
  "data/raw", "data/processed", "data/external", "data/documentation",
  "outputs/figures", "outputs/tables", "outputs/maps", "outputs/presentations",
  "R/04_models", "R/functions"
)
for (d in dirs) {
  if (!dir.exists(here::here(d))) {
    dir.create(here::here(d), recursive = TRUE)
    message("Created directory: ", d)
  }
}

message("\n✓ Environment setup complete.")
message("  R version: ", paste(R.version$major, R.version$minor, sep = "."))
message("  Working directory: ", getwd())
