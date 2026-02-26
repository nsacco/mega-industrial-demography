# =============================================================================
# 01_harmonization.R
# Census harmonization pipeline: Argentina 1970–2022
# Produces: data/processed/census_harmonized.parquet
# =============================================================================

library(tidyverse)
library(haven)
library(arrow)
library(here)
library(glue)
library(logger)

source(here("R/functions/utils.R"))

# ── 1. Configuration ─────────────────────────────────────────────────────────
CENSUS_ROUNDS <- c(1970, 1991, 2001, 2010, 2022)

RAW_DIR  <- here("data/raw")
PROC_DIR <- here("data/processed")

# ── 2. Variable crosswalk (harmonized names → census-specific names) ──────────
# Each list element maps harmonized variable → list of round-specific names.
# NULL means the variable is unavailable in that round.
VARIABLE_CROSSWALK <- list(
  geo_dept_code = list(
    `1970` = "depto",  `1991` = "depto", `2001` = "dpto",
    `2010` = "dpto",   `2022` = "dpto"
  ),
  geo_prov_code = list(
    `1970` = "prov",   `1991` = "prov",  `2001` = "prov",
    `2010` = "prov",   `2022` = "prov"
  ),
  age = list(
    `1970` = "edad",   `1991` = "edad",  `2001` = "edad",
    `2010` = "edad",   `2022` = "edad"
  ),
  sex = list(
    `1970` = "sexo",   `1991` = "sexo",  `2001` = "sexo",
    `2010` = "sexo",   `2022` = "sexo"
  ),
  # Place of birth (for migration)
  birth_prov = list(
    `1970` = "lugar_nac_cod", `1991` = "lugar_nac",
    `2001` = "LUGAR_NAC",     `2010` = "LUGAR_NAC",
    `2022` = "lugar_nac"
  ),
  # Residence 5 years ago
  res5yr_prov = list(
    `1970` = NULL, `1991` = "res_hace_5",
    `2001` = "RES5",           `2010` = "RES5",
    `2022` = "res_5_anos"
  ),
  educ_max = list(
    `1970` = "instruccion",    `1991` = "instruccion",
    `2001` = "NIVEL_ED",       `2010` = "NIVEL_ED",
    `2022` = "nivel_ed"
  ),
  employed = list(
    `1970` = "condicion",      `1991` = "condicion",
    `2001` = "CONDICION",      `2010` = "condicion",
    `2022` = "condicion"
  ),
  sector = list(
    `1970` = "rama",           `1991` = "rama",
    `2001` = "RAMA_ACT",       `2010` = "pp04b_cod",
    `2022` = "rama"
  ),
  hh_weight = list(
    `1970` = "pondera",        `1991` = "pondera",
    `2001` = "PONDERA",        `2010` = "pondera",
    `2022` = "pondera"
  )
)

# ── 3. Geographic concordance ─────────────────────────────────────────────────
# Departments/municipalities change boundaries over time.
# This function loads the crosswalk table and returns a lookup tibble.
load_geo_concordance <- function() {
  path <- here("data/documentation/geo_concordance.csv")
  if (!file.exists(path)) {
    warning("Geo concordance table not found: ", path,
            "\nUsing identity mapping (no harmonization).")
    return(NULL)
  }
  vroom::vroom(path, show_col_types = FALSE)
}

# ── 4. Single-round loader ────────────────────────────────────────────────────
#' Load and recode one census round
#'
#' @param year Integer. Census year.
#' @param crosswalk Named list. Variable crosswalk (VARIABLE_CROSSWALK).
#' @param geo_concordance Tibble or NULL. Geographic harmonization table.
#' @return Tibble with harmonized variables and a `census_year` column.
load_census_round <- function(year, crosswalk = VARIABLE_CROSSWALK,
                               geo_concordance = NULL) {
  log_info("Loading census round: {year}")

  # Locate raw file (supports .sav, .dta, .csv)
  raw_files <- list.files(
    file.path(RAW_DIR, glue("census_{year}")),
    pattern = "\\.(sav|dta|csv|rds)$",
    full.names = TRUE, recursive = TRUE
  )

  if (length(raw_files) == 0) {
    log_warn("No raw files found for census {year}. Returning NULL.")
    return(NULL)
  }

  # Read first matching file
  raw <- switch(
    tools::file_ext(raw_files[1]),
    sav = haven::read_sav(raw_files[1]),
    dta = haven::read_dta(raw_files[1]),
    csv = vroom::vroom(raw_files[1], show_col_types = FALSE),
    rds = readRDS(raw_files[1])
  )

  # Recode to harmonized names
  yr_char <- as.character(year)
  harmonized <- tibble(census_year = year)

  for (harm_var in names(crosswalk)) {
    raw_var <- crosswalk[[harm_var]][[yr_char]]
    if (is.null(raw_var) || !raw_var %in% names(raw)) {
      harmonized[[harm_var]] <- NA
    } else {
      harmonized[[harm_var]] <- haven::zap_labels(raw[[raw_var]])
    }
  }

  # Apply geographic concordance
  if (!is.null(geo_concordance)) {
    harmonized <- harmonized |>
      left_join(
        geo_concordance |> filter(census_year == !!year) |>
          select(geo_dept_code, geo_dept_code_2010 = harmonized_dept),
        by = "geo_dept_code"
      )
  }

  harmonized
}

# ── 5. Recode helper functions ────────────────────────────────────────────────
recode_sex <- function(x) {
  case_when(
    x %in% c(1, "1", "Varón", "Hombre", "Male")   ~ "Male",
    x %in% c(2, "2", "Mujer", "Femenino", "Female") ~ "Female",
    TRUE ~ NA_character_
  )
}

recode_educ <- function(x, year) {
  # Simplified: collapse to 3 levels (primary, secondary, tertiary+)
  if (year <= 1991) {
    case_when(
      x <= 2 ~ "primary_or_less",
      x == 3 ~ "secondary",
      x >= 4 ~ "tertiary_plus",
      TRUE    ~ NA_character_
    )
  } else {
    case_when(
      x %in% 1:3 ~ "primary_or_less",
      x %in% 4:5 ~ "secondary",
      x %in% 6:9 ~ "tertiary_plus",
      TRUE        ~ NA_character_
    )
  }
}

recode_sector_isic2 <- function(x) {
  # Map census sector codes to ISIC Rev.2 sections (broad)
  # Adapt this to actual census codes
  case_when(
    x %in% c(1:5)   ~ "Agriculture",
    x %in% c(10:45) ~ "Industry",
    x %in% c(50:55) ~ "Commerce",
    x %in% c(60:64) ~ "Transport",
    x %in% c(65:74) ~ "Finance",
    x %in% c(75:99) ~ "Services",
    TRUE             ~ "Unknown"
  )
}

# ── 6. Main harmonization pipeline ───────────────────────────────────────────
harmonize_census <- function(rounds = CENSUS_ROUNDS) {
  log_info("Starting census harmonization pipeline.")

  geo_concordance <- load_geo_concordance()

  round_data <- map(
    rounds,
    ~ load_census_round(.x, crosswalk = VARIABLE_CROSSWALK,
                        geo_concordance = geo_concordance)
  ) |> compact()   # remove NULLs (missing rounds)

  if (length(round_data) == 0) {
    log_error("No census rounds loaded. Check raw data in {RAW_DIR}.")
    stop("No census data found.")
  }

  combined <- bind_rows(round_data) |>
    mutate(
      sex_label   = recode_sex(sex),
      educ_level  = map2_chr(educ_max, census_year, recode_educ),
      sector_isic = recode_sector_isic2(sector),
      age_group5  = cut(age,
                        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
                                   50, 55, 60, 65, 70, 75, 80, Inf),
                        right  = FALSE,
                        labels = c("0-4","5-9","10-14","15-19","20-24",
                                   "25-29","30-34","35-39","40-44","45-49",
                                   "50-54","55-59","60-64","65-69","70-74",
                                   "75-79","80+"))
    ) |>
    # Quality flags
    mutate(
      flag_missing_geo  = is.na(geo_dept_code),
      flag_missing_age  = is.na(age),
      flag_imputed_mig  = FALSE   # placeholder
    )

  log_info("Harmonized {nrow(combined)} person-records across {length(round_data)} rounds.")

  # ── 7. Small-area population counts ──────────────────────────────────────
  dept_counts <- combined |>
    group_by(census_year, geo_prov_code, geo_dept_code, sex_label, age_group5) |>
    summarise(
      pop_count   = sum(hh_weight, na.rm = TRUE),
      n_records   = n(),
      .groups     = "drop"
    )

  # ── 8. Save outputs ────────────────────────────────────────────────────────
  arrow::write_parquet(combined,     file.path(PROC_DIR, "census_microdata_harmonized.parquet"))
  arrow::write_parquet(dept_counts,  file.path(PROC_DIR, "census_dept_counts.parquet"))

  log_info("✓ Saved harmonized microdata and department counts to {PROC_DIR}.")

  list(
    microdata   = combined,
    dept_counts = dept_counts
  )
}

# ── Run if called directly ───────────────────────────────────────────────────
if (sys.nframe() == 0L) {
  result <- harmonize_census()
  message("Done. Census rounds harmonized: ", length(unique(result$microdata$census_year)))
}
