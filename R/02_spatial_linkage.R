# =============================================================================
# 02_spatial_linkage.R
# Geospatial processing: link census units to industrial zones and land cover
# Produces: data/processed/spatial_panel.parquet
# =============================================================================

library(tidyverse)
library(sf)
library(terra)
library(stars)
library(spdep)
library(arrow)
library(here)
library(logger)
library(glue)

source(here("R/functions/utils.R"))

RAW_DIR  <- here("data/raw")
PROC_DIR <- here("data/processed")
EXT_DIR  <- here("data/external")

# ── 1. Load department boundaries ─────────────────────────────────────────────
#' Load Argentina department polygons
#'
#' Expects a GeoPackage/Shapefile in data/external/boundaries/
#' @return sf object with department polygons
load_boundaries <- function() {
  path <- file.path(EXT_DIR, "boundaries", "argentina_departamentos.gpkg")
  if (!file.exists(path)) {
    stop(
      "Department boundary file not found: ", path, "\n",
      "Download from: https://www.ign.gob.ar/descarga-de-capas-de-informacion-geografica"
    )
  }
  log_info("Loading department boundaries from {path}")
  sf::st_read(path, quiet = TRUE) |>
    sf::st_transform(crs = 22185)  # Argentina Faja 5 (POSGAR 2007)
}

# ── 2. Load industrial promotion zones ────────────────────────────────────────
#' Load industrial promotion zone polygons
#'
#' @return sf object with zone polygons including policy metadata
load_industrial_zones <- function() {
  path <- file.path(RAW_DIR, "industrial_zones", "industrial_zones.gpkg")
  if (!file.exists(path)) {
    stop(
      "Industrial zones file not found: ", path, "\n",
      "This dataset must be assembled from:\n",
      "  - Ley 22.021 (Noroeste) and derivatives\n",
      "  - Ley 19.640 (Tierra del Fuego)\n",
      "  - Provincial promotion laws\n",
      "  - MEOEI registers"
    )
  }
  log_info("Loading industrial zones from {path}")
  sf::st_read(path, quiet = TRUE) |>
    sf::st_transform(crs = 22185) |>
    # Expected columns: zone_id, zone_name, law_number, start_year,
    #                   end_year (NA if still active), sector_code,
    #                   incentive_type (tax/infra/regulatory)
    mutate(
      active_years = map2(start_year, end_year,
                          ~ seq(.x, if (is.na(.y)) 2022 else .y)),
      ever_treated = TRUE
    )
}

# ── 3. Point-in-polygon: assign zones to departments ─────────────────────────
#' Classify each department by its treatment status in each year
#'
#' @param boundaries sf. Department polygons.
#' @param zones sf. Industrial zone polygons.
#' @param years Integer vector. Years to evaluate.
#' @return Tibble: dept × year panel with treatment indicator and zone covariates
assign_treatment <- function(boundaries, zones, years = c(1970, 1991, 2001, 2010, 2022)) {

  log_info("Assigning industrial zone treatment to departments.")

  # Spatial intersection
  intersection <- sf::st_intersection(
    boundaries |> select(geo_dept_code, geo_prov_code, area_km2 = Shape_Area),
    zones       |> select(zone_id, zone_name, law_number, start_year, end_year,
                          incentive_type, sector_code)
  ) |>
    sf::st_drop_geometry() |>
    # Overlap area
    mutate(overlap_area = as.numeric(sf::st_area(
      sf::st_intersection(boundaries, zones))))

  # Build year-specific treatment panel
  panel <- crossing(geo_dept_code = boundaries$geo_dept_code, census_year = years) |>
    left_join(
      intersection |>
        rowwise() |>
        mutate(active_in_year = map(list(start_year:if_else(is.na(end_year), 2022L, as.integer(end_year))),
                                     ~ census_year %in% .x)) |>
        unnest(active_in_year) |>
        filter(active_in_year) |>
        group_by(geo_dept_code, census_year) |>
        summarise(
          treated         = TRUE,
          n_zones         = n_distinct(zone_id),
          first_treat_yr  = min(start_year),
          incentive_types = paste(unique(incentive_type), collapse = "|"),
          .groups = "drop"
        ),
      by = c("geo_dept_code", "census_year")
    ) |>
    replace_na(list(treated = FALSE, n_zones = 0L))

  panel
}

# ── 4. Buffer analysis ────────────────────────────────────────────────────────
#' Create buffer zones around industrial areas for spillover analysis
#'
#' @param zones sf. Industrial zone polygons.
#' @param distances_km Numeric vector. Buffer distances in km.
#' @return List of sf buffers (one per distance)
create_buffers <- function(zones, distances_km = c(25, 50, 100)) {
  log_info("Creating {length(distances_km)} buffer zones.")
  map(
    set_names(distances_km, glue("buffer_{distances_km}km")),
    ~ sf::st_buffer(zones, dist = .x * 1000)  # convert km → m
  )
}

# ── 5. Land-cover raster linkage ──────────────────────────────────────────────
#' Extract zonal statistics from Hansen Global Forest Cover rasters
#'
#' @param boundaries sf. Department polygons.
#' @param years Integer vector. Available raster years.
#' @return Tibble: dept × year with forest cover % and loss area
extract_forest_cover <- function(boundaries, years = c(2000, 2005, 2010, 2015, 2020, 2022)) {

  raster_dir <- file.path(EXT_DIR, "Hansen_GFW")
  if (!dir.exists(raster_dir)) {
    log_warn("Hansen GFW rasters not found in {raster_dir}. Returning empty tibble.")
    return(tibble())
  }

  results <- map_dfr(years, function(yr) {
    treecover_path <- file.path(raster_dir, glue("treecover_{yr}.tif"))
    loss_path      <- file.path(raster_dir, glue("loss_{yr}.tif"))

    if (!file.exists(treecover_path)) {
      log_warn("Raster not found for year {yr}: {treecover_path}")
      return(NULL)
    }

    r <- terra::rast(treecover_path)

    # Reproject boundaries to raster CRS
    bounds_proj <- boundaries |> sf::st_transform(terra::crs(r, describe = TRUE)$code)

    # Extract mean tree cover per department
    zonal_stats <- terra::extract(
      r, terra::vect(bounds_proj),
      fun = "mean", na.rm = TRUE, ID = TRUE
    ) |>
      as_tibble() |>
      rename(treecover_pct = 2) |>
      mutate(
        geo_dept_code = boundaries$geo_dept_code[ID],
        census_year   = yr
      ) |>
      select(geo_dept_code, census_year, treecover_pct)

    zonal_stats
  })

  results
}

# ── 6. Spatial weights matrix ─────────────────────────────────────────────────
#' Build queen-contiguity spatial weights matrix
#'
#' @param boundaries sf. Department polygons.
#' @param style Character. Standardization style ("W" = row-standardized).
#' @return listw object (spdep)
build_spatial_weights <- function(boundaries, style = "W") {
  log_info("Building spatial weights matrix (queen contiguity, style = {style}).")
  nb  <- spdep::poly2nb(boundaries, queen = TRUE)
  lw  <- spdep::nb2listw(nb, style = style, zero.policy = TRUE)
  log_info("  Neighbors: mean = {round(mean(card(nb)), 2)}, islands = {sum(card(nb) == 0)}")
  lw
}

# ── 7. Assemble spatial panel ─────────────────────────────────────────────────
build_spatial_panel <- function() {
  log_info("Assembling spatial analysis panel.")

  # Load layers
  boundaries <- load_boundaries()
  zones      <- load_industrial_zones()

  # Treatment assignment
  treatment_panel <- assign_treatment(
    boundaries = boundaries,
    zones      = zones,
    years      = c(1970, 1991, 2001, 2010, 2022)
  )

  # Forest cover
  forest_panel <- extract_forest_cover(boundaries)

  # Join
  spatial_panel <- treatment_panel |>
    left_join(forest_panel, by = c("geo_dept_code", "census_year")) |>
    left_join(
      boundaries |> sf::st_drop_geometry() |>
        select(geo_dept_code, geo_prov_code),
      by = "geo_dept_code"
    )

  # Save
  arrow::write_parquet(spatial_panel, file.path(PROC_DIR, "spatial_panel.parquet"))
  log_info("✓ Spatial panel saved: {nrow(spatial_panel)} dept-year observations.")

  # Save spatial weights
  lw <- build_spatial_weights(boundaries)
  saveRDS(lw, file.path(PROC_DIR, "spatial_weights.rds"))

  list(panel = spatial_panel, weights = lw, boundaries = boundaries)
}

# ── Run if called directly ────────────────────────────────────────────────────
if (sys.nframe() == 0L) {
  result <- build_spatial_panel()
}
