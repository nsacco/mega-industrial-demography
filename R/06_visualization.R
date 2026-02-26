# =============================================================================
# 06_visualization.R
# Publication-ready figures, maps, and theme system
# =============================================================================

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(viridis)
library(patchwork)
library(tmap)
library(sf)
library(here)

# ── 1. Core theme ─────────────────────────────────────────────────────────────
theme_industrial <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Typography
      plot.title       = element_text(size = base_size + 2, face = "bold", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = base_size - 1, color = "gray40", margin = margin(b = 10)),
      plot.caption     = element_text(size = base_size - 2, color = "gray55", hjust = 0),
      axis.title       = element_text(size = base_size - 1, color = "gray30"),
      axis.text        = element_text(size = base_size - 2, color = "gray40"),
      legend.title     = element_text(size = base_size - 1, face = "bold"),
      legend.text      = element_text(size = base_size - 2),

      # Grid
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),

      # Facets
      strip.text       = element_text(size = base_size - 1, face = "bold"),
      strip.background = element_rect(fill = "gray96", color = NA),

      # Margins
      plot.margin      = margin(12, 12, 8, 12)
    )
}

theme_industrial_map <- function(base_size = 10) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title    = element_text(size = base_size + 2, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size - 1, color = "gray40", margin = margin(b = 8)),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.margin   = margin(8, 8, 8, 8)
    )
}

# ── 2. Color palettes ─────────────────────────────────────────────────────────
PALETTE_TREATMENT    <- c("Control" = "#4393c3", "Treated" = "#d73027")
PALETTE_DIVERGING    <- RColorBrewer::brewer.pal(11, "RdBu")
PALETTE_SEQUENTIAL   <- viridis::viridis(9, option = "plasma")
PALETTE_CATEGORICAL  <- ggthemes::tableau_color_pal("Tableau 10")(10)

# ── 3. Save helper ────────────────────────────────────────────────────────────
#' Save a ggplot with publication-ready dimensions
ggsave_industrial <- function(plot, path, width = 8, height = 5.5, dpi = 300) {
  ggplot2::ggsave(
    filename = path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  message("Saved: ", path)
}

# ── 4. Event study plot (publication version) ─────────────────────────────────
plot_event_study_pub <- function(es_data,
                                  title    = "Effect of Industrial Policy on Population Growth",
                                  subtitle = "Callaway-Sant'Anna (2021) dynamic ATT; 95% uniform confidence bands") {
  ggplot(es_data, aes(x = event.time, y = estimate)) +
    annotate("rect",
             xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "#f5f5f5", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "longdash", color = "gray60", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "#d73027", alpha = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#2166ac", alpha = 0.15) +
    geom_line(color = "#2166ac", linewidth = 0.8) +
    geom_point(color = "#2166ac", size = 2.5, fill = "white", shape = 21, stroke = 1.2) +
    geom_segment(aes(x = event.time, xend = event.time, y = conf.low, yend = conf.high),
                 color = "#2166ac", linewidth = 0.4, alpha = 0.5) +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Years relative to industrial zone designation",
      y        = "ATT on log population (%)",
      caption  = "Pre-treatment region shaded. Vertical dashed line = policy adoption."
    ) +
    theme_industrial()
}

# ── 5. Scenario projection plot ───────────────────────────────────────────────
plot_scenarios <- function(unc_data, selected_dept = NULL) {
  dat <- if (!is.null(selected_dept)) {
    unc_data |> filter(geo_dept_code == selected_dept)
  } else {
    unc_data |>
      group_by(year, scenario) |>
      summarise(across(starts_with("pop_"), mean), .groups = "drop")
  }

  ggplot(dat, aes(x = year, y = pop_mean, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = pop_q2.5, ymax = pop_q97.5), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c("factual" = "#d73027", "counterfactual_no_policy" = "#4393c3"),
      labels = c("factual" = "With policy", "counterfactual_no_policy" = "No-policy counterfactual")
    ) +
    scale_fill_manual(
      values = c("factual" = "#d73027", "counterfactual_no_policy" = "#4393c3"),
      labels = c("factual" = "With policy", "counterfactual_no_policy" = "No-policy counterfactual")
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    labs(
      title    = "Population Projection Scenarios",
      subtitle = "Factual (with industrial policy) vs. counterfactual (no policy); 95% simulation bands",
      x        = "Year",
      y        = "Projected Population",
      color    = NULL, fill = NULL
    ) +
    theme_industrial() +
    theme(legend.position = "bottom")
}

# ── 6. Choropleth map: treatment intensity ────────────────────────────────────
plot_choropleth <- function(spatial_sf, var, legend_title = NULL,
                             title = NULL, palette = PALETTE_SEQUENTIAL) {
  ggplot(spatial_sf, aes(fill = .data[[var]])) +
    geom_sf(color = "white", linewidth = 0.1) +
    scale_fill_gradientn(
      colors = palette,
      name   = legend_title %||% var,
      labels = comma
    ) +
    labs(title = title) +
    theme_industrial_map()
}

# ── 7. OD flow map ────────────────────────────────────────────────────────────
plot_od_flows <- function(od_summary, boundaries, top_n = 20L) {
  # Get centroids for each province
  centroids <- boundaries |>
    group_by(geo_prov_code) |>
    summarise(geometry = sf::st_union(geometry)) |>
    mutate(centroid = sf::st_centroid(geometry)) |>
    mutate(
      lon = sf::st_coordinates(centroid)[, 1],
      lat = sf::st_coordinates(centroid)[, 2]
    ) |>
    sf::st_drop_geometry()

  flows_top <- od_summary |>
    arrange(desc(flow)) |>
    slice_head(n = top_n) |>
    left_join(centroids |> rename(origin = geo_prov_code, lon_o = lon, lat_o = lat),
              by = "origin") |>
    left_join(centroids |> rename(destination = geo_prov_code, lon_d = lon, lat_d = lat),
              by = "destination")

  ggplot() +
    geom_sf(data = boundaries |> group_by(geo_prov_code) |>
              summarise(geometry = sf::st_union(geometry)),
            fill = "gray96", color = "gray70", linewidth = 0.3) +
    geom_curve(
      data = flows_top,
      aes(x = lon_o, y = lat_o, xend = lon_d, yend = lat_d, linewidth = flow),
      arrow    = arrow(length = unit(0.15, "cm"), type = "closed"),
      color    = "#d73027", alpha = 0.5, curvature = 0.25
    ) +
    scale_linewidth_continuous(range = c(0.3, 2)) +
    labs(
      title     = glue::glue("Top {top_n} Interprovincial Migration Flows"),
      linewidth = "Flow volume"
    ) +
    theme_industrial_map()
}

# ── 8. Population pyramid comparison ─────────────────────────────────────────
plot_pyramid_compare <- function(micro, years = c(1991, 2022),
                                  treatment_status = "treated") {
  dat <- micro |>
    filter(census_year %in% years, !is.na(sex_label), !is.na(age_group5)) |>
    group_by(census_year, sex_label, age_group5, treated) |>
    summarise(pop = sum(hh_weight, na.rm = TRUE), .groups = "drop") |>
    filter(treated == (treatment_status == "treated")) |>
    mutate(pop_signed = if_else(sex_label == "Male", -pop, pop))

  ggplot(dat, aes(x = pop_signed, y = age_group5, fill = sex_label)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ census_year) +
    scale_x_continuous(labels = function(x) comma(abs(x))) +
    scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#d6604d")) +
    labs(
      title    = glue::glue("Population Pyramids — {paste(years, collapse = ' vs. ')}"),
      subtitle = glue::glue("{tools::toTitleCase(treatment_status)} departments"),
      x        = "Population",
      y        = "Age Group",
      fill     = NULL
    ) +
    theme_industrial()
}
