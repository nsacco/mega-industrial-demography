# =============================================================================
# 04_models/bayesian_models.R
# Hierarchical Bayesian models: small-area estimation, partial pooling
# Uses {brms} and {INLA}
# =============================================================================

library(tidyverse)
library(brms)
library(posterior)
library(bayesplot)
library(arrow)
library(here)
library(logger)

source(here("R/functions/utils.R"))

PROC_DIR <- here("data/processed")

# ── 1. Hierarchical model for small-area population estimation ────────────────
#' Bayesian hierarchical model: partial pooling across departments
#' within provinces; accounts for small sample sizes in sparse areas.
#'
#' Model:
#'   log_pop[i,t] ~ Normal(mu[i,t], sigma)
#'   mu[i,t]      = alpha[dept] + beta_treated * treated[i,t] + X[i,t] * gamma
#'   alpha[dept]  ~ Normal(alpha_prov[prov[dept]], sigma_dept)
#'   alpha_prov[p] ~ Normal(0, sigma_prov)
specify_hier_model <- function() {
  brms::bf(
    log_pop ~ treated_num + treecover_pct + census_year_scaled +
              (1 | geo_prov_code / geo_dept_code),
    autocor = ~ ar(time = census_year, gr = geo_dept_code, p = 1)
  )
}

# ── 2. Prior specification ────────────────────────────────────────────────────
#' Weakly informative priors (principled defaults for demographic outcomes)
specify_priors <- function() {
  c(
    brms::prior(normal(0, 1),    class = b),                     # Fixed effects
    brms::prior(normal(8, 2),    class = Intercept),             # Log-pop intercept
    brms::prior(exponential(1),  class = sd),                    # Random effect SDs
    brms::prior(exponential(1),  class = sigma),                 # Residual SD
    brms::prior(normal(0.5, 0.3),class = ar, coef = ar1)        # AR(1) coefficient
  )
}

# ── 3. Prior predictive check ─────────────────────────────────────────────────
prior_predictive_check <- function(formula, priors, dat_small) {
  log_info("Running prior predictive check.")
  brm(
    formula   = formula,
    data      = dat_small,
    prior     = priors,
    sample_prior = "only",
    chains    = 2L,
    iter      = 1000L,
    cores     = 2L,
    seed      = 42L,
    backend   = "cmdstanr"
  )
}

# ── 4. Main model fit ─────────────────────────────────────────────────────────
fit_bayesian_model <- function(dat, formula = specify_hier_model(),
                                priors = specify_priors()) {
  log_info("Fitting hierarchical Bayesian model (brms).")

  dat_scaled <- dat |>
    mutate(
      treated_num        = as.integer(treated),
      census_year_scaled = (census_year - mean(census_year, na.rm = TRUE)) /
                           sd(census_year, na.rm = TRUE)
    ) |>
    filter(!is.na(log_pop), !is.na(treecover_pct))

  fit <- brm(
    formula  = formula,
    data     = dat_scaled,
    prior    = priors,
    chains   = 4L,
    iter     = 4000L,
    warmup   = 2000L,
    cores    = 4L,
    seed     = 42L,
    backend  = "cmdstanr",
    control  = list(adapt_delta = 0.95, max_treedepth = 12L),
    file     = file.path(PROC_DIR, "bayesian_model_fit")   # cache
  )

  log_info("  Converged: {all(brms::rhat(fit) < 1.01, na.rm = TRUE)}")
  fit
}

# ── 5. INLA small-area model (alternative) ───────────────────────────────────
#' INLA-based BYM2 model for spatial smoothing of department-level estimates
#' BYM2 = Riebler et al. (2016) spatially structured random effects
fit_inla_model <- function(dat, adj_matrix) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    log_warn("INLA not installed. Skipping INLA model.")
    return(NULL)
  }
  library(INLA)
  log_info("Fitting INLA BYM2 model.")

  # Create INLA graph from adjacency matrix
  g <- INLA::inla.read.graph(adj_matrix)

  dat_inla <- dat |>
    mutate(
      dept_idx = as.integer(factor(geo_dept_code)),
      pop_int  = round(pop)
    ) |>
    filter(!is.na(pop_int), pop_int > 0)

  formula_inla <- pop_int ~ treated_num + treecover_pct +
    f(dept_idx, model = "bym2", graph = g,
      hyper = list(
        phi   = list(prior = "pc", param = c(0.5, 2/3), initial = -3),
        prec  = list(prior = "pc.prec", param = c(1, 0.01), initial = 5)
      ))

  INLA::inla(
    formula  = formula_inla,
    family   = "poisson",
    data     = dat_inla,
    E        = dat_inla$pop_int,  # offset
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE)
  )
}

# ── 6. Posterior predictive checks ────────────────────────────────────────────
run_ppc <- function(fit, n_samples = 100L) {
  log_info("Running posterior predictive checks.")
  y_rep <- brms::posterior_predict(fit, ndraws = n_samples)
  y_obs <- fit$data$log_pop

  bayesplot::ppc_dens_overlay(y = y_obs, yrep = y_rep[1:min(50L, n_samples), ])
}

# ── 7. Small-area estimates (marginal effects) ────────────────────────────────
extract_small_area_estimates <- function(fit) {
  fitted_vals <- fitted(fit, summary = TRUE) |>
    as_tibble() |>
    bind_cols(fit$data |> select(geo_dept_code, census_year, treated_num))

  fitted_vals |>
    group_by(geo_dept_code, census_year) |>
    summarise(
      est_mean     = mean(Estimate),
      est_lower95  = mean(`Q2.5`),
      est_upper95  = mean(`Q97.5`),
      treated      = any(treated_num == 1),
      .groups      = "drop"
    )
}

# ── 8. Treatment effect posterior ────────────────────────────────────────────
extract_treatment_posterior <- function(fit) {
  posterior::as_draws_df(fit) |>
    select(matches("^b_treated")) |>
    pivot_longer(everything(), names_to = "parameter", values_to = "draw") |>
    group_by(parameter) |>
    summarise(
      mean    = mean(draw),
      sd      = sd(draw),
      q2.5    = quantile(draw, 0.025),
      q97.5   = quantile(draw, 0.975),
      prob_pos = mean(draw > 0)
    )
}

# ── 9. Master function ─────────────────────────────────────────────────────────
run_bayesian_models <- function() {
  dat     <- arrow::read_parquet(file.path(PROC_DIR, "spatial_panel.parquet"))
  census  <- arrow::read_parquet(file.path(PROC_DIR, "census_dept_counts.parquet")) |>
    group_by(geo_dept_code, census_year) |>
    summarise(pop = sum(pop_count, na.rm = TRUE), .groups = "drop")

  dat <- dat |>
    left_join(census, by = c("geo_dept_code", "census_year")) |>
    mutate(log_pop = log(pop + 1))

  fit     <- fit_bayesian_model(dat)
  ppc_plt <- run_ppc(fit)
  sae     <- extract_small_area_estimates(fit)
  te_post <- extract_treatment_posterior(fit)

  ggsave_industrial(ppc_plt, here("outputs/figures/fig_bayesian_ppc.png"))
  arrow::write_parquet(sae, file.path(PROC_DIR, "bayesian_sae.parquet"))

  log_info("✓ Bayesian models complete. Treatment effect (mean) = {round(te_post$mean[1], 4)}")
  list(fit = fit, sae = sae, treatment_effect = te_post)
}

if (sys.nframe() == 0L) run_bayesian_models()
