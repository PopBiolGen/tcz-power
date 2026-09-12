# 03_run_simulations.R
# Run power simulations for both responses across effect size, inter-annual
# variability (sigma_year), and field-effort (days_per_cell) grids.
#
# Two grids are produced:
#   encounter_rate_grid   -- wandidji encounter rate (response 1), both
#                             lambda0 variants (impute / drop_day1)
#   species_grid          -- proportion of inspected burrows containing each
#                             of 4 species (response 2), activity_effect
#                             coupled 1:1 to composition_effect by default
#                             (see 02_sim_function_species_proportion.R)
#
# Results are cached in out/; re-running this script skips any grid whose
# output already exists -- delete the relevant .rds to force a re-run
# (mirrors ../03_run_simulations.R's run_or_load() gotcha).
#
# `days_per_cell` ranges differ a lot between the two grids: response 1's
# baseline per-search event probability is high enough that a handful of
# field days already gives strong power, while response 2's rarest species
# (~1-5% baseline prevalence) need effort an order of magnitude larger
# before power curves become informative -- see plan/CLAUDE.md.

library(tidyverse)
library(parallel)

baselines <- readRDS("out/baselines.rds")

# ── Simulation settings ───────────────────────────────────────────────────────
N_SIM      <- 200L
EFFECTS    <- c(reduction_25pct = 0.75, reduction_50pct = 0.50, reduction_75pct = 0.25)
SIGMA_YEAR <- c(low = 0.2, moderate = 0.75, high = 1.5)  # logit/log-rate scale,
  # unmeasured (no multi-year NTA-KJ data exists yet) -- "moderate" is loosely
  # anchored to the existing ms's own Goanna between-plot logit-SD (~1.15,
  # dat/Site variance.csv) as an order-of-magnitude reference, not a direct
  # estimate of inter-annual variance.
N_CORES    <- min(parallel::detectCores() - 1L, 6L)

set.seed(9034)

run_or_load <- function(cache_path, params, label, sim_fn, arg_cols) {
  if (file.exists(cache_path)) {
    message("Loading cached: ", cache_path)
    return(readRDS(cache_path))
  }
  message("Simulating ", label, " (", nrow(params), " cells, ", N_CORES, " cores) ...")

  results <- do.call(
    mcmapply,
    c(list(FUN = sim_fn, SIMPLIFY = FALSE, mc.cores = N_CORES,
           MoreArgs = list(n_sim = N_SIM)),
      as.list(params[arg_cols]))
  )

  power_vals    <- map_dbl(results, \(r) if (is.list(r)) r$power else NA_real_)
  singular_vals <- map_dbl(results, \(r) if (is.list(r)) r$singular_fraction else NA_real_)

  result <- params |> mutate(power = power_vals, singular_fraction = singular_vals)
  saveRDS(result, cache_path)
  message("Saved: ", cache_path)
  result
}

# ── 1. Encounter-rate grid (response 1) ──────────────────────────────────────
source("src/goanna-burrow-power/01_sim_function_encounter_rate.R")

DAYS_PER_CELL_RATE <- c(1, 2, 5, 10, 20)

lambda0_variants <- list(
  impute    = baselines$lambda0_impute,
  drop_day1 = baselines$lambda0_drop_day1
)

encounter_rate_params <- map_dfr(names(lambda0_variants), function(variant) {
  v <- lambda0_variants[[variant]]
  crossing(
    lambda0_variant = variant,
    lambda0         = v$lambda0,
    throughput0     = v$throughput_per_day,
    days_per_cell   = DAYS_PER_CELL_RATE,
    effect          = EFFECTS,
    sigma_year      = SIGMA_YEAR
  )
}) |>
  mutate(
    effect_label     = names(EFFECTS)[match(effect, EFFECTS)],
    sigma_year_label = names(SIGMA_YEAR)[match(sigma_year, SIGMA_YEAR)]
  )

encounter_rate_grid <- run_or_load(
  "out/encounter_rate_grid.rds", encounter_rate_params, "encounter-rate grid",
  sim_fn   = \(lambda0, throughput0, effect, sigma_year, days_per_cell, n_sim)
    sim_power_baci_exponential(lambda0 = lambda0, throughput0 = throughput0,
                                effect = effect, sigma_year = sigma_year,
                                days_per_cell = days_per_cell,
                                censor_time_pool = baselines$censor_time_pool,
                                n_sim = n_sim),
  arg_cols = c("lambda0", "throughput0", "effect", "sigma_year", "days_per_cell")
)

# ── 2. Species-proportion grid (response 2) ──────────────────────────────────
source("src/goanna-burrow-power/02_sim_function_species_proportion.R")

DAYS_PER_CELL_SPECIES <- c(5, 10, 20, 40, 80)

species_params <- crossing(
  baselines$species_baselines |> select(species_label, p0),
  days_per_cell = DAYS_PER_CELL_SPECIES,
  effect        = EFFECTS,   # used for BOTH activity_effect and
                              # composition_effect by default -- see
                              # 02_sim_function_species_proportion.R header
  sigma_year    = SIGMA_YEAR
) |>
  mutate(
    effect_label     = names(EFFECTS)[match(effect, EFFECTS)],
    sigma_year_label = names(SIGMA_YEAR)[match(sigma_year, SIGMA_YEAR)],
    throughput0       = baselines$throughput_inspected_per_day
  )

species_grid <- run_or_load(
  "out/species_grid.rds", species_params, "species-proportion grid",
  sim_fn   = \(p0, throughput0, effect, sigma_year, days_per_cell, n_sim)
    sim_power_baci_binomial(p0 = p0, throughput0 = throughput0,
                             activity_effect = effect, composition_effect = effect,
                             sigma_year = sigma_year, days_per_cell = days_per_cell,
                             n_sim = n_sim),
  arg_cols = c("p0", "throughput0", "effect", "sigma_year", "days_per_cell")
)

message("\nAll simulations complete.")
message("Encounter-rate grid: ", nrow(encounter_rate_grid), " cells, mean singular_fraction = ",
        round(mean(encounter_rate_grid$singular_fraction, na.rm = TRUE), 3))
message("Species grid: ", nrow(species_grid), " cells, mean singular_fraction = ",
        round(mean(species_grid$singular_fraction, na.rm = TRUE), 3))
