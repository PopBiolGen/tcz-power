# 03_run_simulations.R
# Run power simulations for both effect sizes (25% and 50% relative reduction).
#
# Two grids are produced:
#   full_grid   — all species × n = 20, 25, 30 plots per group
#   focal_grid  — four representative species × n = 20–200 plots per group
#
# Results are cached in out/; re-running this script skips any scenario whose
# output already exists. Delete the relevant .rds file to force a re-run.
#
# Runtime: ~10–15 min on 6 cores (200 Monte Carlo reps per cell).

library(tidyverse)
library(parallel)

source("src/01_data_prep.R")
source("src/02_sim_function.R")

species_df <- readRDS("out/species_baselines.rds")

# ── Simulation settings ───────────────────────────────────────────────────────
N_SIM      <- 200L    # Monte Carlo replicates per cell
SIGMA_PLOT <- 0.5     # Between-plot SD on the logit scale (see Methods)
EFFECTS    <- c(reduction_25pct = 0.75, reduction_50pct = 0.50, reduction_75pct = 0.25)
N_CORES    <- min(parallel::detectCores() - 1L, 6L)

set.seed(9034)

# ── Helper: run one grid, cache result ────────────────────────────────────────
run_or_load <- function(cache_path, params, label) {
  if (file.exists(cache_path)) {
    message("Loading cached: ", cache_path)
    return(readRDS(cache_path))
  }
  message("Simulating ", label, " (", nrow(params), " cells, ",
          N_CORES, " cores) ...")
  power_vals <- mcmapply(
    function(p0, n, eff)
      sim_power_baci(p0 = p0, n_per_group = n, effect = eff,
                     sigma_plot = SIGMA_PLOT, n_sim = N_SIM),
    p0  = params$p_baseline,
    n   = params$n_per_group,
    eff = params$effect,
    mc.cores = N_CORES
  )
  result <- mutate(params, power = power_vals)
  saveRDS(result, cache_path)
  message("Saved: ", cache_path)
  result
}

# ── 1. Full grid ──────────────────────────────────────────────────────────────
# All species × n ∈ {20, 25, 30} × both effect sizes

full_params <- crossing(
  select(species_df, species, p_baseline),
  n_per_group = c(20L, 25L, 30L),
  effect      = EFFECTS
) |>
  mutate(effect_label = names(EFFECTS)[match(effect, EFFECTS)])

power_full <- run_or_load("out/power_full_grid.rds", full_params, "full grid")

# ── 2. Focal-species extended grid ────────────────────────────────────────────
# Four representative species across a wide range of n, both effects.
# These are used to show power curves up to adequate sample sizes.

focal_species <- c(
  "Cat (Felis catus)",
  "Bilby (Macrotis lagotis)",
  "Lizard - goanna small (Varanus sp)",
  "Lizard - goanna large (Varanus sp)"
)

focal_params <- crossing(
  filter(species_df, species %in% focal_species) |>
    select(species, p_baseline),
  n_per_group = c(20L, 30L, 50L, 75L, 100L, 150L, 200L),
  effect       = EFFECTS
) |>
  mutate(effect_label = names(EFFECTS)[match(effect, EFFECTS)])

power_focal <- run_or_load("out/power_focal_grid.rds", focal_params,
                           "focal species extended grid")

message("\nAll simulations complete.")
