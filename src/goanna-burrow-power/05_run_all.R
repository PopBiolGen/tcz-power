# 05_run_all.R
# End-to-end driver for the goanna-burrow-power analysis. Run from the
# monitoring-power repo root, e.g.:
#   source("src/goanna-burrow-power/05_run_all.R")
#
# Mirrors ../05_run_all.R's pattern: delete cached grids first (forcing a
# full re-simulation), then source stages in order.

file.remove(
  "out/encounter_rate_grid.rds",
  "out/species_grid.rds"
) |> invisible()

source("src/goanna-burrow-power/00_load_pilot_data.R")
source("src/goanna-burrow-power/03_run_simulations.R")  # sources 01/02 internally
source("src/goanna-burrow-power/04_figures.R")

message("\ngoanna-burrow-power pipeline complete. See out/figures/.")
