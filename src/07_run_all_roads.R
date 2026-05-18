file.remove("out/power_full_grid.rds", "out/power_focal_grid.rds")


source("src/01_data_prep.R")
source("src/02_sim_function.R")
source("src/06_compare_rd_transects.R")

species_df <- readRDS("out/species_baselines_road.rds")

source("src/03_run_simulations.R")  # ~10–15 min
source("src/04_figures.R")

quarto::quarto_render("ms/power_analysis.qmd")

