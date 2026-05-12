# 01_data_prep.R
# Load sand-plot sign detection data and compute per-species baseline
# detection probabilities.
#
# Assumption: Cattle (Bos taurus) were recorded in every plot, so the total
# number of plots surveyed is taken as the Cattle detection count (460).
#
# Output: out/species_baselines.rds

library(tidyverse)
library(readxl)

raw <- read_excel("dat/Table_9_sign_plot_detections.xlsx")

n_total_plots <- raw |>
  filter(Animal == "Cattle (Bos taurus)") |>
  pull(`Frequency of observations`)

species_df <- raw |>
  rename(species = Animal, detections = `Frequency of observations`) |>
  mutate(p_baseline = detections / n_total_plots) |>
  arrange(desc(p_baseline))

dir.create("out", showWarnings = FALSE)
saveRDS(species_df, "out/species_baselines.rds")

message("Saved: out/species_baselines.rds  (", nrow(species_df), " species, ",
        n_total_plots, " total plots)")
