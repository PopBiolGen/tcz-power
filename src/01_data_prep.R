# 01_data_prep.R
# Load sand-plot sign detection data and compute per-species baseline
# detection probabilities.
#
#
# Output: out/species_baselines.rds

library(tidyverse)
library(readxl)

# get observed frequencies
obs.freq <- read.csv("dat/Detection Table.csv")

# get observed between site variances
est.var <- read.csv("dat/Site variance.csv")

# merge the two data frames on species name
species_df <- merge(obs.freq, est.var)

dir.create("out", showWarnings = FALSE)
saveRDS(species_df, "out/species_baselines.rds")

message("Saved: out/species_baselines.rds  (", nrow(species_df), " species)")
