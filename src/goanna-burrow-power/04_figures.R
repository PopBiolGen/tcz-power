# 04_figures.R
# Build figures and summary tables from simulation results.
# Requires out/encounter_rate_grid.rds and out/species_grid.rds (from 03).
#
# Outputs written to out/figures/:
#   fig_encounter_rate_power.png  -- power vs days of effort/year/area, response 1,
#                                     faceted by sigma_year, coloured by
#                                     effect size, split by lambda0 variant
#   fig_species_power.png         -- power vs days of effort/year/area, response 2,
#                                     faceted by species x sigma_year,
#                                     coloured by effect size
#   tbl_singular_fits.csv         -- singular/boundary GLMM fit fraction by
#                                     grid cell, so low-effort cells with
#                                     unreliable power estimates are visible
#                                     rather than silently trusted
#
# `days_per_cell` (the simulation's own unit) is the TOTAL effort invested in
# one treatment arm across all 3 years of one period -- the sim functions
# spread it evenly (in expectation) across those years. The field crew's
# actual plan is constant annual effort per area (north/impact, south/control)
# sustained every year for 3 years before and 3 after, so figures/tables here
# report `days_per_year_per_area = days_per_cell / N_YEARS_PER_PERIOD` instead
# -- the unit that maps directly onto that planning conversation.

library(tidyverse)

N_YEARS_PER_PERIOD <- 3L  # matches n_before/n_after defaults in 01_/02_sim_function_*.R

dir.create("out/figures", showWarnings = FALSE, recursive = TRUE)

encounter_rate_grid <- readRDS("out/encounter_rate_grid.rds") |>
  mutate(days_per_year_per_area = days_per_cell / N_YEARS_PER_PERIOD)
species_grid <- readRDS("out/species_grid.rds") |>
  mutate(days_per_year_per_area = days_per_cell / N_YEARS_PER_PERIOD)

effect_labels <- c(
  reduction_25pct = "25% relative reduction",
  reduction_50pct = "50% relative reduction",
  reduction_75pct = "75% relative reduction"
)
sigma_labels <- c(
  low      = "Low inter-annual variability",
  moderate = "Moderate inter-annual variability",
  high     = "High inter-annual variability"
)

# ── Figure 1: encounter-rate power vs days of effort ─────────────────────────
fig_encounter_rate <- encounter_rate_grid |>
  mutate(
    effect_label     = factor(effect_labels[effect_label], levels = effect_labels),
    sigma_year_label = factor(sigma_labels[sigma_year_label], levels = sigma_labels),
    lambda0_variant  = str_to_title(str_replace(lambda0_variant, "_", " "))
  ) |>
  ggplot(aes(x = days_per_year_per_area, y = power, colour = effect_label,
             linetype = lambda0_variant, group = interaction(effect_label, lambda0_variant))) +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  geom_point(size = 1.8) +
  geom_line() +
  facet_wrap(~sigma_year_label) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_manual(
    values = c("25% relative reduction" = "#E41A1C",
               "50% relative reduction" = "#377EB8",
               "75% relative reduction" = "orange"),
    name = "Minimum detectable effect"
  ) +
  labs(
    x = "Field days per year, per area (north/south)",
    y = "Simulated power",
    linetype = "Baseline lambda0 estimate",
    title = "Power to detect a decline in wandidji burrow encounter rate"
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("out/figures/fig_encounter_rate_power.png", fig_encounter_rate,
       width = 10, height = 5, dpi = 200)

# ── Figure 2: species-proportion power vs days of effort ─────────────────────
fig_species <- species_grid |>
  mutate(
    effect_label     = factor(effect_labels[effect_label], levels = effect_labels),
    sigma_year_label = factor(sigma_labels[sigma_year_label], levels = sigma_labels)
  ) |>
  ggplot(aes(x = days_per_year_per_area, y = power, colour = effect_label, group = effect_label)) +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  geom_point(size = 1.8) +
  geom_line() +
  facet_grid(species_label ~ sigma_year_label) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_manual(
    values = c("25% relative reduction" = "#E41A1C",
               "50% relative reduction" = "#377EB8",
               "75% relative reduction" = "orange"),
    name = "Minimum detectable effect (composition_effect = activity_effect)"
  ) +
  labs(
    x = "Field days per year, per area (north/south)",
    y = "Simulated power",
    title = "Power to detect a decline in proportion of inspected burrows containing species X"
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("out/figures/fig_species_power.png", fig_species,
       width = 10, height = 9, dpi = 200)

# ── Table: singular-fit fraction by grid cell ────────────────────────────────
tbl_singular_fits <- bind_rows(
  encounter_rate_grid |>
    select(response = lambda0_variant, days_per_year_per_area, effect_label,
           sigma_year_label, singular_fraction) |>
    mutate(response = paste0("encounter_rate (", response, ")")),
  species_grid |>
    select(response = species_label, days_per_year_per_area, effect_label,
           sigma_year_label, singular_fraction)
) |>
  arrange(desc(singular_fraction))

write_csv(tbl_singular_fits, "out/figures/tbl_singular_fits.csv")

n_high_singular <- sum(tbl_singular_fits$singular_fraction > 0.2, na.rm = TRUE)
if (n_high_singular > 0) {
  message(n_high_singular, " grid cells have >20% singular/boundary fits -- ",
          "treat their power estimates cautiously (see out/figures/tbl_singular_fits.csv)")
}

message("Figures and table written to out/figures/")
