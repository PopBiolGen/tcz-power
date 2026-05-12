# 04_figures.R
# Build all figures and summary tables from simulation results.
# Requires out/power_full_grid.rds and out/power_focal_grid.rds (from 03).
#
# Outputs written to out/figures/:
#   fig_power_vs_p.png    — power vs baseline p, all species, both effects
#   fig_power_curves.png  — power vs n, focal species, both effects
#   tbl_power_n25.csv     — summary table at n = 25 per group

library(tidyverse)

dir.create("out/figures", showWarnings = FALSE, recursive = TRUE)

power_full  <- readRDS("out/power_full_grid.rds")
power_focal <- readRDS("out/power_focal_grid.rds")

# ── Shared theme elements ─────────────────────────────────────────────────────
effect_labels <- c(
  reduction_25pct = "25% relative reduction",
  reduction_50pct = "50% relative reduction",
  reduction_75pct = "75% relative reduction"
)

# ── Figure 1: power vs baseline detection probability ────────────────────────
fig_power_vs_p <- power_full |>
  filter(p_baseline < 1, p_baseline >= 0.01) |>
  mutate(
    n_label      = factor(paste0("n = ", n_per_group),
                          levels = paste0("n = ", c(20, 25, 30))),
    effect_label = factor(effect_labels[effect_label],
                          levels = effect_labels)
  ) |>
  ggplot(aes(x = p_baseline, y = power, colour = n_label, group = n_label)) +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_smooth(se = FALSE, span = 1.2, linewidth = 0.8) +
  annotate("text", x = 0.54, y = 0.84, label = "80% power",
           colour = "grey35", size = 3.2, hjust = 1) +
  facet_wrap(~effect_label) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.60)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_brewer(palette = "Set1", name = "Plots per group") +
  labs(
    x = "Baseline detection probability",
    y = "Simulated power"
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("out/figures/fig_power_vs_p.png", fig_power_vs_p,
       width = 9, height = 4.5, dpi = 200)

# ── Figure 2: power vs n for focal species ───────────────────────────────────
fig_power_curves <- power_focal |>
  mutate(
    effect_label = factor(effect_labels[effect_label], levels = effect_labels),
    # Replace line 61 with:
    species = str_remove(species, " \\(.*\\)")  # strip the binomial, keep common name
  ) |>
  ggplot(aes(x = n_per_group, y = power,
             colour = effect_label, group = effect_label)) +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  geom_point(size = 2) +
  geom_line() +
  annotate("text", x = 198, y = 0.84, label = "80% power",
           colour = "grey35", size = 3.2, hjust = 1) +
  facet_wrap(~species, nrow = 2) +
  scale_x_continuous(breaks = c(20, 50, 100, 150, 200)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_manual(
    values = c("25% relative reduction" = "#E41A1C",
               "50% relative reduction" = "#377EB8",
               "75% relative reduction" = "orange"),
    name   = "Minimum detectable effect"
  ) +
  labs(
    x = "Plots per treatment group",
    y = "Simulated power"
  ) +
  theme_bw() +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("out/figures/fig_power_curves.png", fig_power_curves,
       width = 8, height = 6, dpi = 200)

# ── Table: summary at n = 25 per group ───────────────────────────────────────
tbl_power_n25 <- power_full |>
  filter(p_baseline < 1, p_baseline >= 0.01, n_per_group == 25) |>
  select(species, p_baseline, effect_label, power) |>
  pivot_wider(names_from = effect_label, values_from = power,
              names_prefix = "power_") |>
  arrange(desc(p_baseline)) |>
  mutate(
    p_baseline           = round(p_baseline, 3),
    power_reduction_25pct = round(power_reduction_25pct, 2),
    power_reduction_50pct = round(power_reduction_50pct, 2),
    power_reduction_75pct = round(power_reduction_75pct, 2)
  ) |>
  rename(
    `Baseline p`  = p_baseline,
    `Power (25% reduction)` = power_reduction_25pct,
    `Power (50% reduction)` = power_reduction_50pct,
    `Power (75% reduction)` = power_reduction_75pct
  )

write_csv(tbl_power_n25, "out/figures/tbl_power_n25.csv")

message("Figures and table written to out/figures/")
