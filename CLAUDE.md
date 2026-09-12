# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## What this is

A statistical power analysis for a Before–After Control–Impact (BACI) track-detection monitoring
design, for the Toad Containment Zone (TCZ) program (GitHub: `PopBiolGen/tcz-power`). It estimates
how many sand-plot/road-transect monitoring plots per treatment group are needed to reliably detect
a given relative decline in animal track detection probability following an impact event — i.e. the
monitoring-design counterpart to `../invasion-front-monitoring` and `../TCZ-sims`'s spread models.
This is a research script repository, not a package: no `DESCRIPTION`/`renv.lock`, no test suite, no
build/lint tooling. Scripts run top-to-bottom via `Rscript` or from RStudio
(`tcz-bio-monitoring-power.Rproj`).

`dat/`, `out/`, `*.Rproj`, and `*.html` are all gitignored — only `src/*.R` and `ms/*.qmd` are
tracked. That means the repo as checked out has no data and no cached results; both must be
regenerated (see below) or supplied from wherever the historical survey data lives before anything
here will run.

## Running things

Two alternative end-to-end pipelines, both entered by sourcing everything from the repo root:

```r
source("src/05_run_all.R")       # 2ha-sandplot baselines only
source("src/07_run_all_roads.R") # road-transect baselines + 2ha sigma (current/preferred)
```

Both scripts delete `out/power_full_grid.rds`/`out/power_focal_grid.rds` first (forcing a full
re-simulation — see the caching note below), then run `01_data_prep.R` → (`06_compare_rd_transects.R`
for the roads variant only) → `03_run_simulations.R` (~10–15 min on 6 cores) → `04_figures.R` →
`quarto::quarto_render()` on a `ms/*.qmd` report. Individual stages can also be sourced directly for
iterating on one part (e.g. re-running just `04_figures.R` after tweaking a plot).

**Known mismatch**: `05_run_all.R` renders `ms/power_analysis.qmd`, but that report's setup chunk
reads `out/species_baselines_road.rds` — the output of `06_compare_rd_transects.R`, which
`05_run_all.R` never runs. `ms/power_analysis_2ha_plots.qmd` is the report actually paired with
`05_run_all.R`'s own output (`out/species_baselines.rds`), but nothing currently renders it
automatically. On a clean run, `05_run_all.R` will fail at the render step unless
`species_baselines_road.rds` already exists from a prior `07_run_all_roads.R` run — either fix the
qmd target when touching this, or just use `07_run_all_roads.R`, which is internally consistent and
is what the current `ms/power_analysis.qmd` content assumes.

## Data inputs (`dat/`, gitignored)

- `Detection Table.csv` — per-species detection frequency/probability from historical 2ha sand-plot
  surveys (the LaGrange-region Bilby distribution study; "between June and November", i.e. cold
  season, across a range of habitat types).
- `Site variance.csv` — between-plot variance components (`lme4`-style `grp`/`sdcor` output) from
  the same 2ha sand-plot data, keyed by species (`col`). This is the **only** source of between-plot
  variability (`sigma`) in the whole analysis — it's the only dataset with repeat site visits, so
  both pipelines borrow it even when baseline `p` comes from elsewhere.
- `road-transects-pilot.csv` — a much smaller pilot dataset (February 2024, wet season, 100m
  transects along sandy tracks only, sandy country only). Produces substantially higher baseline
  detection probabilities than the 2ha plots (see `fig-compare-baselines` in the road-based report) —
  attributed to season/habitat, not treated as a correction to the 2ha numbers.
- `Table_9_sign_plot_detections.xlsx` — present but not read by any script; looks like the original
  source table `Detection Table.csv`/`Site variance.csv` were derived from. Treat as background
  reference, not a pipeline input, unless you're re-deriving those CSVs from scratch.

Species-name conventions differ by source and require manual reconciliation, not an automated join:
`Detection Table.csv`/`Site variance.csv` use dot-joined compound names (e.g.
`Legless.lizard.Sand.slider.spp`), while `road-transects-pilot.csv`'s columns are short
lower-case/hyphenated field codes (`t-multifasciata`, `m-lagotis`, `turkey`) manually `rename()`d to
match in `06_compare_rd_transects.R`. If the pilot CSV's columns ever change (new species, renamed
column), that `rename()` call needs updating by hand — it will silently mismatch or error rather
than warn.

## Pipeline architecture (`src/`)

Scripts are numbered in run order; each writes its output to `out/` (also gitignored) for the next
stage to read, rather than passing objects in-session — except where a driver script (`05_run_all.R`
/ `07_run_all_roads.R`) sources several in sequence within one session:

1. **`01_data_prep.R`** — merges `Detection Table.csv` + `Site variance.csv` on species name into
   `out/species_baselines.rds` (`species`, `p_baseline`, `sigma`).
2. **`06_compare_rd_transects.R`** *(roads pipeline only)* — recomputes per-species mean detection
   from the road-transect pilot data, joins it against the 2ha plots' `sigma` (not their `p`), plots
   the two datasets' baseline `p` against each other (`out/figures/baseline-p-comparison.png`), and
   writes `out/species_baselines_road.rds` (`species`, `sigma` from 2ha, `p_baseline` from roads).
3. **`02_sim_function.R`** — defines `sim_power_baci(p0, sigma, n_per_group, effect, n_before, n_after,
   n_sim, alpha)`: Monte Carlo power for a random-intercept-per-plot binomial GLMM
   (`glmer(y ~ period * treatment + (1 | plot), family = binomial)`, BOBYQA optimiser) testing the
   period×treatment interaction. `effect` is expressed as *proportion of baseline retained* after
   impact (0.75 = 25% reduction), not the reduction itself. `p0` is capped at 0.999 (species at `p=1`
   are analytically degenerate — no variance in the control group). Wrapped in nested `tryCatch` so a
   failed `glmer` fit (or a C-level error escaping from a parallel worker) returns `NA` rather than
   killing the whole grid.
4. **`03_run_simulations.R`** — builds two parameter grids and calls `sim_power_baci()` over every
   cell via `parallel::mcmapply` (`N_CORES = min(detectCores()-1, 6)`, `N_SIM = 200`, seed `9034`):
   - **full grid** — all species in `species_df` × `n_per_group ∈ {20, 35, 50}` × three effect sizes
     → `out/power_full_grid.rds`. (A comment at the top of this file still says `n = 20, 25, 30`;
     the actual grid is `{20, 35, 50}` — trust the code, not the header comment, and fix the comment
     if you touch this again.)
   - **focal grid** — four hardcoded species (`Cat`, `Bilby`, `Goanna`, `Bustard`) × `n_per_group`
     from 20 to 200 × the same three effect sizes → `out/power_focal_grid.rds`. If a focal species
     name doesn't match `species_df$species` exactly (see the naming-convention note above), it's
     silently dropped by the `filter()`, not errored.
   - **Caching**: `run_or_load()` skips simulation entirely if the target `.rds` already exists —
     changing `N_SIM`, the effect sizes, or either grid's `n_per_group` range has **no effect** on a
     re-run unless you delete the corresponding `out/*.rds` first. Both driver scripts do this for
     `power_full_grid.rds`/`power_focal_grid.rds` up front for exactly this reason; a bare
     `source("src/03_run_simulations.R")` will not.
5. **`04_figures.R`** — reads both grids, writes `out/figures/fig_power_vs_p.png` (power vs. baseline
   `p`, all species, faceted by effect size, coloured by `n`), `out/figures/fig_power_curves.png`
   (power vs. `n`, focal species, faceted by species, coloured by effect size), and
   `out/figures/tbl_power_n35.csv` (summary table at `n = 35`/group). All three read from whichever
   grid `.rds` files are on disk — no notion of which pipeline (2ha vs. roads) produced them, so
   running this after a partial/mixed pipeline run will silently mix data sources.
   (`out/figures/tbl_power_n25.csv` also exists on disk in a populated checkout — that's a stale
   artifact from before the full grid's `n_per_group` set changed from `{20,25,30}` to `{20,35,50}`;
   current `04_figures.R` doesn't produce it.)

`80% power` is the only threshold annotated on the figures; there's no programmatic "minimum
adequate n" lookup — read it off the plotted/tabulated power curves.

## Report (`ms/`)

Two Quarto documents, same title and structure, differing only in which baseline dataset they
report on (see the `readRDS()` calls in each's setup chunk):

- **`power_analysis.qmd`** — reads `species_baselines_road.rds` (road-transect `p`, 2ha `sigma`).
  Paired with `07_run_all_roads.R`. Includes the `fig-compare-baselines` section justifying the
  road/2ha data blend; this is the current, more complete report.
- **`power_analysis_2ha_plots.qmd`** — reads `species_baselines.rds` (2ha-only). Paired with
  `05_run_all.R`'s own output, but not rendered by that script (see "Known mismatch" above).

Both embed all figures/tables as pre-rendered images/CSVs from `out/` (`embed-resources: true`) — the
qmd itself does no simulation, so re-rendering after only editing prose is fast, but re-rendering
after changing simulation parameters requires re-running the R pipeline first.

## `src/goanna-burrow-power/` — a second, independent BACI power analysis

A sibling analysis to the sand-plot/road-transect pipeline above, for the `goanna-hunting-app`
burrow-search pilot data (NTA-KJ, first field tranche, 3 field days). Same BACI design (3 years
before + 3 after) but structurally different: no fixed sites (unit of replication is the individual
search/inspected burrow, not a revisited plot), a random year intercept instead of a site intercept,
and exponential/time-to-event + binomial responses instead of a single binomial-on-a-site model.
Reads pilot data directly from `DATA_PATH` (see `../goanna-burrow-analysis/CLAUDE.md`) rather than
depending on that repo's R code — cross-repo coupling here is data-only, per the parent `CLAUDE.md`.

Run via `source("src/goanna-burrow-power/05_run_all.R")` from this repo's root; outputs land in the
same top-level `out/` (distinct filenames: `baselines.rds`, `encounter_rate_grid.rds`,
`species_grid.rds`, `out/figures/fig_encounter_rate_power.png`,
`out/figures/fig_species_power.png`, `out/figures/tbl_singular_fits.csv`) — no collision with the
sand-plot pipeline's own `out/*.rds`.

- **Response 1** — wandidji burrow encounter rate. `burrow_type == "wandidji"` is the event;
  non-wandidji outcomes are right-censored at the observed search duration, not dropped. Fit as a
  Poisson GLMM with `offset(log(duration))` (the cheap equivalent of censored exponential
  regression) and a `(1 | year)` random intercept.
- **Response 2** — proportion of inspected burrows containing each of 4 species (king brown,
  wanggali/sand goanna, wandidji/spiny-tailed goanna, northern bluetongue) — 4 independent binomial
  GLMMs (`(1 | year)`), not a joint multinomial, since the underlying field is multi-select.
- **Sample size is derived, not a free grid axis.** The number of search/inspected-burrow replicates
  achievable per design cell is `days_per_cell × observed pilot throughput × rate_multiplier`
  (multiplier = 1 except the impact-after cell, where it equals the assumed rate-decline `effect`)
  — more field days yield more replicates, but a real decline in the underlying rate also shrinks
  how many replicates a fixed days budget can produce. `days_per_cell` is therefore the grid's
  effort axis, not `n_per_group`.
- **A field-protocol gap in the pilot** (`burrow_type` unrecorded on the first field day, partly
  missing on the second) is handled two ways in `00_load_pilot_data.R` — `impute` (empirical
  wandidji-rate imputation) and `drop_day1` (exclude the fully-untyped day) — both carried through
  response 1's grid as a labelled sensitivity pair rather than picking one silently.
- **`sigma_year`** (between-year variance) has no pilot estimate (the trial is one month) and is
  swept as a low/moderate/high sensitivity scenario, not fixed at a guessed value. With only 6
  year-levels, expect frequent singular/boundary GLMM fits — `singular_fraction` is reported
  alongside `power` in every grid cell for exactly this reason; check
  `out/figures/tbl_singular_fits.csv` before trusting a given cell's power estimate.

## Notes for editing

- Don't add a new figure/table to `04_figures.R` without checking both qmd files for whether it
  should appear in one, the other, or both — they're currently kept in sync by hand, not generated
  from a shared template.
- If you change which dataset a report's baseline `p` comes from, keep the `readRDS()` target in the
  qmd's setup chunk and the driver script that's supposed to feed it pointed at the same pipeline —
  that's exactly the drift that produced the current `05_run_all.R` mismatch.
- Historical survey data (`dat/`) is not fetched by any script here (unlike `../invasion-front-monitoring`
  or `../TCZ-sims`, which pull from an external `DATA_PATH`) — since it's gitignored with no external
  fetch step, treat it as supplied out-of-band, and don't assume a clean checkout can regenerate
  `out/` without first sourcing that data from wherever it currently lives.
