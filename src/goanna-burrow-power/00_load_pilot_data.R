# 00_load_pilot_data.R
# Loads the NTA-KJ goanna-burrow pilot data (goanna-hunting-app, first field
# tranche) directly from the DATA_PATH warehouse-extract CSV -- the same
# convention ../../goanna-burrow-analysis/src/a-load-data.R uses -- rather
# than depending on that repo's R code (data-only cross-repo coupling, per
# the parent CLAUDE.md's "don't pull logic across repo boundaries" guidance).
#
# Computes pilot baseline parameters for two BACI power analyses:
#   Response 1 -- wandidji burrow encounter rate (exponential/time-to-event)
#   Response 2 -- proportion of inspected burrows containing each of
#                 4 species (binomial)
#
# Output: out/baselines.rds

library(tidyverse)

if (!nzchar(Sys.getenv("DATA_PATH"))) {
  stop("DATA_PATH is not set; cannot locate the warehouse research export")
}
warehouse_dir <- file.path(Sys.getenv("DATA_PATH"), "Toads", "warehouse-extracts")

dir.create("out", showWarnings = FALSE, recursive = TRUE)

# what_did_you_find arrives as a raw Postgres TEXT[] literal string (e.g.
# "{king_brown,other}"). Mirrors
# ../../goanna-burrow-analysis/src/a-load-data.R's parse_pg_text_array() --
# duplicated here (not sourced) per the data-only cross-repo convention noted
# above.
parse_pg_text_array <- function(x) {
  x <- str_remove(x, "^\\{") |> str_remove("\\}$")
  if_else(is.na(x) | x == "", NA_character_, x) |>
    str_split(fixed(",")) |>
    map(\(v) if (identical(v, NA_character_)) character(0) else v)
}

kj <- read_csv(file.path(warehouse_dir, "goanna_burrow_searches.csv"),
               show_col_types = FALSE) |>
  filter(native_title_area_id == "NTA-KJ") |>
  mutate(what_did_you_find = parse_pg_text_array(what_did_you_find))

n_field_days <- n_distinct(kj$search_date)
message("NTA-KJ pilot: ", nrow(kj), " searches across ", n_field_days,
        " field days (", paste(sort(unique(kj$search_date)), collapse = ", "), ")")

# ── Response 1: wandidji encounter-rate baseline -----------------------------
#
# burrow_type wasn't recorded at all on the first field day (every found
# burrow that day is untyped) and is still partly missing on the second day
# -- both are the same "not always recorded early in the trial" data-quality
# issue, not two different problems. An untyped found burrow is NOT the same
# as "no burrow" and must not be silently counted as a non-wandidji outcome.
#
# Two lambda0 variants are computed and BOTH carried forward rather than
# picking one silently:
#   impute     -- every untyped found burrow (any date) is counted as a
#                 fractional wandidji event, at the empirical wandidji rate
#                 among *typed* found burrows (~46%). Preserves all 139
#                 searches' person-time-at-risk.
#   drop_day1  -- the fully-untyped first field day is excluded entirely
#                 (both from the event count and from time-at-risk); the
#                 smaller residual gap on day 2 is still imputed at the same
#                 empirical rate (dropping day 1 doesn't change that rate,
#                 since day 1 contributed zero typed rows to begin with).
typed_found <- kj |> filter(burrow_found, !is.na(burrow_type))
frac_wandidji_typed <- mean(typed_found$burrow_type == "wandidji")

compute_lambda0 <- function(data) {
  untyped_found_n <- sum(data$burrow_found & is.na(data$burrow_type))
  wandidji_n      <- sum(data$burrow_type == "wandidji", na.rm = TRUE)
  wandidji_events <- wandidji_n + frac_wandidji_typed * untyped_found_n
  total_time_sec  <- sum(data$effective_duration_seconds)
  list(
    lambda0             = wandidji_events / total_time_sec,
    wandidji_events     = wandidji_events,
    total_time_sec      = total_time_sec,
    n_searches          = nrow(data),
    n_field_days        = n_distinct(data$search_date),
    throughput_per_day  = nrow(data) / n_distinct(data$search_date)
  )
}

lambda0_impute    <- compute_lambda0(kj)
lambda0_drop_day1 <- compute_lambda0(kj |> filter(search_date != min(search_date)))

message("lambda0 (impute): ", signif(lambda0_impute$lambda0, 4), "/sec, throughput ",
        round(lambda0_impute$throughput_per_day, 1), " searches/day")
message("lambda0 (drop first day): ", signif(lambda0_drop_day1$lambda0, 4), "/sec, throughput ",
        round(lambda0_drop_day1$throughput_per_day, 1), " searches/day")
if (abs(lambda0_impute$lambda0 - lambda0_drop_day1$lambda0) / lambda0_impute$lambda0 > 0.25) {
  warning("lambda0 impute vs. drop_day1 estimates differ by >25% -- ",
          "check the missing-burrow_type handling before trusting downstream grids")
}

# Censoring-time pool for the encounter-rate simulation: observed durations
# of searches with a *known* non-wandidji outcome (found another type, or
# nothing found). Untyped found burrows are excluded here -- their true
# wandidji status is unknown, so they can't be cleanly labelled as a censored
# (non-wandidji) observation.
censor_time_pool <- kj |>
  filter(!burrow_found | (!is.na(burrow_type) & burrow_type != "wandidji")) |>
  pull(effective_duration_seconds)

# ── Response 2: species-proportion baselines ---------------------------------
#
# Denominator = every inspected burrow, regardless of whether burrow_type was
# recorded (inspection_method is a separate field, unaffected by the
# burrow_type gap above).
inspected <- kj |> filter(!is.na(inspection_method))

species_map <- c(
  king_brown          = "king_brown",
  wanggali            = "sand_goanna",
  wandidji            = "spiny_tailed_goanna",
  northern_bluetongue = "northern_bluetongue"
)

species_baselines <- tibble(
  species_label = names(species_map),
  species_code  = unname(species_map)
) |>
  mutate(
    n_inspected = nrow(inspected),
    n_present   = map_int(species_code, \(code)
                           sum(map_lgl(inspected$what_did_you_find, \(v) code %in% v))),
    p0          = n_present / n_inspected
  )

throughput_inspected_per_day <- nrow(inspected) / n_field_days

message("Species baselines (n inspected = ", nrow(inspected), "):")
print(species_baselines)

baselines <- list(
  lambda0_impute               = lambda0_impute,
  lambda0_drop_day1            = lambda0_drop_day1,
  censor_time_pool             = censor_time_pool,
  species_baselines            = species_baselines,
  throughput_inspected_per_day = throughput_inspected_per_day,
  n_field_days                 = n_field_days
)

saveRDS(baselines, "out/baselines.rds")
message("Saved: out/baselines.rds")
