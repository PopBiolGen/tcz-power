# 02_sim_function_species_proportion.R
# Defines sim_power_baci_binomial(): simulate power for a BACI
# proportion-of-inspected-burrows-containing-species-X design via Monte
# Carlo. Structurally the same engine as ../02_sim_function.R's
# sim_power_baci(), with `plot` replaced by `year` (fixed at 6 levels: 3
# before + 3 after, crossed with treatment, nested within period -- a
# shared-environmental-shock random intercept, not a repeated-site effect,
# since there are no fixed sites here).
#
# Two DISTINCT effect parameters, not one:
#   activity_effect     -- how much the OVERALL (any-type) burrow-finding
#                           rate is assumed to decline in the impact-after
#                           cell. This is NOT what's being tested for
#                           significance here -- it only governs how many
#                           inspected burrows exist to sample from in that
#                           cell (n_cell = days_per_cell * throughput0 *
#                           activity_effect), mirroring
#                           ../01_sim_function_encounter_rate.R's sample-size
#                           mechanism. In practice this is set equal to
#                           response 1's wandidji-rate `effect`, on the
#                           simplifying assumption that overall goanna-burrow
#                           activity and wandidji-specific activity decline
#                           together -- a documented assumption, not a
#                           measured fact (see plan/CLAUDE.md).
#   composition_effect  -- the proportion of baseline species-presence
#                           retained in the impact-after cell. THIS is the
#                           parameter actually tested via the
#                           period:treatment interaction -- a shift in what's
#                           found *inside* already-found, already-inspected
#                           burrows, independent of how many burrows get
#                           found in the first place.

library(lme4)
library(tidyverse)

#' Simulate BACI power for a species-presence-in-burrow design
#'
#' @param p0                  Numeric. Baseline presence probability among
#'                            inspected burrows (control/before).
#' @param throughput0         Numeric. Baseline inspected burrows per field
#'                            day (observed pilot throughput).
#' @param activity_effect     Numeric. Proportion of baseline overall
#'                            burrow-finding activity retained in the
#'                            impact-after cell (drives sample size only --
#'                            see header).
#' @param composition_effect  Numeric. Proportion of baseline presence
#'                            probability retained in the impact-after cell
#'                            (the tested effect).
#' @param sigma_year          Numeric. Between-year SD on the logit scale.
#' @param days_per_cell       Numeric. Field-days of effort in each of the 4
#'                            design cells.
#' @param n_sim               Integer. Number of Monte Carlo replicates.
#' @param n_before            Integer. Number of before-impact years.
#' @param n_after             Integer. Number of after-impact years.
#' @param alpha               Numeric. Significance threshold.
#'
#' @return List with `power` and `singular_fraction`.
sim_power_baci_binomial <- function(p0,
                                     throughput0,
                                     activity_effect,
                                     composition_effect,
                                     sigma_year,
                                     days_per_cell,
                                     n_sim    = 200L,
                                     n_before = 3L,
                                     n_after  = 3L,
                                     alpha    = 0.05) {

  tryCatch({

    # Cap p0 (and the shifted probability) to keep logit finite -- mirrors
    # ../02_sim_function.R's p0 cap for the analogous degenerate-p=1 case.
    p0 <- pmin(p0, 0.999)
    baci_logit <- qlogis(pmin(p0 * composition_effect, 0.999)) - qlogis(p0)

    n_years <- n_before + n_after

    cells <- tibble(
      treatment     = c(0L, 0L, 1L, 1L),
      period        = c(0L, 1L, 0L, 1L),
      activity_mult = c(1,   1,   1,   activity_effect)
    ) |>
      mutate(n_cell = pmax(round(days_per_cell * throughput0 * activity_mult), 1L))

    significant <- logical(n_sim)
    singular    <- logical(n_sim)

    for (i in seq_len(n_sim)) {
      u_year <- rnorm(n_years, mean = 0, sd = sigma_year)

      df <- pmap_dfr(cells, function(treatment, period, activity_mult, n_cell) {
        year_choices <- if (period == 0L) seq_len(n_before) else n_before + seq_len(n_after)
        year_id      <- sample(year_choices, n_cell, replace = TRUE)
        logit_p      <- qlogis(p0) + u_year[year_id] + period * treatment * baci_logit
        y            <- rbinom(n_cell, 1L, plogis(logit_p))

        tibble(
          treatment = treatment,
          period    = period,
          year      = factor(year_id, levels = seq_len(n_years)),
          y         = y
        )
      })

      fit <- tryCatch(
        suppressWarnings(
          glmer(y ~ period * treatment + (1 | year),
                data    = df,
                family  = binomial,
                control = glmerControl(optimizer = "bobyqa"))
        ),
        error = function(e) NULL
      )

      if (!is.null(fit)) {
        singular[i] <- isSingular(fit)
        coefs <- coef(summary(fit))
        if ("period:treatment" %in% rownames(coefs)) {
          significant[i] <- coefs["period:treatment", "Pr(>|z|)"] < alpha
        }
      }
    }

    list(power = mean(significant, na.rm = TRUE),
         singular_fraction = mean(singular))

  }, error = function(e) {
    warning("sim_power_baci_binomial failed for p0=", round(p0, 3),
            ", composition_effect=", composition_effect,
            ", days_per_cell=", days_per_cell, ": ", conditionMessage(e))
    list(power = NA_real_, singular_fraction = NA_real_)
  })
}
