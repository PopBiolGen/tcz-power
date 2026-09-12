# 01_sim_function_encounter_rate.R
# Defines sim_power_baci_exponential(): simulate power for a BACI wandidji
# burrow encounter-rate design via Monte Carlo.
#
# Model:
#   log(rate_ijt) = log(lambda0) + u_j + log(effect)*After_t*Impact_i
#   u_j ~ N(0, sigma_year^2)   -- one deviation per year (6 total: 3 before +
#                                 3 after), shared by both treatment arms
#                                 surveyed in that year (a common
#                                 environmental shock) -- crossed with
#                                 treatment, nested within period.
#
# Each search is a time-to-event trial: true detection time
# T ~ Exponential(rate), right-censored at an empirically-drawn "gave up /
# found something else" time. event = 1 if T <= censor time.
#
# Sample size is NOT a free input. The number of searches achievable in a
# design cell is `days_per_cell * throughput0 * rate_multiplier`, where
# rate_multiplier is 1 everywhere except the impact-after cell, where it's
# `effect` -- the same decline that reduces the hazard also means fewer
# searches get completed in the same days of fieldwork (see plan/CLAUDE.md
# for the reasoning). The unit of replication remains the individual search;
# what's derived is how many such replicates a fixed effort budget yields.
#
# Fit as a Poisson GLMM with offset(log(duration)) -- the standard
# computationally-cheap equivalent of censored exponential regression --
# testing the period:treatment interaction via a Wald z-test, exactly as
# ../02_sim_function.R does for the binomial design.

library(lme4)
library(tidyverse)

#' Simulate BACI power for a wandidji encounter-rate design
#'
#' @param lambda0           Numeric. Baseline wandidji encounter rate
#'                           (events per second of search), control/before.
#' @param throughput0        Numeric. Baseline searches per field day
#'                           (observed pilot throughput).
#' @param effect             Numeric. Proportion of baseline rate retained in
#'                           impact searches after impact (0.75 = 25%
#'                           reduction). Also scales the impact-after cell's
#'                           achievable search count (see header).
#' @param sigma_year         Numeric. Between-year SD on the log-rate scale.
#' @param days_per_cell      Numeric. Field-days of effort in each of the 4
#'                           design cells (control/impact x before/after).
#' @param censor_time_pool   Numeric vector. Empirical pool of "gave up /
#'                           found something else" search durations (seconds)
#'                           to sample censoring times from.
#' @param n_sim              Integer. Number of Monte Carlo replicates.
#' @param n_before           Integer. Number of before-impact years.
#' @param n_after            Integer. Number of after-impact years.
#' @param alpha              Numeric. Significance threshold.
#'
#' @return List with `power` (proportion of significant simulations) and
#'   `singular_fraction` (proportion of fits with a singular/boundary
#'   (1 | year) variance estimate).
sim_power_baci_exponential <- function(lambda0,
                                        throughput0,
                                        effect,
                                        sigma_year,
                                        days_per_cell,
                                        censor_time_pool,
                                        n_sim    = 200L,
                                        n_before = 3L,
                                        n_after  = 3L,
                                        alpha    = 0.05) {

  tryCatch({

    n_years <- n_before + n_after

    cells <- tibble(
      treatment       = c(0L, 0L, 1L, 1L),
      period          = c(0L, 1L, 0L, 1L),
      rate_multiplier = c(1,   1,   1,   effect)
    ) |>
      mutate(n_cell = pmax(round(days_per_cell * throughput0 * rate_multiplier), 1L))

    significant <- logical(n_sim)
    singular    <- logical(n_sim)

    for (i in seq_len(n_sim)) {
      u_year <- rnorm(n_years, mean = 0, sd = sigma_year)

      df <- pmap_dfr(cells, function(treatment, period, rate_multiplier, n_cell) {
        year_choices <- if (period == 0L) seq_len(n_before) else n_before + seq_len(n_after)
        year_id      <- sample(year_choices, n_cell, replace = TRUE)
        rate         <- lambda0 * exp(u_year[year_id]) * rate_multiplier

        true_time   <- rexp(n_cell, rate = rate)
        censor_time <- sample(censor_time_pool, n_cell, replace = TRUE)

        tibble(
          treatment = treatment,
          period    = period,
          year      = factor(year_id, levels = seq_len(n_years)),
          duration  = pmin(true_time, censor_time),
          event     = as.integer(true_time <= censor_time)
        )
      })

      fit <- tryCatch(
        suppressWarnings(suppressMessages(
          glmer(event ~ period * treatment + (1 | year) + offset(log(duration)),
                data    = df,
                family  = poisson,
                control = glmerControl(optimizer = "bobyqa"))
        )),
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
    warning("sim_power_baci_exponential failed for lambda0=", signif(lambda0, 3),
            ", effect=", effect, ", days_per_cell=", days_per_cell, ": ",
            conditionMessage(e))
    list(power = NA_real_, singular_fraction = NA_real_)
  })
}
