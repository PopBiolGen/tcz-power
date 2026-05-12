# 02_sim_function.R
# Defines sim_power_baci(): simulate power for a BACI logistic regression
# via Monte Carlo.
#
# Model:
#   logit(p_ijt) = β0 + β1·After_t + β2·Impact_i + β3·After_t·Impact_i + u_i
#   u_i ~ N(0, sigma_plot^2)
#
# The BACI effect (β3) is derived from the assumed baseline detection
# probability and the proportional reduction in the impact group after impact.
# All other fixed effects (β1, β2) are set to 0 — i.e. no pre-existing
# difference between groups, no temporal trend in controls.
#
# Each simulation:
#   1. Draw plot-level random effects.
#   2. Generate binary observations for every plot × year combination.
#   3. Fit a GLMM with a random intercept per plot.
#   4. Test the period × treatment interaction at level alpha.
# Power = proportion of simulations in which H0 is rejected.

library(lme4)

#' Simulate BACI power for a logistic regression design
#'
#' @param p0       Numeric. Baseline detection probability (control, before).
#' @param n_per_group Integer. Number of plots per treatment group.
#' @param effect   Numeric. Proportion of baseline retained in impact plots
#'                 after impact (e.g. 0.75 = 25% reduction, 0.50 = 50%).
#' @param n_before Integer. Number of before-impact survey years.
#' @param n_after  Integer. Number of after-impact survey years.
#' @param sigma_plot Numeric. Between-plot SD on the logit scale.
#' @param n_sim    Integer. Number of Monte Carlo replicates.
#' @param alpha    Numeric. Significance threshold.
#'
#' @return Numeric. Estimated power (proportion of significant simulations).
sim_power_baci <- function(p0,
                           n_per_group,
                           effect      = 0.75,
                           n_before    = 3L,
                           n_after     = 3L,
                           sigma_plot  = 0.5,
                           n_sim       = 200L,
                           alpha       = 0.05) {

  # Cap p0 to keep logit finite; species at p=1 are analytically degenerate
  # (no variation in the control group, making the GLMM ill-posed).
  p0 <- pmin(p0, 0.999)

  baci_logit  <- qlogis(p0 * effect) - qlogis(p0)
  n_years     <- n_before + n_after
  n_plots_tot <- 2L * n_per_group
  n_obs       <- n_plots_tot * n_years

  # Pre-build index vectors — same structure every replicate
  plot_i      <- rep(seq_len(n_plots_tot), each = n_years)
  year_i      <- rep(seq_len(n_years),     times = n_plots_tot)
  treatment_i <- as.integer(plot_i > n_per_group)   # 0 = control, 1 = impact
  period_i    <- as.integer(year_i > n_before)       # 0 = before,  1 = after

  significant <- logical(n_sim)

  for (i in seq_len(n_sim)) {
    u       <- rnorm(n_plots_tot, mean = 0, sd = sigma_plot)
    logit_p <- qlogis(p0) + u[plot_i] + period_i * treatment_i * baci_logit
    y       <- rbinom(n_obs, 1L, plogis(logit_p))

    df <- data.frame(y = y, period = period_i,
                     treatment = treatment_i, plot = plot_i)

    fit <- tryCatch(
      suppressWarnings(
        glmer(y ~ period * treatment + (1 | plot),
              data    = df,
              family  = binomial,
              control = glmerControl(optimizer = "bobyqa"))
      ),
      error = function(e) NULL
    )

    if (!is.null(fit)) {
      coefs <- coef(summary(fit))
      if ("period:treatment" %in% rownames(coefs)) {
        significant[i] <- coefs["period:treatment", "Pr(>|z|)"] < alpha
      }
    }
  }

  mean(significant, na.rm = TRUE)
}
