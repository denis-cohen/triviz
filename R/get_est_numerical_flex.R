#' @title Generate point estimates with simulation-based, bootstrapped, or
#' Bayesian uncertainty intervals
#'
#' @param draws A matrix of numerical draws (in rows) of the quantities of interest
#'  (columns).
#' @param variable Column name for the group indicator
#' @param alpha Confidence level
#' @param twotailed Logical flag indicating whether to perform a two-tailed
#' hypothesis test
#' @param type A character that specifies the type of the interval:
#' \code{"simulation"}, \code{"bootstrap"}, \code{"bayesian"}.
#'
#' @return Returns, as a named list, group-specific point estimates and all
#' pairwise differences with numerical uncertainty intervals as well as the underlying
#' draws of these quantities of interest.

#' @export

get_est_numerical_flex <- function(draws,
                                   variable,
                                   twotailed = FALSE,
                                   alpha = .05,
                                   type = c("simulation", "bootstrap", "bayesian")) {


  ## Warnings
  if (twotailed) {
    warning(
      paste0(
        "Two-tailed hypothesis tests rest on the assumption of normal ",
        "sampling/posterior distributions. This assumption may not be ",
        "reasonable. Please consider using one-tailed/directional tests ",
        "instead by setting 'twotailed = FALSE'."
      )
    )
  }

  ## Parameter draws
  if (is.null(draws) | !is.matrix(draws) | is.null(colnames(draws))) {
    stop("Please supply 'draws' as a matrix. Group names should be supplied as column names.")
  } else {
    n_draws <- nrow(draws)
  }

  ## Medians
  ev_medians <- apply(ev_draws, 2, stats::median)

  ## Level order
  ordered_levels <- order(ev_medians)
  ordered_labels <- names(ev_medians)[ordered_levels]

  ## Ordered expected values (draws)
  ev_draws <- ev_draws[, ordered_levels]

  ## Ordered expected values (summaries)
  ev <- apply(ev_draws, 2, function (x) {
    c(
      "EV" = stats::median(x),
      "SE" = stats::sd(x),
      "p" = ifelse(
        twotailed,
        2 * stats::pnorm(abs(stats::median(x) / stats::sd(x)), lower.tail = FALSE),
        ifelse(type == "bayesian", mean(x >= 0), mean(x <= 0))
      ),
      "lower" = ifelse(
        twotailed,
        stats::median(x) + stats::qnorm(alpha / 2) * stats::sd(x),
        stats::quantile(x, alpha / 2)
      ),
      "upper" = ifelse(
        twotailed,
        stats::median(x) + stats::qnorm(1 - alpha / 2) * stats::sd(x),
        stats::quantile(x, (1 - alpha / 2))
      )
    )
  }) %>%
    t() %>%
    dplyr::as_tibble(., rownames = "Group")

  ## Pairwise contrasts (draws)
  contrasts_draws <- array(NA, dim = c(ncol(ev_draws) - 1L,
                                       n_draws,
                                       ncol(ev_draws)))
  dimnames(contrasts_draws) <- list(NULL, NULL, ordered_labels)

  i_num <- 0L
  for (i in ordered_labels) {
    i_num <- i_num + 1L
    not_i <- colnames(ev_draws)[colnames(ev_draws) != i]
    j_num <- 0L
    for (j in not_i) {
      j_num <- j_num + 1L
      contrasts_draws[j_num, , i_num] <-
        ev_draws[, j] - ev_draws[, i]
    }
  }

  contrasts <- apply(contrasts_draws, c(1, 3), function(x) {
    c(
      "FD" = stats::median(x),
      "SE" = stats::sd(x),
      "p" = ifelse(
        twotailed,
        2 * stats::pnorm(abs(stats::median(x) / stats::sd(x)), lower.tail = FALSE),
        ifelse(type == "bayesian", mean(x >= 0), mean(x <= 0))
      ),
      "lower" = ifelse(
        twotailed,
        stats::median(x) + stats::qnorm(alpha / 2) * stats::sd(x),
        stats::quantile(x, alpha / 2)
      ),
      "upper" = ifelse(
        twotailed,
        stats::median(x) + stats::qnorm((1 - alpha) / 2) * stats::sd(x),
        stats::quantile(x, 1 - alpha / 2)
      )
    )
  }) %>%
    aperm(c(2, 1, 3))

  ## Return value
  output <- list(
    "expected_values" = ev,
    "contrasts" = contrasts,
    "expected_values_draws" = ev_draws,
    "contrasts_draws" = contrasts_draws,
    "type" = type,
    "alpha" = alpha,
    "twotailed" = twotailed
  )
  class(output) <- "triviz_estimates"
  return(output)
}
