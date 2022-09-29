#' @title Generate point estimates with simulation-based, bootstrapped, or
#' Bayesian uncertainty intervals
#'
#' @param model A model object
#' @param data A data frame (by default, \code{model$model} is used)
#' @param formula The model formula (by default, \code{model$formula} is used)
#' @param variable Column name for the group indicator
#' @param alpha Confidence level
#' @param twotailed Logical flag indicating whether to perform a two-tailed
#' hypothesis test
#' @param n_draws Number of simulation draws or bootstrap resamples
#' @param seed Seed for reproducibility
#' @param link A character that specifies the name of the link function
#' or a user-supplied link function
#' @param type A character that specifies the type of the interval:
#' \code{"simulation"}, \code{"bootstrap"}, \code{"bayesian"}. Simulation-based
#' and bootstrapped confidence intervals are calculated as part of the function;
#' Bayesian credible intervals require the provision of \code{posterior_draws}
#' along with \code{data}, \code{formula}, and \variable{variable}.
#' @param posterior_draws A matrix of posterior draws (in rows) of the model
#'  coefficients (columns). Only required and evaluated if \code{type == "bayesian"}.
#'
#' @return Returns, as a named list, group-specific point estimates and all
#' pairwise differences with numerical uncertainty intervals as well as the underlying
#' draws of these quantities of interest.

#' @export

get_est_numerical <- function(model = NULL,
                              data = model$model,
                              formula = model$formula,
                              variable,
                              twotailed = FALSE,
                              alpha = .05,
                              n_draws = 1000L,
                              seed = 20220315L,
                              link = "identity",
                              type = c("simulation", "bootstrap", "bayesian"),
                              posterior_draws = NULL) {

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
  if (type %in% c("simulation", "bootstrap")) {
    draws <- triviz:::sim_boot(mod, type, n_draws, seed)
  } else if (type == "bayesian") {
    if (is.null(posterior_draws) | !is.matrix(posterior_draws)) {
      stop("Please supply 'posterior_draws' as a matrix.")
      message(
        paste0(
          "Columns must represent the parameters matching the model ",
          "matrix produced by 'formula'. Rows must represent posterior draws."
        )
      )
    } else {
      draws <- posterior_draws
      n_draws <- nrow(posterior_draws)
    }
  }


  ## Levels
  levels <- levels(droplevels(dat[[variable]]))
  levels_list <- list(levels)
  names(levels_list) <- variable

  ## List of manipulated data frames
  x_list <- lapply(levels,
                   function (level)
                     stats::model.matrix(
                       formula,
                       data = dat %>%
                         dplyr::mutate(!!rlang::sym(variable) :=
                                         factor(level, levels = levels)),
                       xlev = levels_list
                     ))

  ## Draws of expected values
  ev_draws <- sapply(x_list,
                     function (x) {
                       apply(x,
                             1,
                             triviz:::predict_from_draws,
                             draws = draws,
                             link_function = link) %>%
                         apply(., 1, mean)
                     })
  colnames(ev_draws) <- levels

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
        mean(x <= 0)
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
        mean(x <= 0)
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
