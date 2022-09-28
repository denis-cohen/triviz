#' @title Generate continuous estimates with simulation-based, bootstrapped, or
#' Bayesian uncertainty intervals
#'
#' @param model A model object
#' @param data A data frame (by default, \code{model$model} is used)
#' @param formula The model formula (by default, \code{model$formula} is used)
#' @param group_variable Column name for the group indicator
#' @param continuous_variable Column name for the continuous variable along which
#' group-specific conditional expectations will be computed
#' @param seq_length Length of the value sequence of \code{continuous_variable}
#' for the calculation of the continuous estimates
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
#' @return Returns, as a named list, group-specific continuous estimates and all
#' pairwise differences with numerical uncertainty intervals as well as the underlying
#' draws of these quantities of interest.

#' @export

get_est_numerical_continuous <- function(model = NULL,
                                         data = model$model,
                                         formula = model$formula,
                                         group_variable,
                                         continuous_variable,
                                         seq_length = 21L,
                                         alpha = .95,
                                         twotailed = FALSE,
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


  ## Levels and value sequence
  levels <- levels(droplevels(dat[[group_variable]]))
  levels_list <- list(levels)
  names(levels_list) <- group_variable
  x_seq <- seq(min(dat[[continuous_variable]], na.rm = TRUE),
               max(dat[[continuous_variable]], na.rm = TRUE),
               length.out = seq_length)

  ## List of manipulated data frames
  x_list <- lapply(levels,
                   function (level)
                     model.matrix(
                       formula,
                       data = dat %>%
                         dplyr::mutate(!!sym(group_variable) :=
                                         factor(level, levels = levels)),
                       xlev = levels_list
                     ))

  ## Container
  ev_draws <- array(NA, dim = c(length(levels),
                                length(x_seq),
                                n_draws))

  for (l in seq_along(levels)) {
    for (x in seq_along(x_seq)) {
      x_tmp <- model.matrix(
        formula,
        data = dat %>%
          dplyr::mutate(
            !!sym(group_variable) :=
              factor(levels[l], levels = levels),!!sym(continuous_variable) :=
              x_seq[x]
          ),
        xlev = levels_list
      )
      ev_draws[l, x,] <- triviz:::predict_from_draws(draws = draws,
                                                     x = t(x_tmp),
                                                     link_function = link) %>%
        apply(1, mean)
    }
  }
  dimnames(ev_draws)[[1]] <- levels

  ## Medians
  ev_medians <- apply(ev_draws, 1:2, median)

  ## Level order
  ordered_levels <- order(ev_medians[, ceiling(length(x_seq) / 2)])
  ordered_labels <- rownames(ev_medians)[ordered_levels]

  ## Ordered expected values (draws)
  ev_draws <- ev_draws[ordered_levels, ,]

  ## Ordered expected values (summaries)
  ev <- dplyr::tibble()
  for (j in seq_along(x_seq)) {
    ev <- dplyr::bind_rows(
      ev,
      apply(ev_draws[, j,], 1, function (x) {
        c(
          "EV" = median(x),
          "SE" = sd(x),
          "p" = ifelse(twotailed,
                       2 * pnorm(abs(
                         median(x) / sd(x)
                       ), lower.tail = FALSE),
                       mean(x > 0)),
          "lower" = ifelse(
            twotailed,
            median(x) + qnorm(1 - alpha / 2) * sd(x),
            quantile(x, .025)
          ),
          "upper" = ifelse(
            twotailed,
            median(x) - qnorm(1 - alpha / 2) * sd(x),
            quantile(x, .975)
          )
        )
      }) %>%
        t() %>%
        dplyr::as_tibble(., rownames = "Group") %>%
        dplyr::mutate(!!as.name(continuous_variable) := x_seq[j])
    )
  }

  ## Pairwise contrasts (draws)
  contrasts_draws <- array(NA, dim = c(length(levels) - 1L,
                                       seq_length,
                                       n_draws,
                                       length(levels)))
  dimnames(contrasts_draws) <-
    list(NULL, x_seq, NULL, ordered_labels)

  i_num <- 0L
  for (i in ordered_labels) {
    i_num <- i_num + 1L
    not_i <- dimnames(ev_draws)[[1]][dimnames(ev_draws)[[1]] != i]
    j_num <- 0L
    for (j in not_i) {
      j_num <- j_num + 1L
      contrasts_draws[j_num, , , i_num] <-
        ev_draws[j, ,] - ev_draws[i, ,]
    }
  }

  ## Contrasts
  contrast_values <- dplyr::tibble()
  for (j in seq_along(x_seq)) {
    for (k in ordered_labels) {
      ordered_labels2 <- ordered_labels[-which(ordered_labels == k)]
      for (l in ordered_labels2) {
        contrast_values <- dplyr::bind_rows(
          contrast_values,
          apply(contrasts_draws[which(ordered_labels2 == l), j, , k] %>%
                  as.matrix(),
                2, function (x) {
                  c(
                    "FD" = median(x),
                    "SE" = sd(x),
                    "p" = ifelse(
                      twotailed,
                      2 * pnorm(abs(median(x) / sd(x)), lower.tail = FALSE),
                      mean(x > 0)
                    ),
                    "lower" = ifelse(
                      twotailed,
                      median(x) + qnorm(1 - alpha / 2) * sd(x),
                      quantile(x, .025)
                    ),
                    "upper" = ifelse(
                      twotailed,
                      median(x) - qnorm(1 - alpha / 2) * sd(x),
                      quantile(x, .975)
                    )
                  )
                }) %>%
            t() %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(
              !!as.name(continuous_variable) := x_seq[j],
              Group1 := l,
              Group2 := k
            )
        )
      }
    }
  }

  contrast_values <- contrast_values %>%
    dplyr::select(Group1,
                  Group2,
                  FD,
                  SE,
                  p ,
                  lower,
                  upper,
                  !!as.name(continuous_variable)) %>%
    dplyr::arrange(Group1,!!as.name(continuous_variable), Group2)

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
