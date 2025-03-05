#' @title Generate point estimates with analytical confidence intervals from a model object
#'
#' @param model A model object
#' @param data A data frame (by default, \code{model$model} is used)
#' @param formula The model formula (by default, \code{model$formula} is used)
#' @param variable Column name for the group indicator
#' @param alpha Confidence level
#' @param twotailed Logical flag indicating whether to perform a two-tailed
#' hypothesis test
#'
#' @return Returns, as a named list, group-specific point estimates and all
#' pairwise differences with analytical confidence intervals.
#'
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom stats model.matrix
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @export

get_est_analytical <- function(model,
                               data = model$model,
                               formula = model$formula,
                               variable,
                               alpha = 0.05,
                               twotailed = TRUE) {
  ## Data list
  data_list <- list(levels(dat[[variable]]))
  names(data_list) <- variable

  ## Expected values
  ev <- prediction::prediction(
    mod,
    at = data_list,
    vce = "delta",
    level = 1 - alpha * (1 + as.numeric(!twotailed))
  ) %>%
    summary()

  ## Level order
  ordered_levels <- order(ev$Prediction)
  ordered_labels <- as.character(ev[[1]][order(ev$Prediction)])

  ## Expected values
  ev <- ev %>%
    dplyr::slice(ordered_levels) %>%
    dplyr::rename(EV = Prediction) %>%
    dplyr::rename_at(.vars = dplyr::vars(dplyr::starts_with("at(")),
                     ~ "Group") %>%
    dplyr::select(-z)

  ## Pairwise contrasts
  contrasts <- lapply(seq_along(ordered_labels), function (j) {
    margins::margins(
      stats::update(
        mod,
        data = dat %>%
          dplyr::mutate(
            !!rlang::sym(variable) :=
              stats::relevel(!!rlang::sym(variable),
                             ref = ordered_labels[j])
          )
      ),
      variable = variable,
      vce = "delta",
      level = 1 - alpha * (1 + as.numeric(!twotailed))
    ) %>%
      base::summary() %>%
      dplyr::slice(order(AME)) %>%
      dplyr::select(AME, SE, p, lower, upper) %>%
      dplyr::rename(FD = AME) %>%
      as.matrix()
  }) %>%
    base::simplify2array()
  dimnames(contrasts) <- list(NULL,
                              unlist(dimnames(contrasts)[2]),
                              ordered_labels)

  ## Return value
  output <- list(
    "expected_values" = ev,
    "contrasts" = contrasts,
    "type" = "analytical",
    "alpha" = alpha,
    "twotailed" = twotailed
  )
  class(output) <- "triviz_estimates"
  return(output)
}
