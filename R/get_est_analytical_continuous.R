#' @title Generate continuous estimates with analytical confidence intervals from a model object
#'
#' @param model A \code{lm} or \code{glm} model object
#' @param data A data frame (by default, \code{model$model} is used)
#' @param formula The model formula (by default, \code{model$formula} is used)
#' @param group_variable Column name for the group indicator
#' @param continuous_variable Column name for the continuous variable along which
#' group-specific conditional expectations will be computed
#' @param seq_length Length of the value sequence of \code{continuous_variable}
#' for the calculation of the continuous estimates
#' @param alpha Confidence level
#'
#' @return Returns group-specific continuous estimates and all pairwise
#' differences with analytical 95% confidence intervals.
#'
#' @export

get_est_analytical_continuous <- function(model,
                                          data = model$model,
                                          formula = model$formula,
                                          group_variable,
                                          continuous_variable,
                                          seq_length = 21L,
                                          alpha = 0.05,
                                          twotailed = TRUE) {
  ## Data list
  data_list <- list(levels(dat[[group_variable]]),
                    seq(min(dat[[continuous_variable]], na.rm = TRUE),
                        max(dat[[continuous_variable]], na.rm = TRUE),
                        length.out = seq_length))
  names(data_list) <- c(group_variable,
                        continuous_variable)

  ## Expected values
  ev <- prediction::prediction(mod,
                               at = data_list,
                               vce = "delta",
                               level = 1 - alpha * (1 + as.numeric(!twotailed))) %>%
    summary() %>%
    dplyr::rename(
      Group = paste0("at(", group_variable, ")"),
      !!as.name(continuous_variable) := paste0("at(", continuous_variable, ")")
    ) %>%
    dplyr::rename(EV = Prediction) %>%
    dplyr::select(-z)


  ## Level order
  mean_pred <- sapply(ev %>%
                        split(.$Group), function(element)
                          mean(element$EV))
  ordered_labels <- names(mean_pred)[order(mean_pred)]

  ## Pairwise contrasts
  contrasts <- lapply(seq_along(ordered_labels), function (j) {
    margins::margins(
      stats::update(mod,
                    data = dat %>%
                      mutate(
                        !!sym(group_variable) :=
                          relevel(!!sym(group_variable),
                                  ref = ordered_labels[j])
                      )),
      variable = group_variable,
      at = data_list[continuous_variable],
      vce = "delta",
      level = 1 - alpha * (1 + as.numeric(!twotailed))
    ) %>%
      base::summary() %>%
      dplyr::slice(order(AME)) %>%
      dplyr::rename(FD = AME,
                    Group1 = factor) %>%
      dplyr::mutate(Group2 = ordered_labels[j],
                    Group1 = gsub(group_variable, "", Group1)) %>%
      dplyr::select(Group1, Group2, FD, SE, p, lower, upper, !!as.name(continuous_variable))
  }) %>%
    do.call(rbind, .) %>%
    dplyr::arrange(Group1, !!as.name(continuous_variable), Group2)

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
