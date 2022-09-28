#' @title sim_boot
#'
#' @description Internal function
#'
#' @noRd

sim_boot <- function(mod,
                     type = c("simulation", "bootstrap"),
                     n_draws = 1000L,
                     seed = 20220315L) {
  set.seed(seed)

  type <- match.arg(type)

  if (type == "simulation") {
    return(MASS::mvrnorm(n_draws, stats::coef(mod), stats::vcov(mod)))
  } else if (type == "bootstrap") {
    return(t(sapply(seq_len(n_draws), function (resample) {
      set.seed(seed + resample)
      mod_resample <- stats::update(mod,
                                    data = dat %>%
                                      dplyr::slice_sample(n = nrow(.), replace = TRUE))
      return(stats::coef(mod_resample))
    })))
  }
}
