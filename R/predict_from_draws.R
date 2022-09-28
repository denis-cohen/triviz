#' @title predict_from_draws
#'
#' @description Internal function
#'
#' @noRd

predict_from_draws <- function(draws, x, link_function) {
  if (is.null(link_function)) {
    nu <- draws %*% x
  } else {
    link_function <- match.fun(link_function)
    nu <- link_function(draws %*% x)
  }
}
