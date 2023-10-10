## ---- Packages ----
pacman::p_load(
  here,
  dplyr,
  margins,
  prediction,
  MASS
)

library(confidenceR)

## ---- Data ----
load("dat/gles.RData")
dat <- gles.imp[[1]] %>%
  dplyr::select(pid, east, age, fem, unemp00, hinc_eq, imm_self, -year) %>%
  dplyr::filter(pid %in% c("None", "Other", "Left"))
  dplyr::add_row(pid = as.factor("Test"), unemp00 = .5, east = as.factor(0), age = 27, fem = as.factor(1), imm_self = 4.1) %>%
  dplyr::add_row(pid = as.factor("TEst1"), unemp00 = .2, east = as.factor(0), age = 27, fem = as.factor(1), imm_self = 6.1)

dat$pid <- factor(dat$pid)

## ---- Logit model ----
mod <- glm(hinc_eq ~
             pid +
             east +
             age +
             fem,
           data = dat,
           family = "gaussian")

## ---- Analytical example ----
analytical_est <- triviz::get_est_analytical(
  model = mod,
  data = mod$model,
  formula = mod$formula,
  variable = "pid"
)

plot_point_estimates(estimates = list("expected_values" = analytical_est$expected_values,
                                      "contrasts" = analytical_est$contrasts),
                     p_val_threshold = 0.05,
                     interactive = FALSE,
                     type = "analytical",
                     show_lower_triangular_title = TRUE
                     )

pacman::p_load(
  here,
  dplyr,
  margins,
  prediction,
  MASS
)

## ---- Data ----
load("dat/gles.RData")
dat <- gles.imp[[1]] %>%
  dplyr::select(-year)

## ---- Logit model ----
mod <- glm(I(vote2017 == "CDU/CSU") ~
             log_hinc_eq * myclass4_r +
             age +
             fem,
           data = dat,
           family = "binomial")

# ## ---- Analytical example ----
analytical_est <- triviz::get_est_analytical_continuous(
  model = mod,
  data = mod$model,
  formula = mod$formula,
  group_variable = "myclass4_r",
  continuous_variable = "log_hinc_eq"
)

triviz::plot_continuous_estimates(estimates = list("expected_values" = analytical_est$expected_values,
                                                     "contrasts" = analytical_est$contrasts),
                            variable = "log_hinc_eq",
                            type = "analytical",
                            variable_label = "log(hin_eq)",
                            one_tailed_test = FALSE,
                            p_val_threshold = 0.1,
                            interactive = FALSE)
