## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = TRUE,
  warning   = FALSE,
  message   = FALSE
)
library(misl)

## ----install, eval = FALSE----------------------------------------------------
# # Install from GitHub
# remotes::install_github("JustinManjourides/misl")
# 
# # Optional backend packages for additional learners
# install.packages(c("ranger", "xgboost", "earth", "MASS"))

## ----simulate-----------------------------------------------------------------
library(misl)

set.seed(42)
n <- 300

sim_data <- data.frame(
  # Continuous predictors (always observed)
  age    = round(rnorm(n, mean = 50, sd = 12)),
  bmi    = round(rnorm(n, mean = 26, sd = 4), 1),
  # Continuous outcome with missingness
  sbp    = round(120 + 0.4 * rnorm(n, mean = 50, sd = 12) +
                       0.3 * rnorm(n, mean = 26, sd = 4) + rnorm(n, sd = 10)),
  # Binary outcome with missingness (0 = no, 1 = yes)
  smoker = rbinom(n, 1, prob = 0.3),
  # Unordered categorical outcome with missingness
  group  = factor(sample(c("A", "B", "C"), n, replace = TRUE,
                         prob = c(0.4, 0.35, 0.25))),
  # Ordered categorical outcome with missingness
  health = factor(sample(c("Poor", "Fair", "Good", "Excellent"), n,
                         replace = TRUE, prob = c(0.1, 0.2, 0.5, 0.2)),
                  levels  = c("Poor", "Fair", "Good", "Excellent"),
                  ordered = TRUE)
)

# Introduce missing values
sim_data[sample(n, 40), "sbp"]    <- NA
sim_data[sample(n, 30), "smoker"] <- NA
sim_data[sample(n, 30), "group"]  <- NA
sim_data[sample(n, 30), "health"] <- NA

# Summarise missingness
sapply(sim_data, function(x) sum(is.na(x)))

## ----list-learners------------------------------------------------------------
knitr::kable(list_learners())

## ----list-learners-ordinal----------------------------------------------------
knitr::kable(list_learners("ordinal"))

## ----basic-usage, eval = FALSE------------------------------------------------
# misl_imp <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = "glm",
#   bin_method = "glm",
#   cat_method = "multinom_reg",
#   ord_method = "polr",
#   seed       = 42,
#   quiet      = TRUE
# )

## ----inspect-output, eval = FALSE---------------------------------------------
# # Number of imputed datasets
# length(misl_imp)
# 
# # Confirm no missing values remain
# anyNA(misl_imp[[1]]$datasets)
# 
# # Confirm ordered factor is preserved
# is.ordered(misl_imp[[1]]$datasets$health)
# levels(misl_imp[[1]]$datasets$health)

## ----custom-spec, eval = FALSE------------------------------------------------
# library(parsnip)
# 
# # A random forest with custom hyperparameters
# custom_rf <- rand_forest(trees = 500, mtry = 3) |>
#   set_engine("ranger")
# 
# misl_custom <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = list(custom_rf),
#   bin_method = list(custom_rf),
#   cat_method = "multinom_reg",
#   ord_method = "polr",
#   seed       = 42,
#   quiet      = TRUE
# )

## ----mixed-learners, eval = FALSE---------------------------------------------
# library(parsnip)
# 
# # Mix a named learner with a custom tuned spec
# custom_xgb <- boost_tree(trees = 200, learn_rate = 0.05) |>
#   set_engine("xgboost")
# 
# misl_mixed <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = list("glm", custom_xgb),
#   bin_method = list("glm", custom_xgb),
#   cat_method = list("multinom_reg", "rand_forest"),
#   ord_method = list("polr", "rand_forest"),
#   cv_folds   = 3,
#   seed       = 42
# )

## ----svm-learner, eval = FALSE------------------------------------------------
# library(parsnip)
# 
# # SVM - not in the built-in registry but works via parsnip
# svm_spec <- svm_rbf() |>
#   set_engine("kernlab")
# 
# misl_svm <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = list("glm", svm_spec),
#   bin_method = list("glm", svm_spec),
#   cat_method = "multinom_reg",
#   ord_method = "polr",
#   cv_folds   = 3,
#   seed       = 42
# )

## ----ordinal-example, eval = FALSE--------------------------------------------
# # Ensure your ordered variable is an ordered factor
# sim_data$health <- factor(sim_data$health,
#   levels  = c("Poor", "Fair", "Good", "Excellent"),
#   ordered = TRUE
# )
# 
# misl_ordinal <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = "glm",
#   bin_method = "glm",
#   cat_method = "multinom_reg",
#   ord_method = "polr",   # proportional odds model for ordered categories
#   seed       = 42,
#   quiet      = TRUE
# )
# 
# # Imputed values respect the ordering
# is.ordered(misl_ordinal[[1]]$datasets$health)
# levels(misl_ordinal[[1]]$datasets$health)

## ----multi-learner, eval = FALSE----------------------------------------------
# misl_stack <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = c("glm", "rand_forest"),
#   bin_method = c("glm", "rand_forest"),
#   cat_method = c("multinom_reg", "rand_forest"),
#   ord_method = c("polr", "rand_forest"),
#   cv_folds   = 3,
#   seed       = 42
# )

## ----pool-results, eval = FALSE-----------------------------------------------
# # Fit a linear model to each imputed dataset
# models <- lapply(misl_imp, function(imp) {
#   lm(sbp ~ age + bmi + smoker + group + health, data = imp$datasets)
# })
# 
# # Pool point estimates and standard errors using Rubin's rules
# m       <- length(models)
# ests    <- sapply(models, function(fit) coef(fit))
# vars    <- sapply(models, function(fit) diag(vcov(fit)))
# 
# Q_bar   <- rowMeans(ests)                          # pooled estimate
# U_bar   <- rowMeans(vars)                          # within-imputation variance
# B       <- apply(ests, 1, var)                     # between-imputation variance
# T_total <- U_bar + (1 + 1 / m) * B                # total variance
# 
# pooled <- data.frame(
#   term      = names(Q_bar),
#   estimate  = round(Q_bar, 4),
#   std.error = round(sqrt(T_total), 4),
#   conf.low  = round(Q_bar - 1.96 * sqrt(T_total), 4),
#   conf.high = round(Q_bar + 1.96 * sqrt(T_total), 4)
# )
# print(pooled)

## ----pool-mice, eval = FALSE--------------------------------------------------
# library(mice)
# pooled_mice <- summary(pool(models), conf.int = TRUE)

## ----trace-plot, eval = FALSE-------------------------------------------------
# # Plot mean of imputed sbp values across iterations for each dataset
# plot_misl_trace(misl_imp, variable = "sbp", ylab = "Mean imputed sbp (mm Hg)")

## ----trace-plot-sd, eval = FALSE----------------------------------------------
# # Plot the standard deviation instead
# plot_misl_trace(misl_imp, variable = "sbp", statistic = "sd")

## ----parallel, eval = FALSE---------------------------------------------------
# library(future)
# 
# # Use all available cores
# plan(multisession)
# 
# misl_parallel <- misl(
#   sim_data,
#   m          = 10,
#   maxit      = 5,
#   con_method = c("glm", "rand_forest"),
#   bin_method = c("glm", "rand_forest"),
#   cat_method = c("multinom_reg", "rand_forest"),
#   ord_method = c("polr", "rand_forest"),
#   seed       = 42
# )
# 
# # Always reset the plan when done
# plan(sequential)

## ----parallel-limited, eval = FALSE-------------------------------------------
# plan(multisession, workers = 4)

## ----detect-cores, eval = FALSE-----------------------------------------------
# parallel::detectCores()

## ----session-info-------------------------------------------------------------
sessionInfo()

