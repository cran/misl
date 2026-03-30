## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = TRUE,
  warning   = FALSE,
  message   = FALSE
)

## ----install, eval = FALSE----------------------------------------------------
# # Install from GitHub
# remotes::install_github("JustinManjourides/misl")
# 
# # Optional backend packages for additional learners
# install.packages(c("ranger", "xgboost", "earth"))

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
  # Categorical outcome with missingness
  group  = factor(sample(c("A", "B", "C"), n, replace = TRUE,
                         prob = c(0.4, 0.35, 0.25)))
)

# Introduce missing values
sim_data[sample(n, 40), "sbp"]    <- NA
sim_data[sample(n, 30), "smoker"] <- NA
sim_data[sample(n, 30), "group"]  <- NA

# Summarise missingness
sapply(sim_data, function(x) sum(is.na(x)))

## ----list-learners------------------------------------------------------------
knitr::kable(list_learners())

## ----basic-usage--------------------------------------------------------------
misl_imp <- misl(
  sim_data,
  m          = 5,
  maxit      = 5,
  con_method = "glm",
  bin_method = "glm",
  cat_method = "multinom_reg",
  seed       = 42,
  quiet      = TRUE
)

## ----inspect-output-----------------------------------------------------------
# Number of imputed datasets
length(misl_imp)

# Confirm no missing values remain
anyNA(misl_imp[[1]]$datasets)

# Compare observed vs imputed distribution for sbp
obs_mean <- mean(sim_data$sbp, na.rm = TRUE)
imp_mean <- mean(misl_imp[[1]]$datasets$sbp)

cat("Observed mean sbp (non-missing):", round(obs_mean, 2), "\n")
cat("Imputed mean sbp  (all values): ", round(imp_mean, 2), "\n")

# Check binary imputations are valid
table(misl_imp[[1]]$datasets$smoker)

# Check categorical imputations stay within observed levels
levels(misl_imp[[1]]$datasets$group)

## ----multi-learner, eval = FALSE----------------------------------------------
# misl_stack <- misl(
#   sim_data,
#   m          = 5,
#   maxit      = 5,
#   con_method = c("glm", "rand_forest"),
#   bin_method = c("glm", "rand_forest"),
#   cat_method = c("multinom_reg", "rand_forest"),
#   cv_folds   = 3,
#   seed       = 42
# )

## ----pool-results-------------------------------------------------------------
# Fit a linear model to each imputed dataset
models <- lapply(misl_imp, function(imp) {
  lm(sbp ~ age + bmi + smoker + group, data = imp$datasets)
})

# Pool point estimates and standard errors using Rubin's rules
m       <- length(models)
ests    <- sapply(models, function(fit) coef(fit))
vars    <- sapply(models, function(fit) diag(vcov(fit)))

Q_bar   <- rowMeans(ests)                          # pooled estimate
U_bar   <- rowMeans(vars)                          # within-imputation variance
B       <- apply(ests, 1, var)                     # between-imputation variance
T_total <- U_bar + (1 + 1 / m) * B                # total variance

pooled <- data.frame(
  term     = names(Q_bar),
  estimate = round(Q_bar, 4),
  std.error = round(sqrt(T_total), 4),
  conf.low  = round(Q_bar - 1.96 * sqrt(T_total), 4),
  conf.high = round(Q_bar + 1.96 * sqrt(T_total), 4)
)
print(pooled)

## ----pool-mice, eval = FALSE--------------------------------------------------
# library(mice)
# pooled_mice <- summary(pool(models), conf.int = TRUE)

## ----trace-plot---------------------------------------------------------------
# Extract trace data across all m datasets
trace <- do.call(rbind, lapply(misl_imp, function(r) r$trace))

# Plot mean of imputed sbp values across iterations for each dataset
sbp_trace <- subset(trace, variable == "sbp" & statistic == "mean")

plot(
  sbp_trace$iteration,
  sbp_trace$value,
  col  = sbp_trace$m,
  pch  = 16,
  xlab = "Iteration",
  ylab = "Mean imputed sbp",
  main = "Trace plot: mean of imputed sbp values",
  xaxt = "n"
)
axis(1, at = 1:5)
legend("topright", legend = paste("m =", 1:5),
       col = 1:5, pch = 16, cex = 0.8)

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

