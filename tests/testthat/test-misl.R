# tests/testthat/test-misl2.R
# Run with: devtools::test()

# ── Shared test fixtures ──────────────────────────────────────────────────────

make_cont_data <- function(n = 50, seed = 1) {
  set.seed(seed)
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  df[sample(n, 10), "y"] <- NA
  df
}

make_bin_data <- function(n = 50, seed = 2) {
  set.seed(seed)
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rbinom(n, 1, 0.5))
  df[sample(n, 10), "y"] <- NA
  df
}

make_cat_data <- function(n = 50, seed = 3) {
  set.seed(seed)
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y  = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  df[sample(n, 10), "y"] <- NA
  df
}

make_ord_data <- function(n = 50, seed = 6) {
  set.seed(seed)
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y  = factor(sample(c("low", "med", "high"), n, replace = TRUE),
                levels = c("low", "med", "high"), ordered = TRUE)
  )
  df[sample(n, 10), "y"] <- NA
  df
}

make_mixed_data <- function(n = 80, seed = 4) {
  set.seed(seed)
  df <- data.frame(
    cont = rnorm(n),
    bin  = rbinom(n, 1, 0.5),
    cat  = factor(sample(c("x", "y", "z"), n, replace = TRUE)),
    pred = rnorm(n)
  )
  df[sample(n, 10), "cont"] <- NA
  df[sample(n, 10), "bin"]  <- NA
  df[sample(n, 10), "cat"]  <- NA
  df
}

make_factor_bin_data <- function(n = 50, seed = 5) {
  set.seed(seed)
  df <- data.frame(
    x = rnorm(n),
    y = factor(sample(c("Yes", "No"), n, replace = TRUE))
  )
  df[sample(n, 10), "y"] <- NA
  df
}

# ── check_dataset() ───────────────────────────────────────────────────────────

test_that("check_dataset() errors on non-data-frame input", {
  expect_error(check_dataset("a string"),    "data frame or matrix")
  expect_error(check_dataset(1:10),          "data frame or matrix")
  expect_error(check_dataset(list(a = 1:5)), "data frame or matrix")
})

test_that("check_dataset() errors on empty input", {
  expect_error(check_dataset(data.frame()),              "at least one row")
  expect_error(check_dataset(data.frame(a = integer())), "at least one row")
})

test_that("check_dataset() errors on complete dataset", {
  expect_error(check_dataset(datasets::iris), "complete")
})

test_that("check_dataset() passes silently on valid incomplete data", {
  df <- make_cont_data()
  expect_invisible(check_dataset(df))
})

# ── check_datatype() ──────────────────────────────────────────────────────────

test_that("check_datatype() identifies categorical columns", {
  expect_equal(check_datatype(factor(c("a", "b", "c"))), "categorical")
})

test_that("check_datatype() identifies ordinal columns", {
  expect_equal(
    check_datatype(factor(c("low", "med", "high"), ordered = TRUE)),
    "ordinal"
  )
})

test_that("check_datatype() identifies two-level factor as binomial", {
  expect_equal(check_datatype(factor(c("Yes", "No", "Yes", NA))), "binomial")
  expect_equal(check_datatype(factor(c("A", "B"))),                "binomial")
})

test_that("check_datatype() identifies binomial columns", {
  expect_equal(check_datatype(c(0, 1, 0, 1, NA)), "binomial")
  expect_equal(check_datatype(c(0, 0, 0)),         "binomial")
})

test_that("check_datatype() identifies continuous columns", {
  expect_equal(check_datatype(c(1.5, 2.3, 3.7)), "continuous")
  expect_equal(check_datatype(c(1L, 2L, 3L)),     "continuous")
  expect_equal(check_datatype(c(0, 1, 2)),         "continuous")
})

# ── list_learners() ───────────────────────────────────────────────────────────

test_that("list_learners() returns a tibble with expected columns", {
  result <- list_learners()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("learner", "description", "package", "installed") %in%
                    colnames(result)))
})

test_that("list_learners() filters by outcome type", {
  cont <- list_learners("continuous")
  bin  <- list_learners("binomial")
  cat  <- list_learners("categorical")
  ord  <- list_learners("ordinal")

  expect_true("glm"  %in% cont$learner)
  expect_true("glm"  %in% bin$learner)
  expect_false("glm" %in% cat$learner)
  expect_false("glm" %in% ord$learner)

  expect_true("multinom_reg"  %in% cat$learner)
  expect_false("multinom_reg" %in% cont$learner)
  expect_false("multinom_reg" %in% bin$learner)
  expect_false("multinom_reg" %in% ord$learner)

  expect_true("polr"  %in% ord$learner)
  expect_false("polr" %in% cont$learner)
  expect_false("polr" %in% bin$learner)
  expect_false("polr" %in% cat$learner)

  expect_true("rand_forest" %in% ord$learner)
  expect_true("boost_tree"  %in% ord$learner)

  expect_false("continuous"  %in% colnames(cont))
  expect_false("binomial"    %in% colnames(bin))
  expect_false("categorical" %in% colnames(cat))
  expect_false("ordinal"     %in% colnames(ord))
})

test_that("list_learners() installed_only returns only installed learners", {
  result <- list_learners(installed_only = TRUE)
  expect_true(all(result$installed))
})

test_that("list_learners() errors on invalid outcome_type", {
  expect_error(list_learners("invalid"), "should be one of")
})

# ── misl() -- output structure ────────────────────────────────────────────────

test_that("misl() returns a list of length m", {
  result <- misl(make_cont_data(), m = 2, maxit = 1, con_method = "glm", seed = 1)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("misl() each element has $datasets and $trace", {
  result <- misl(make_cont_data(), m = 2, maxit = 1, con_method = "glm", seed = 1)
  for (i in seq_along(result)) {
    expect_named(result[[i]], c("datasets", "trace"))
  }
})

test_that("misl() imputed datasets have same dimensions as input", {
  df     <- make_cont_data()
  result <- misl(df, m = 2, maxit = 1, con_method = "glm", seed = 1)
  for (i in seq_along(result)) {
    expect_equal(dim(result[[i]]$datasets), dim(df))
  }
})

# ── misl() -- no remaining missingness ───────────────────────────────────────

test_that("misl() produces no NAs for continuous outcome", {
  result <- misl(make_cont_data(), m = 2, maxit = 2, con_method = "glm", seed = 1)
  expect_false(anyNA(result[[1]]$datasets))
  expect_false(anyNA(result[[2]]$datasets))
})

test_that("misl() produces no NAs for binomial outcome", {
  result <- misl(make_bin_data(), m = 2, maxit = 2, bin_method = "glm", seed = 2)
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() produces no NAs for categorical outcome", {
  result <- misl(make_cat_data(), m = 2, maxit = 2,
                 cat_method = "rand_forest", seed = 3)
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() produces no NAs for ordinal outcome", {
  skip_if_not_installed("MASS")
  result <- misl(make_ord_data(), m = 2, maxit = 2,
                 ord_method = "polr", seed = 6)
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() produces no NAs for mixed outcome types", {
  result <- misl(
    make_mixed_data(), m = 2, maxit = 2,
    con_method = "glm", bin_method = "glm", cat_method = "rand_forest",
    seed = 4
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() produces no NAs for factor-coded binary outcome", {
  result <- misl(make_factor_bin_data(), m = 2, maxit = 2,
                 bin_method = "glm", seed = 5)
  expect_false(anyNA(result[[1]]$datasets))
})

# ── misl() -- imputed values are plausible ────────────────────────────────────

test_that("misl() binary imputations are only 0 or 1", {
  df     <- make_bin_data()
  result <- misl(df, m = 2, maxit = 2, bin_method = "glm", seed = 2)
  for (i in seq_along(result)) {
    expect_true(all(result[[i]]$datasets$y %in% c(0, 1)))
  }
})

test_that("misl() categorical imputations stay within observed levels", {
  df     <- make_cat_data()
  result <- misl(df, m = 2, maxit = 2, cat_method = "rand_forest", seed = 3)
  for (i in seq_along(result)) {
    expect_true(all(result[[i]]$datasets$y %in% levels(df$y)))
  }
})

test_that("misl() ordinal imputations stay within observed levels and remain ordered", {
  skip_if_not_installed("MASS")
  df     <- make_ord_data()
  result <- misl(df, m = 2, maxit = 2, ord_method = "polr", seed = 6)
  for (i in seq_along(result)) {
    expect_true(all(result[[i]]$datasets$y %in% levels(df$y)))
    expect_true(is.ordered(result[[i]]$datasets$y))
  }
})

test_that("misl() factor-coded binary imputations stay within observed levels", {
  df     <- make_factor_bin_data()
  result <- misl(df, m = 2, maxit = 2, bin_method = "glm", seed = 5)
  for (i in seq_along(result)) {
    expect_true(all(result[[i]]$datasets$y %in% levels(df$y)))
    expect_s3_class(result[[i]]$datasets$y, "factor")
  }
})

test_that("misl() does not alter observed values", {
  df      <- make_cont_data()
  result  <- misl(df, m = 2, maxit = 2, con_method = "glm", seed = 1)
  obs_idx <- !is.na(df$y)
  for (i in seq_along(result)) {
    expect_equal(result[[i]]$datasets$y[obs_idx], df$y[obs_idx])
  }
})

# ── misl() -- trace plot ──────────────────────────────────────────────────────

test_that("misl() trace has expected columns", {
  result <- misl(make_cont_data(), m = 2, maxit = 2, con_method = "glm", seed = 1)
  expect_named(
    result[[1]]$trace,
    c("statistic", "variable", "m", "iteration", "value")
  )
})

test_that("misl() trace has mean and sd rows for continuous columns", {
  result <- misl(make_cont_data(), m = 2, maxit = 2, con_method = "glm", seed = 1)
  trace  <- result[[1]]$trace
  y_rows <- trace[trace$variable == "y", ]
  expect_true("mean" %in% y_rows$statistic)
  expect_true("sd"   %in% y_rows$statistic)
})

test_that("misl() trace is NA for factor-coded binary columns", {
  result <- misl(make_factor_bin_data(), m = 1, maxit = 2,
                 bin_method = "glm", seed = 5)
  trace  <- result[[1]]$trace
  y_rows <- trace[trace$variable == "y", ]
  expect_true(all(is.na(y_rows$value)))
})

test_that("misl() trace is NA for ordinal columns", {
  skip_if_not_installed("MASS")
  result <- misl(make_ord_data(), m = 1, maxit = 2,
                 ord_method = "polr", seed = 6)
  trace  <- result[[1]]$trace
  y_rows <- trace[trace$variable == "y", ]
  expect_true(all(is.na(y_rows$value)))
})

# ── misl() -- cv_folds parameter ─────────────────────────────────────────────

test_that("misl() runs with cv_folds = 3", {
  skip_if_not_installed("ranger")
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = c("glm", "rand_forest"),
    cv_folds = 3, seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() cv_folds is ignored for single learner", {
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = "glm", cv_folds = 3, seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() errors on invalid cv_folds", {
  df <- make_cont_data()
  expect_error(misl(df, m = 1, maxit = 1, con_method = "glm", cv_folds = 1),   "'cv_folds' must be an integer >= 2.")
  expect_error(misl(df, m = 1, maxit = 1, con_method = "glm", cv_folds = 1.5), "'cv_folds' must be an integer >= 2.")
  expect_error(misl(df, m = 1, maxit = 1, con_method = "glm", cv_folds = "3"), "'cv_folds' must be an integer >= 2.")
})

# ── misl() -- binomial bootstrap guard ───────────────────────────────────────

test_that("misl() handles imbalanced binary columns without error", {
  set.seed(99)
  n  <- 100
  df <- data.frame(
    x  = rnorm(n),
    y  = c(rep(1, 95), rep(0, 5))
  )
  df[sample(which(df$y == 1), 5), "y"] <- NA
  expect_no_error(
    misl(df, m = 2, maxit = 1, bin_method = "glm", seed = 99)
  )
})

# ── misl() -- ignore_predictors ──────────────────────────────────────────────

test_that("misl() respects ignore_predictors", {
  df <- make_cont_data()
  expect_no_error(
    misl(df, m = 1, maxit = 1, con_method = "glm",
         ignore_predictors = "x1", seed = 1)
  )
})

# ── misl() -- single vs multiple learners ────────────────────────────────────

test_that("misl() runs with a single learner (no stacking)", {
  result <- misl(make_cont_data(), m = 2, maxit = 1, con_method = "glm", seed = 1)
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() runs with multiple learners (stacking)", {
  skip_if_not_installed("ranger")
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = c("glm", "rand_forest"), seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

# ── misl() -- multinom_reg learner ───────────────────────────────────────────

test_that("misl() runs with multinom_reg for categorical outcome", {
  result <- misl(
    make_cat_data(), m = 2, maxit = 1,
    cat_method = "multinom_reg", seed = 3
  )
  expect_false(anyNA(result[[1]]$datasets))
  expect_true(all(result[[1]]$datasets$y %in% levels(make_cat_data()$y)))
})

# ── misl() -- polr learner ────────────────────────────────────────────────────

test_that("misl() runs with polr for ordinal outcome", {
  skip_if_not_installed("MASS")
  result <- misl(
    make_ord_data(), m = 2, maxit = 1,
    ord_method = "polr", seed = 6
  )
  expect_false(anyNA(result[[1]]$datasets))
  expect_true(all(result[[1]]$datasets$y %in% levels(make_ord_data()$y)))
  expect_true(is.ordered(result[[1]]$datasets$y))
})

# ── misl() -- custom parsnip specs ───────────────────────────────────────────

test_that("misl() accepts a parsnip spec for continuous outcome", {
  skip_if_not_installed("ranger")
  custom_spec <- parsnip::rand_forest(trees = 50) |>
    parsnip::set_engine("ranger")
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = list(custom_spec), seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() accepts a mixed list of strings and parsnip specs", {
  skip_if_not_installed("ranger")
  custom_spec <- parsnip::rand_forest(trees = 50) |>
    parsnip::set_engine("ranger")
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = list("glm", custom_spec),
    cv_folds = 3, seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() accepts a parsnip spec for binary outcome", {
  skip_if_not_installed("ranger")
  custom_spec <- parsnip::rand_forest(trees = 50) |>
    parsnip::set_engine("ranger")
  result <- misl(
    make_bin_data(), m = 2, maxit = 1,
    bin_method = list(custom_spec), seed = 2
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() accepts a parsnip spec for categorical outcome", {
  custom_spec <- parsnip::multinom_reg() |>
    parsnip::set_engine("nnet")
  result <- misl(
    make_cat_data(), m = 2, maxit = 1,
    cat_method = list(custom_spec), seed = 3
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() accepts a parsnip spec for ordinal outcome", {
  skip_if_not_installed("MASS")
  # polr is handled via MASS::polr() directly, passed as the named string
  result <- misl(
    make_ord_data(), m = 2, maxit = 1,
    ord_method = list("polr"), seed = 6
  )
  expect_false(anyNA(result[[1]]$datasets))
  expect_true(is.ordered(result[[1]]$datasets$y))
})

test_that("misl() enforces mode on user-supplied parsnip spec", {
  skip_if_not_installed("ranger")
  # Supply a spec with the wrong mode - misl should correct it
  wrong_mode_spec <- parsnip::rand_forest(trees = 50) |>
    parsnip::set_engine("ranger") |>
    parsnip::set_mode("classification")  # wrong for continuous
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = list(wrong_mode_spec), seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

test_that("misl() backwards compatible: character vector still works", {
  result <- misl(
    make_cont_data(), m = 2, maxit = 1,
    con_method = c("glm"), seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})

# ── plot_misl_trace() ─────────────────────────────────────────────────────────

test_that("plot_misl_trace() runs without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  result <- misl(make_cont_data(), m = 3, maxit = 3, con_method = "glm", seed = 1)
  expect_no_error(plot_misl_trace(result))
})

test_that("plot_misl_trace() returns trace data invisibly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  result <- misl(make_cont_data(), m = 3, maxit = 3, con_method = "glm", seed = 1)
  out <- plot_misl_trace(result)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("statistic", "variable", "m", "iteration", "value") %in%
                    colnames(out)))
})

test_that("plot_misl_trace() drops categorical variables silently", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  result <- misl(make_mixed_data(), m = 2, maxit = 2,
                 con_method = "glm", bin_method = "glm",
                 cat_method = "rand_forest", seed = 4)
  out <- plot_misl_trace(result)
  # cat column should not appear in trace output since it has no trace values
  expect_false("cat" %in% unique(out$variable))
})

test_that("plot_misl_trace() returns message when no trace data available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  result <- misl(make_cat_data(), m = 2, maxit = 2,
                 cat_method = "rand_forest", seed = 3)
  expect_message(plot_misl_trace(result), "No trace data available")
})

test_that("plot_misl_trace() accepts custom ncol and nrow", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggforce")
  result <- misl(make_cont_data(), m = 3, maxit = 3, con_method = "glm", seed = 1)
  expect_no_error(plot_misl_trace(result, ncol = 1, nrow = 2))
})

test_that("plot_misl_trace() errors when ggplot2 not available", {
  result <- misl(make_cont_data(), m = 2, maxit = 2, con_method = "glm", seed = 1)
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "ggplot2") FALSE else TRUE,
    .package = "base"
  )
  expect_error(plot_misl_trace(result), "ggplot2")
})

test_that("plot_misl_trace() errors when ggforce not available", {
  result <- misl(make_cont_data(), m = 2, maxit = 2, con_method = "glm", seed = 1)
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "ggforce") FALSE else TRUE,
    .package = "base"
  )
  expect_error(plot_misl_trace(result), "ggforce")
})

test_that("misl() errors with a helpful message for unknown learner", {
  suppressWarnings(
    expect_error(
      misl(make_cont_data(), m = 1, maxit = 1, con_method = "lasso"),
      "Unknown learner"
    )
  )
})

test_that("misl() errors with install hint for missing backend package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "xgboost") FALSE else TRUE,
    .package = "base"
  )
  suppressWarnings(
    expect_error(
      misl(make_cont_data(), m = 1, maxit = 1, con_method = "boost_tree"),
      "install.packages"
    )
  )
})

test_that("misl() errors with helpful message for invalid learner type", {
  suppressWarnings(
    expect_error(
      misl(make_cont_data(), m = 1, maxit = 1, con_method = list(123)),
      "named character string or a parsnip model spec"
    )
  )
})

test_that("misl() completes successfully with a valid user-supplied parsnip spec", {
  skip_if_not_installed("ranger")
  valid_spec <- parsnip::rand_forest(trees = 10) |>
    parsnip::set_engine("ranger")
  result <- misl(
    make_cont_data(), m = 1, maxit = 1,
    con_method = list(valid_spec), seed = 1
  )
  expect_false(anyNA(result[[1]]$datasets))
})
