# misl.R
# Multiple Imputation by Super Learning
# Refactored to use tidymodels + stacks (replacing sl3 / delayed)
#
# Dependencies:
#   tidymodels, stacks, future.apply, ranger, xgboost, earth

#' @importFrom stats predict runif sd as.formula
NULL

# ---------------------------------------------------------------------------- #
# Public API
# ---------------------------------------------------------------------------- #

#' MISL: Multiple Imputation by Super Learning
#'
#' Imputes missing values using multiple imputation by super learning.
#'
#' @param dataset A dataframe or matrix containing the incomplete data.
#'   Missing values are represented with \code{NA}.
#' @param m The number of multiply imputed datasets to create. Default \code{5}.
#' @param maxit The number of iterations per imputed dataset. Default \code{5}.
#' @param seed Integer seed for reproducibility, or \code{NA} to skip. Default \code{NA}.
#' @param con_method Character vector of learner IDs for continuous columns.
#'   Default \code{c("glm", "rand_forest", "boost_tree")}.
#' @param bin_method Character vector of learner IDs for binary columns
#'   (values must be \code{0/1/NA}). Default \code{c("glm", "rand_forest", "boost_tree")}.
#' @param cat_method Character vector of learner IDs for categorical columns.
#'   Default \code{c("rand_forest", "boost_tree")}.
#' @param cv_folds Integer number of cross-validation folds used when stacking
#'   multiple learners. Reducing this (e.g. to \code{3}) speeds up computation
#'   at a small cost to ensemble accuracy. Default \code{5}. Ignored when only
#'   a single learner is supplied.
#' @param ignore_predictors Character vector of column names to exclude as
#'   predictors. Default \code{NA}.
#' @param quiet Suppress console progress messages. Default \code{TRUE}.
#'
#' @details
#' Supported \code{*_method} values and their required packages:
#' \itemize{
#'   \item \code{"glm"}          - base R (logistic for binary/categorical, linear for continuous)
#'   \item \code{"rand_forest"}  - \pkg{ranger}
#'   \item \code{"boost_tree"}   - \pkg{xgboost}
#'   \item \code{"mars"}         - \pkg{earth}
#'   \item \code{"multinom_reg"} - \pkg{nnet} (categorical only)
#' }
#' Use \code{\link{list_learners}()} to explore available options.
#'
#' @section Parallelism:
#' Imputation across the \code{m} datasets is parallelised via
#' \pkg{future.apply}. To enable parallel execution, set a \pkg{future} plan
#' before calling \code{misl()}:
#' \preformatted{
#' library(future)
#' plan(multisession, workers = 4)
#' result <- misl(data, m = 5)
#' plan(sequential)
#' }
#' The inner cross-validation fits (used for stacking) run sequentially within
#' each worker to avoid over-subscribing cores.
#'
#' @return A list of \code{m} named lists, each with:
#'   \describe{
#'     \item{\code{datasets}}{A fully imputed tibble.}
#'     \item{\code{trace}}{A long-format tibble of mean/sd trace statistics per
#'       iteration, for convergence inspection.}
#'   }
#' @export
#'
#' @examples
#' # Small self-contained example
#' set.seed(1)
#' n <- 100
#' demo_data <- data.frame(
#'   x1 = rnorm(n),
#'   x2 = rnorm(n),
#'   y  = rnorm(n)
#' )
#' demo_data[sample(n, 10), "y"] <- NA
#'
#' misl_imp <- misl(demo_data, m = 2, maxit = 2, con_method = "glm")
misl <- function(dataset,
                 m                 = 5,
                 maxit             = 5,
                 seed              = NA,
                 con_method        = c("glm", "rand_forest", "boost_tree"),
                 bin_method        = c("glm", "rand_forest", "boost_tree"),
                 cat_method        = c("rand_forest", "boost_tree"),
                 cv_folds          = 5,
                 ignore_predictors = NA,
                 quiet             = TRUE) {

  # --- 0. Validity checks ---
  check_dataset(dataset)
  if (!is.numeric(cv_folds) || cv_folds < 2 || cv_folds != as.integer(cv_folds)) {
    stop("'cv_folds' must be an integer >= 2.")
  }
  dataset <- tibble::as_tibble(dataset)

  if (!is.na(seed)) set.seed(seed)

  # --- 1. Parallel-safe imputation over m datasets ---
  future.apply::future_lapply(
    seq_len(m),
    future.stdout = NA,
    future.seed   = TRUE,
    FUN = function(m_loop) {

      if (!quiet) message("Imputing dataset: ", m_loop)

      # Columns that need imputation (random visit order per van Buuren)
      column_order <- sample(colnames(dataset)[colSums(is.na(dataset)) != 0])

      # Trace-plot scaffold
      trace_plot <- tidyr::expand_grid(
        statistic = c("mean", "sd"),
        variable  = colnames(dataset),
        m         = m_loop,
        iteration = seq_len(maxit),
        value     = NA_real_
      )

      # Step 2 of FCS: initialise with random draws from observed values
      data_cur <- dataset
      for (col in colnames(data_cur)) {
        missing_idx <- is.na(data_cur[[col]])
        if (any(missing_idx)) {
          data_cur[[col]][missing_idx] <- sample(
            dataset[[col]][!is.na(dataset[[col]])],
            size    = sum(missing_idx),
            replace = TRUE
          )
        }
      }

      # --- 2. Gibbs iterations ---
      for (i_loop in seq_len(maxit)) {

        if (!quiet) message("  Iteration: ", i_loop)

        for (col in column_order) {

          if (!quiet) message("    Imputing: ", col)

          outcome_type <- check_datatype(dataset[[col]])
          obs_idx      <- !is.na(dataset[[col]])
          miss_idx     <-  is.na(dataset[[col]])

          xvars <- setdiff(colnames(data_cur), col)
          if (!is.na(ignore_predictors[1])) {
            xvars <- setdiff(xvars, ignore_predictors)
          }

          full_df <- data_cur[obs_idx, c(xvars, col), drop = FALSE]

          # For binomial columns, ensure the bootstrap sample contains both
          # classes - a single-class bootstrap would drop a factor level and
          # cause predict() to fail when looking up .pred_1.
          if (outcome_type == "binomial") {
            boot_df  <- full_df
            attempts <- 0L
            repeat {
              candidate <- dplyr::slice_sample(full_df, n = nrow(full_df), replace = TRUE)
              if (length(unique(candidate[[col]])) > 1L) {
                boot_df <- candidate
                break
              }
              attempts <- attempts + 1L
              if (attempts >= 10L) {
                warning("Could not obtain a two-class bootstrap sample for '", col,
                        "' after 10 attempts; using the observed data directly.")
                break
              }
            }
          } else {
            boot_df <- dplyr::slice_sample(full_df, n = nrow(full_df), replace = TRUE)
          }

          learner_names <- switch(outcome_type,
                                  continuous  = con_method,
                                  binomial    = bin_method,
                                  categorical = cat_method
          )

          # --- 3. Fit stacked super learner ---
          sl_fit <- .fit_super_learner(
            train_data    = boot_df,
            full_data     = full_df,
            xvars         = xvars,
            yvar          = col,
            outcome_type  = outcome_type,
            learner_names = learner_names,
            cv_folds      = cv_folds        # <-- now passed through
          )

          # --- 4. Impute ---
          pred_data <- data_cur[, xvars, drop = FALSE]

          if (outcome_type == "binomial") {
            preds        <- predict(sl_fit$boot, new_data = pred_data, type = "prob")[[".pred_1"]]
            imputed_vals <- as.integer(stats::runif(length(preds)) <= preds)
            data_cur[[col]] <- ifelse(miss_idx, imputed_vals, dataset[[col]])

          } else if (outcome_type == "continuous") {
            preds_boot      <- predict(sl_fit$boot, new_data = pred_data)[[".pred"]]
            preds_full      <- predict(sl_fit$full, new_data = pred_data)[[".pred"]]
            observed_preds  <- preds_full[obs_idx]
            observed_values <- dataset[[col]][obs_idx]

            data_cur[[col]][miss_idx] <- vapply(
              preds_boot[miss_idx],
              function(yhat) {
                donors <- utils::head(order(abs(yhat - observed_preds)), 5)
                observed_values[sample(donors, 1)]
              },
              numeric(1)
            )

          } else if (outcome_type == "categorical") {
            prob_mat     <- as.matrix(predict(sl_fit$boot, new_data = pred_data, type = "prob"))
            lvls         <- levels(dataset[[col]])
            u            <- stats::runif(nrow(prob_mat))
            cum_mat      <- t(apply(prob_mat, 1, cumsum))
            idx          <- pmin(1 + rowSums(u > cum_mat), length(lvls))
            imputed_vals <- lvls[idx]

            data_cur[[col]] <- factor(
              ifelse(miss_idx, imputed_vals, as.character(dataset[[col]])),
              levels = lvls
            )
          }

          # --- 5. Trace statistics ---
          if (outcome_type != "categorical" && any(miss_idx)) {
            imp_vals <- data_cur[[col]][miss_idx]
            rows <- trace_plot$variable == col &
              trace_plot$m         == m_loop &
              trace_plot$iteration == i_loop

            trace_plot$value[rows & trace_plot$statistic == "mean"] <- mean(imp_vals)
            trace_plot$value[rows & trace_plot$statistic == "sd"]   <- stats::sd(imp_vals)
          }

        } # end column loop
      } # end iteration loop

      list(datasets = data_cur, trace = trace_plot)
    }
  )
}


#' List available learners for MISL imputation
#'
#' Displays the learners available for use in \code{\link{misl}()}, optionally
#' filtered by outcome type and/or whether the required backend package is
#' installed.
#'
#' @param outcome_type One of \code{"continuous"}, \code{"binomial"},
#'   \code{"categorical"}, or \code{"all"} (default).
#' @param installed_only If \code{TRUE}, only learners whose backend package is
#'   already installed are returned. Default \code{FALSE}.
#'
#' @return A tibble with columns \code{learner}, \code{description},
#'   \code{package}, \code{installed}, and outcome-type support flags
#'   (when \code{outcome_type = "all"}).
#' @export
#'
#' @examples
#' list_learners()
#' list_learners("continuous")
#' list_learners("categorical", installed_only = TRUE)
list_learners <- function(outcome_type = "all", installed_only = FALSE) {

  outcome_type <- match.arg(outcome_type, c("all", "continuous", "binomial", "categorical"))

  registry <- tibble::tribble(
    ~learner,        ~description,                                  ~continuous, ~binomial, ~categorical, ~package,
    "glm",           "Linear / logistic regression",                TRUE,        TRUE,      FALSE,        "stats",
    "mars",          "Multivariate adaptive regression splines",    TRUE,        TRUE,      FALSE,        "earth",
    "multinom_reg",  "Multinomial regression",                      FALSE,       FALSE,     TRUE,         "nnet",
    "rand_forest",   "Random forest",                               TRUE,        TRUE,      TRUE,         "ranger",
    "boost_tree",    "Gradient boosted trees",                      TRUE,        TRUE,      TRUE,         "xgboost"
  )

  registry$installed <- vapply(
    registry$package,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )

  if (outcome_type != "all") {
    registry <- registry[registry[[outcome_type]], ]
    registry <- registry[, !colnames(registry) %in% c("continuous", "binomial", "categorical")]
  }

  if (installed_only) registry <- registry[registry$installed, ]

  if (nrow(registry) == 0) {
    message("No learners found for the specified filters.")
    return(invisible(tibble::tibble()))
  }

  registry
}


# ---------------------------------------------------------------------------- #
# Internal helpers
# ---------------------------------------------------------------------------- #

#' Validate the input dataset before imputation
#' @param dataset The object passed to \code{misl()}.
#' @keywords internal
check_dataset <- function(dataset) {
  if (!is.data.frame(dataset) && !is.matrix(dataset)) {
    stop("'dataset' must be a data frame or matrix.")
  }
  if (nrow(dataset) == 0 || ncol(dataset) == 0) {
    stop("'dataset' must have at least one row and one column.")
  }
  if (sum(is.na(dataset)) == 0) {
    stop("Your dataset is complete - no need for MISL!")
  }
  invisible(NULL)
}


#' Determine the outcome type of a column
#' @param x A vector (one column from the dataset).
#' @return One of \code{"categorical"}, \code{"binomial"}, or \code{"continuous"}.
#' @keywords internal
check_datatype <- function(x) {
  if (is.factor(x))             return("categorical")
  if (all(x %in% c(0, 1, NA))) return("binomial")
  return("continuous")
}


#' Fit a stacked super learner ensemble
#'
#' @param cv_folds Integer number of cross-validation folds used when stacking
#'   multiple learners. Ignored when only a single learner is supplied.
#' @return Named list with \code{$boot} (fit on bootstrap sample) and
#'   \code{$full} (fit on full observed data; \code{NULL} unless continuous).
#' @keywords internal
.fit_super_learner <- function(train_data, full_data, xvars, yvar,
                               outcome_type, learner_names, cv_folds = 5) {

  mode <- if (outcome_type == "continuous") "regression" else "classification"

  # Package required by each optional learner (NA = base R, always available)
  learner_pkgs <- c(
    rand_forest  = "ranger",
    boost_tree   = "xgboost",
    mars         = "earth",
    multinom_reg = "nnet"
  )

  check_learner_pkg <- function(name) {
    pkg <- learner_pkgs[name]
    if (!is.na(pkg) && !requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Learner '", name, "' requires the '", pkg, "' package, which is not installed.\n",
        "  Install it with: install.packages('", pkg, "')"
      )
    }
  }

  make_spec <- function(name) {
    check_learner_pkg(name)
    switch(name,
           glm = {
             if (mode == "regression") parsnip::linear_reg() |> parsnip::set_engine("lm")
             else                      parsnip::logistic_reg() |> parsnip::set_engine("glm")
           },
           rand_forest = {
             parsnip::rand_forest(trees = 100) |>
               parsnip::set_engine("ranger") |>
               parsnip::set_mode(mode)
           },
           boost_tree = {
             parsnip::boost_tree(trees = 100) |>
               parsnip::set_engine("xgboost") |>
               parsnip::set_mode(mode)
           },
           mars = {
             parsnip::mars() |>
               parsnip::set_engine("earth") |>
               parsnip::set_mode(mode)
           },
           multinom_reg = {
             if (mode == "regression") {
               stop("Learner 'multinom_reg' is only valid for categorical outcomes.")
             }
             parsnip::multinom_reg() |> parsnip::set_engine("nnet")
           },
           stop("Unknown learner: '", name, "'. See list_learners() for valid options.")
    )
  }

  prep_outcome <- function(df) {
    if (mode == "classification") df[[yvar]] <- factor(df[[yvar]])
    df
  }
  train_data <- prep_outcome(train_data)
  full_data  <- prep_outcome(full_data)

  make_recipe <- function(df) {
    recipes::recipe(stats::as.formula(paste(yvar, "~ .")), data = df) |>
      recipes::step_dummy(recipes::all_nominal_predictors()) |>
      recipes::step_zv(recipes::all_predictors()) |>
      recipes::step_nzv(recipes::all_predictors()) |>
      recipes::step_normalize(recipes::all_numeric_predictors())
  }

  build_fit <- function(df, rec) {
    wf <- workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(make_spec(learner_names[[1]]))

    if (length(learner_names) == 1) {
      # Single learner: skip stacking, fit directly
      workflows::fit(wf, data = df)
    } else {
      # Multiple learners: build a stacked ensemble
      # allow_par = FALSE prevents inner parallelism conflicting with
      # the outer future_lapply across m datasets.
      cv        <- rsample::vfold_cv(df, v = cv_folds)
      ctrl      <- stacks::control_stack_resamples()
      ctrl$allow_par <- FALSE
      stack_obj <- stacks::stacks()

      for (nm in learner_names) {
        wf_nm <- workflows::workflow() |>
          workflows::add_recipe(rec) |>
          workflows::add_model(make_spec(nm))
        rs        <- tune::fit_resamples(wf_nm, resamples = cv, control = ctrl)
        stack_obj <- stacks::add_candidates(stack_obj, rs, name = nm)
      }

      stack_obj |>
        stacks::blend_predictions() |>
        stacks::fit_members()
    }
  }

  # Build the recipe once from full_data so that step_zv/step_nzv drop the
  # same predictors for both the bootstrap and full fits.
  shared_rec <- make_recipe(full_data)

  list(
    boot = build_fit(train_data, shared_rec),
    full = if (outcome_type == "continuous") build_fit(full_data, shared_rec) else NULL
  )
}
