# misl2.R
# Multiple Imputation by Super Learning - Version 2.0
# Changes from v1.0:
#   - *_method arguments now accept parsnip model specs in addition to
#     named character strings, allowing any parsnip-compatible learner
#   - New ord_method argument for ordered categorical (ordinal) outcomes
#   - check_datatype() now distinguishes ordered from unordered factors
#   - list_learners() updated with ordinal column and polr learner

#' @importFrom stats predict runif sd as.formula
NULL

# Suppress R CMD check notes for ggplot2 column references
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("iteration", "value", "m", "statistic", "variable"))
}

# ---------------------------------------------------------------------------- #
# Public API
# ---------------------------------------------------------------------------- #

#' MISL: Multiple Imputation by Super Learning (v2.0)
#'
#' Imputes missing values using multiple imputation by super learning.
#'
#' @param dataset A dataframe or matrix containing the incomplete data.
#'   Missing values are represented with \code{NA}.
#' @param m The number of multiply imputed datasets to create. Default \code{5}.
#' @param maxit The number of iterations per imputed dataset. Default \code{5}.
#' @param seed Integer seed for reproducibility, or \code{NA} to skip. Default \code{NA}.
#' @param con_method Character vector of learner IDs, a list of parsnip model
#'   specs, or a mixed list of both, for continuous columns.
#'   Default \code{c("glm", "rand_forest", "boost_tree")}.
#' @param bin_method Character vector of learner IDs, a list of parsnip model
#'   specs, or a mixed list of both, for binary columns
#'   (values must be \code{0/1/NA} or a two-level factor). Default
#'   \code{c("glm", "rand_forest", "boost_tree")}.
#' @param cat_method Character vector of learner IDs, a list of parsnip model
#'   specs, or a mixed list of both, for unordered categorical columns.
#'   Default \code{c("rand_forest", "boost_tree")}.
#' @param ord_method Character vector of learner IDs, a list of parsnip model
#'   specs, or a mixed list of both, for ordered categorical columns.
#'   Default \code{c("polr", "rand_forest", "boost_tree")}.
#' @param cv_folds Integer number of cross-validation folds used when stacking
#'   multiple learners. Reducing this (e.g. to \code{3}) speeds up computation
#'   at a small cost to ensemble accuracy. Default \code{5}. Ignored when only
#'   a single learner is supplied.
#' @param ignore_predictors Character vector of column names to exclude as
#'   predictors. Default \code{NA}.
#' @param quiet Suppress console progress messages. Default \code{TRUE}.
#'
#' @details
#' Built-in named learners (see \code{\link{list_learners}()}):
#' \itemize{
#'   \item \code{"glm"}          - base R (logistic for binary, linear for continuous)
#'   \item \code{"rand_forest"}  - \pkg{ranger}
#'   \item \code{"boost_tree"}   - \pkg{xgboost}
#'   \item \code{"mars"}         - \pkg{earth}
#'   \item \code{"multinom_reg"} - \pkg{nnet} (unordered categorical only)
#'   \item \code{"polr"}         - \pkg{MASS} (ordered categorical only)
#' }
#'
#' Any parsnip-compatible model spec can also be passed directly via the
#' \code{*_method} arguments. Named strings and parsnip specs can be mixed
#' in the same list:
#' \preformatted{
#' library(parsnip)
#' misl(data,
#'   con_method = list(
#'     "glm",
#'     rand_forest(trees = 500) |> set_engine("ranger")
#'   )
#' )
#' }
#' The mode (regression vs classification) is always enforced by \code{misl}
#' regardless of what is set on the spec.
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
#' # Using named learners (same as v1.0)
#' set.seed(1)
#' n <- 100
#' demo_data <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
#' demo_data[sample(n, 10), "y"] <- NA
#' misl_imp <- misl(demo_data, m = 2, maxit = 2, con_method = "glm")
#'
#' # Using a custom parsnip spec
#' \dontrun{
#' library(parsnip)
#' misl_imp <- misl(
#'   demo_data, m = 2, maxit = 2,
#'   con_method = list(
#'     "glm",
#'     rand_forest(trees = 500) |> set_engine("ranger")
#'   )
#' )
#' }
misl <- function(dataset,
                 m                 = 5,
                 maxit             = 5,
                 seed              = NA,
                 con_method        = c("glm", "rand_forest", "boost_tree"),
                 bin_method        = c("glm", "rand_forest", "boost_tree"),
                 cat_method        = c("rand_forest", "boost_tree"),
                 ord_method        = c("polr", "rand_forest", "boost_tree"),
                 cv_folds          = 5,
                 ignore_predictors = NA,
                 quiet             = TRUE) {

  # --- 0. Validity checks ---
  check_dataset(dataset)
  if (!is.numeric(cv_folds) || cv_folds < 2 || cv_folds != as.integer(cv_folds)) {
    stop("'cv_folds' must be an integer >= 2.")
  }

  # Coerce *_method arguments to lists for uniform handling downstream,
  # preserving backwards compatibility with character vector inputs
  con_method <- as.list(con_method)
  bin_method <- as.list(bin_method)
  cat_method <- as.list(cat_method)
  ord_method <- as.list(ord_method)

  # If ord_method contains "polr" alongside other learners, polr cannot
  # currently be stacked. Warn and reduce to polr only -- but only if the
  # dataset actually contains an ordinal variable, to avoid spurious warnings.
  has_ordinal <- any(sapply(dataset, function(x)
    is.factor(x) && nlevels(x) > 2 && is.ordered(x)))

  if (has_ordinal && "polr" %in% ord_method && length(ord_method) > 1L) {
    warning(
      "'polr' cannot currently be stacked with other ordinal learners. ",
      "Using 'polr' as the sole ordinal learner and ignoring others. ",
      "See list_learners() for details."
    )
    ord_method <- list("polr")
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
                                  categorical = cat_method,
                                  ordinal     = ord_method
          )

          # --- 3. Fit stacked super learner ---
          sl_fit <- .fit_super_learner(
            train_data    = boot_df,
            full_data     = full_df,
            xvars         = xvars,
            yvar          = col,
            outcome_type  = outcome_type,
            learner_names = learner_names,
            cv_folds      = cv_folds
          )

          # --- 4. Impute ---
          pred_data <- data_cur[, xvars, drop = FALSE]

          if (outcome_type == "binomial") {
            lvls         <- if (is.factor(dataset[[col]])) levels(dataset[[col]]) else c(0L, 1L)
            preds        <- predict(sl_fit$boot, new_data = pred_data, type = "prob")[[2]]
            imputed_vals <- lvls[as.integer(stats::runif(length(preds)) <= preds) + 1L]

            data_cur[[col]] <- if (is.factor(dataset[[col]])) {
              factor(ifelse(miss_idx, imputed_vals, as.character(dataset[[col]])), levels = lvls)
            } else {
              ifelse(miss_idx, as.integer(imputed_vals), dataset[[col]])
            }

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

          } else if (outcome_type %in% c("categorical", "ordinal")) {
            prob_mat <- if (inherits(sl_fit$boot, "misl_polr_fit")) {
              as.matrix(predict(sl_fit$boot, new_data = pred_data, type = "prob"))
            } else {
              as.matrix(predict(sl_fit$boot, new_data = pred_data, type = "prob"))
            }
            lvls         <- levels(dataset[[col]])
            u            <- stats::runif(nrow(prob_mat))
            cum_mat      <- t(apply(prob_mat, 1, cumsum))
            idx          <- pmin(1 + rowSums(u > cum_mat), length(lvls))
            imputed_vals <- lvls[idx]

            data_cur[[col]] <- factor(
              ifelse(miss_idx, imputed_vals, as.character(dataset[[col]])),
              levels  = lvls,
              ordered = is.ordered(dataset[[col]])
            )
          }

          # --- 5. Trace statistics ---
          if (!outcome_type %in% c("categorical", "ordinal") && any(miss_idx)) {
            imp_vals <- data_cur[[col]][miss_idx]
            if (is.numeric(imp_vals)) {
              rows <- trace_plot$variable == col &
                trace_plot$m         == m_loop &
                trace_plot$iteration == i_loop

              trace_plot$value[rows & trace_plot$statistic == "mean"] <- mean(imp_vals)
              trace_plot$value[rows & trace_plot$statistic == "sd"]   <- stats::sd(imp_vals)
            }
          }

        } # end column loop
      } # end iteration loop

      list(datasets = data_cur, trace = trace_plot)
    }
  )
}


#' List available learners for MISL imputation
#'
#' Displays the built-in named learners available for use in
#' \code{\link{misl}()}. Note that any parsnip-compatible model spec can
#' also be passed directly via the \code{*_method} arguments.
#'
#' @param outcome_type One of \code{"continuous"}, \code{"binomial"},
#'   \code{"categorical"}, \code{"ordinal"}, or \code{"all"} (default).
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
#' list_learners("ordinal")
#' list_learners("categorical", installed_only = TRUE)
list_learners <- function(outcome_type = "all", installed_only = FALSE) {

  outcome_type <- match.arg(outcome_type,
                            c("all", "continuous", "binomial", "categorical", "ordinal"))

  registry <- tibble::tribble(
    ~learner,        ~description,                                  ~continuous, ~binomial, ~categorical, ~ordinal, ~package,
    "glm",           "Linear / logistic regression",                TRUE,        TRUE,      FALSE,        FALSE,    "stats",
    "mars",          "Multivariate adaptive regression splines",    TRUE,        TRUE,      FALSE,        FALSE,    "earth",
    "multinom_reg",  "Multinomial regression",                      FALSE,       FALSE,     TRUE,         FALSE,    "nnet",
    "polr",          "Proportional odds logistic regression",       FALSE,       FALSE,     FALSE,        TRUE,     "MASS",
    "rand_forest",   "Random forest",                               TRUE,        TRUE,      TRUE,         TRUE,     "ranger",
    "boost_tree",    "Gradient boosted trees",                      TRUE,        TRUE,      TRUE,         TRUE,     "xgboost"
  )

  registry$installed <- vapply(
    registry$package,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )

  if (outcome_type != "all") {
    registry <- registry[registry[[outcome_type]], ]
    registry <- registry[, !colnames(registry) %in%
                           c("continuous", "binomial", "categorical", "ordinal")]
  }

  if (installed_only) registry <- registry[registry$installed, ]

  if (nrow(registry) == 0) {
    message("No learners found for the specified filters.")
    return(invisible(tibble::tibble()))
  }

  if (outcome_type %in% c("all", "ordinal")) {
    message(
      "Note: 'polr' cannot currently be stacked with other ordinal learners. ",
      "When 'polr' is supplied alongside other learners in ord_method, it will ",
      "be used as the sole ordinal learner and others will be ignored. ",
      "Full stacking support for ordinal outcomes is planned for a future release."
    )
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
#' @return One of \code{"categorical"}, \code{"ordinal"}, \code{"binomial"},
#'   or \code{"continuous"}.
#' @keywords internal
check_datatype <- function(x) {
  if (is.factor(x) && nlevels(x) > 2 && !is.ordered(x)) return("categorical")
  if (is.factor(x) && nlevels(x) > 2 &&  is.ordered(x)) return("ordinal")
  if (is.factor(x) && nlevels(x) <= 2)                   return("binomial")
  if (all(x %in% c(0, 1, NA)))                            return("binomial")
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

  # Package required by each built-in named learner
  learner_pkgs <- c(
    rand_forest  = "ranger",
    boost_tree   = "xgboost",
    mars         = "earth",
    multinom_reg = "nnet",
    polr         = "MASS"
  )

  # Resolve a single learner element to a parsnip spec
  make_spec <- function(x) {

    # --- User-supplied parsnip spec ---
    if (inherits(x, "model_spec")) {
      return(parsnip::set_mode(x, mode))
    }

    # --- Named built-in learner ---
    if (is.character(x)) {
      pkg <- learner_pkgs[x]
      if (!is.na(pkg) && !requireNamespace(pkg, quietly = TRUE)) {
        stop(
          "Learner '", x, "' requires the '", pkg, "' package, which is not installed.\n",
          "  Install it with: install.packages('", pkg, "')"
        )
      }

      return(switch(x,
                    glm = {
                      if (mode == "regression") parsnip::linear_reg()   |> parsnip::set_engine("lm")
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
                    polr = {
                      if (mode == "regression") {
                        stop("Learner 'polr' is only valid for ordinal outcomes.")
                      }
                      # polr is handled separately in build_fit() via MASS::polr() directly
                      "polr"
                    },
                    stop("Unknown learner: '", x, "'. See list_learners() for valid options.")
      ))
    }

    # --- Neither string nor parsnip spec ---
    stop(
      "Each learner must be either a named character string or a parsnip model spec.\n",
      "  See list_learners() for valid named options, or ?parsnip::model_spec for ",
      "custom specs."
    )
  }

  prep_outcome <- function(df) {
    if (mode == "classification") {
      if (outcome_type == "ordinal") {
        df[[yvar]] <- as.ordered(df[[yvar]])
      } else {
        df[[yvar]] <- factor(df[[yvar]])
      }
    }
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

  # Build the recipe once from full_data so that step_zv/step_nzv drop the
  # same predictors for both the bootstrap and full fits.
  shared_rec <- make_recipe(full_data)

  build_fit <- function(df, rec) {

    # Special case: single polr learner -- bypass parsnip entirely
    if (length(learner_names) == 1 && identical(learner_names[[1]], "polr")) {
      prepped  <- recipes::prep(rec, training = df)
      baked    <- recipes::bake(prepped, new_data = df)
      fmla     <- stats::as.formula(paste(yvar, "~ ."))
      fit      <- tryCatch(
        MASS::polr(fmla, data = baked, Hess = TRUE),
        error = function(e) stop("polr failed during fitting: ", conditionMessage(e))
      )
      # Return a lightweight wrapper with a predict method
      structure(
        list(fit = fit, recipe = prepped, type = "polr"),
        class = "misl_polr_fit"
      )
    } else {
      wf <- workflows::workflow() |>
        workflows::add_recipe(rec) |>
        workflows::add_model(make_spec(learner_names[[1]]))

      if (length(learner_names) == 1) {
        # Single non-polr learner: skip stacking, fit directly
        tryCatch(
          workflows::fit(wf, data = df),
          error = function(e) {
            if (inherits(learner_names[[1]], "model_spec")) {
              stop(
                "User-supplied learner failed during fitting.\n",
                "  Check that the required engine package is installed and that\n",
                "  the spec is compatible with a ", mode, " outcome.\n",
                "  Original error: ", conditionMessage(e)
              )
            }
            stop(e)
          }
        )
      } else {
        # Multiple learners: build a stacked ensemble
        cv        <- rsample::vfold_cv(df, v = cv_folds)
        ctrl      <- stacks::control_stack_resamples()
        ctrl$allow_par <- FALSE
        stack_obj <- stacks::stacks()

        n_candidates <- 0L
        for (nm in learner_names) {
          wf_nm <- workflows::workflow() |>
            workflows::add_recipe(rec) |>
            workflows::add_model(make_spec(nm))

          rs <- tryCatch(
            tune::fit_resamples(wf_nm, resamples = cv, control = ctrl),
            error = function(e) {
              if (inherits(nm, "model_spec")) {
                warning(
                  "User-supplied learner failed during resampling and will be skipped.\n",
                  "  Check that the required engine package is installed and that\n",
                  "  the spec is compatible with a ", mode, " outcome.\n",
                  "  Original error: ", conditionMessage(e)
                )
              } else {
                warning("Learner '", nm, "' failed during resampling and will be skipped: ",
                        conditionMessage(e))
              }
              NULL
            }
          )

          if (!is.null(rs)) {
            stack_obj <- tryCatch(
              stacks::add_candidates(stack_obj, rs,
                                     name = if (is.character(nm)) nm else "custom"),
              error = function(e) {
                warning("Learner could not be added to the stack and will be skipped: ",
                        conditionMessage(e))
                stack_obj
              }
            )
            n_candidates <- n_candidates + 1L
          }
        }

        if (n_candidates == 0L) {
          warning("All learners failed during stacking; falling back to first learner.")
          return(
            tryCatch(
              workflows::fit(
                workflows::workflow() |>
                  workflows::add_recipe(rec) |>
                  workflows::add_model(make_spec(learner_names[[1]])),
                data = df
              ),
              error = function(e) {
                if (inherits(learner_names[[1]], "model_spec")) {
                  stop(
                    "User-supplied fallback learner failed during fitting.\n",
                    "  Check that the required engine package is installed and that\n",
                    "  the spec is compatible with a ", mode, " outcome.\n",
                    "  Original error: ", conditionMessage(e)
                  )
                }
                stop(e)
              }
            )
          )
        }

        stack_obj |>
          stacks::blend_predictions() |>
          stacks::fit_members()
      }
    }
  }

  list(
    boot = build_fit(train_data, shared_rec),
    full = if (outcome_type == "continuous") build_fit(full_data, shared_rec) else NULL
  )
}

#' Plot trace statistics from a MISL imputation
#'
#' Plots the mean and standard deviation of imputed values across iterations
#' for all incomplete variables, paginated in grids of up to 3 variables per
#' page. Stable traces that mix well across datasets indicate convergence.
#' Note that trace statistics are only computed for continuous and numeric
#' binary columns -- categorical and ordinal columns are excluded automatically.
#'
#' @param misl_result A list returned by \code{\link{misl}()}.
#' @param ncol Number of columns per page. Default \code{2}.
#' @param nrow Number of rows per page. Default \code{3}.
#'
#' @return Invisibly returns the long-format trace data frame used for plotting.
#' @export
#'
#' @examples
#' set.seed(1)
#' n <- 100
#' demo_data <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
#' demo_data[sample(n, 10), "y"] <- NA
#' misl_imp <- misl(demo_data, m = 3, maxit = 3, con_method = "glm")
#' plot_misl_trace(misl_imp)
plot_misl_trace <- function(misl_result, ncol = 2, nrow = 3) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_misl_trace(). ",
         "Install it with: install.packages('ggplot2')")
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    stop("Package 'ggforce' is required for plot_misl_trace(). ",
         "Install it with: install.packages('ggforce')")
  }

  # Combine trace data across all m datasets and drop NAs
  # (categorical and ordinal columns have NA trace values)
  trace <- do.call(rbind, lapply(misl_result, function(r) r$trace))
  trace <- trace[stats::complete.cases(trace), ]
  trace <- trace[!is.na(trace$value), ]

  if (nrow(trace) == 0) {
    message("No trace data available -- trace is only computed for continuous ",
            "and numeric binary columns.")
    return(invisible(trace))
  }

  trace$m         <- as.factor(trace$m)
  trace$iteration <- as.integer(trace$iteration)

  n_panels  <- length(unique(trace$variable)) * length(unique(trace$statistic))
  num_pages <- ceiling(n_panels / (ncol * nrow))

  for (page_num in seq_len(num_pages)) {
    print(
      ggplot2::ggplot(
        trace,
        ggplot2::aes(
          x     = iteration,
          y     = value,
          group = m,
          color = m
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 1.5) +
        ggforce::facet_wrap_paginate(
          ggplot2::vars(variable, statistic),
          scales   = "free",
          ncol     = ncol,
          nrow     = nrow,
          page     = page_num
        ) +
        ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
        ggplot2::labs(x = "Iteration", y = "Value", color = "Dataset (m)") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  invisible(trace)
}

#' Predict method for misl_polr_fit objects
#' @keywords internal
predict.misl_polr_fit <- function(object, new_data, type = "prob", ...) {
  baked <- recipes::bake(object$recipe, new_data = new_data)
  probs <- predict(object$fit, newdata = baked, type = "probs")
  if (is.vector(probs)) probs <- matrix(probs, nrow = 1)
  as.data.frame(probs)
}
