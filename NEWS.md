# misl 2.0.0

## New features

* `misl()` now accepts any parsnip-compatible model spec directly via the
  `*_method` arguments, allowing any learner in the tidymodels ecosystem to be
  used without requiring a package update. Named strings and parsnip specs can
  be freely mixed in the same list.

* New `ord_method` argument in `misl()` for ordered categorical (ordinal)
  outcomes. Ordered factors are automatically detected and routed to
  `ord_method`.

* New built-in learner `"polr"` (proportional odds logistic regression via
  `MASS::polr()`) for ordinal outcomes. Note that `"polr"` cannot currently
  be stacked with other ordinal learners — when supplied alongside other
  learners in `ord_method`, `"polr"` will be used as the sole ordinal learner
  and others will be ignored with a warning. Full stacking support for ordinal
  outcomes is planned for a future release.

* New `plot_misl_trace()` function for visualising convergence of imputed
  values across iterations, with one line per imputed dataset.

* `list_learners()` now accepts `"ordinal"` as a valid `outcome_type` filter
  and includes the new `ordinal` support column.

## Bug fixes

* `cv_folds` argument is now correctly passed through to the internal
  `.fit_super_learner()` function. Previously the value was accepted by
  `misl()` but ignored, with 5 folds always used regardless of the user's
  setting.

* The preprocessing recipe is now built once from the full observed data and
  shared across both the bootstrap and full fits for continuous outcomes. 
  Previously separate recipes were built for each fit, meaning `step_zv()` and
  `step_nzv()` could drop different predictors between the two fits and produce
  inconsistent PMM donor selection.

* Two-level factors (e.g. `"Yes"/"No"`) are now correctly identified as
  binomial outcomes rather than categorical. Previously these were routed to
  `cat_method`, which caused incorrect imputation and errors with learners that
  require a numeric binary outcome.

* Trace statistics (`mean`, `sd`) are no longer computed for factor-coded
  binary columns, preventing errors from calling `mean()` and `var()` on factor
  vectors.

* Stacking now gracefully recovers when individual learners fail during
  cross-validation resampling. Previously a single learner failure would crash
  the entire imputation. Failed learners are now skipped with a warning, and if
  all learners fail the first learner is used as a fallback.

# misl 1.0.0

* Initial CRAN submission.
