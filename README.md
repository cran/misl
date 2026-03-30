# misl <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Note:** This package is currently experimental and under active development. The API may change. Feedback and bug reports are welcome via [GitHub Issues](https://github.com/JustinManjourides/misl/issues).

## Overview

`misl` implements **Multiple Imputation by Super Learning (MISL)**, a flexible approach to handling missing data that uses a stacked ensemble of machine learning algorithms to impute missing values across continuous, binary, and categorical variables.

Rather than relying on a single parametric imputation model, MISL builds a super learner for each incomplete variable using the [tidymodels](https://www.tidymodels.org/) framework, combining learners such as linear/logistic regression, random forests, gradient boosted trees, and MARS to produce well-calibrated imputations.

The method is described in:

> Carpenito T, Manjourides J. (2022) MISL: Multiple imputation by super learning. *Statistical Methods in Medical Research*. 31(10):1904–1915. doi: [10.1177/09622802221104238](https://doi.org/10.1177/09622802221104238)

---

## Installation

`misl` is not yet on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("JustinManjourides/misl")
```

The following backend packages are optional but recommended:

```r
install.packages(c("ranger", "xgboost", "earth"))
```

---

## Quick Start

```r
library(misl)

# Introduce missingness into a dataset
set.seed(42)
n <- 200
demo_data <- data.frame(
  age    = rnorm(n, mean = 50, sd = 10),
  weight = rnorm(n, mean = 70, sd = 15),
  smoker = rbinom(n, 1, 0.3),
  group  = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
demo_data[sample(n, 20), "age"]    <- NA
demo_data[sample(n, 15), "weight"] <- NA
demo_data[sample(n, 10), "smoker"] <- NA
demo_data[sample(n, 10), "group"]  <- NA

# Run MISL with default settings
misl_imp <- misl(
  demo_data,
  m      = 5,
  maxit  = 5,
  con_method = c("glm", "rand_forest"),
  bin_method = c("glm", "rand_forest"),
  cat_method = c("rand_forest", "multinom_reg")
)

# Each of the m imputed datasets is accessible via:
completed_data <- misl_imp[[1]]$datasets

# Trace plots can be used to inspect convergence:
trace <- misl_imp[[1]]$trace
```

### Parallelism

Imputation across the `m` datasets is parallelised via the
[future](https://future.futureverse.org/) framework. To enable parallel
execution, set a plan before calling `misl()`:

```r
library(future)
plan(multisession, workers = 4)

misl_imp <- misl(demo_data, m = 5, maxit = 5)

plan(sequential)  # reset when done
```

### Available learners

```r
# View all available learners
list_learners()

# Filter by outcome type
list_learners("continuous")
list_learners("categorical")

# Show only installed learners
list_learners(installed_only = TRUE)
```

---

## Citation

If you use `misl` in your research, please cite the original paper:

```
Carpenito T, Manjourides J. (2022) MISL: Multiple imputation by super
learning. Statistical Methods in Medical Research. 31(10):1904-1915.
doi: 10.1177/09622802221104238
```

BibTeX:

```bibtex
@article{carpenito2022misl,
  author  = {Carpenito, T and Manjourides, J},
  title   = {{MISL}: Multiple imputation by super learning},
  journal = {Statistical Methods in Medical Research},
  year    = {2022},
  volume  = {31},
  number  = {10},
  pages   = {1904--1915},
  doi     = {10.1177/09622802221104238}
}
```

---

## License

MIT © see [LICENSE](LICENSE)
