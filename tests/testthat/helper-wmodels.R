# tests/testthat/helper-wmodels.R
#
# Shared synthetic test fixtures, auto-loaded by testthat before any
# test-*.R file runs (files prefixed "helper-" are sourced automatically
# into an environment visible to all test files). Defining test_data and
# col.x here -- rather than at the top of each test-*.R file -- also avoids
# a scoping issue where get(design$call$data) inside wElnet()/wRandomforest()
# cannot reliably find a data object defined only at test-file top level.
#
# Mirrors the real-world column structure (cluster/strata/weights +
# predictors + binary outcome) without depending on any external file, so
# tests are fully portable and reproducible on CRAN's check machines.
#
# Design notes on avoiding known failure modes:
#
# 1. PSU must be properly NESTED within strata, with >= 2 PSUs per stratum.
#    Replicate-weight methods (especially JKn, which calls
#    survey::as.svrepdesign() -> survey::jknweights()) require this for
#    jackknife variance estimation; a stratum with only one PSU errors with
#    "Stratum k has only one PSU". A naive rep(1:2, length.out = n) for both
#    SDMVSTRA and SDMVPSU with the same period/phase accidentally collapses
#    each stratum to a single PSU -- avoid that pattern.
#
# 2. The binary outcome (HBP) is assigned deterministically to guarantee
#    both classes are well-represented in every stratum x cluster
#    combination and in every CV fold's training subset. A purely random
#    rbinom() draw can -- by chance, depending on seed and k -- leave some
#    fold's training subset with only one class present, which fails inside
#    randomForest::randomForest() with "Need at least two classes to do
#    classification". Determinism removes this flakiness entirely.
#
# 3. strata/cluster combinations are kept reasonably well-populated
#    (n = 200, 2 strata x 2 PSUs per stratum = 4 combinations, 50 rows each)
#    rather than finely subdivided, since randomForest's stratified
#    resampling is implemented in compiled code and can crash the R session
#    outright (rather than raising a catchable error) if a combination has
#    too few rows.
# Defensively set the same survey options recommended in the package's own
# @examples blocks (options(survey.adjust.domain.lonely=TRUE) and
# options(survey.lonely.psu="adjust")), which avoid errors/warnings from a
# stratum having only one PSU within a particular domain or subpopulation.
# The current synthetic test_data below is structurally designed to avoid
# lonely PSUs entirely (every stratum has >= 2 distinct PSUs), so these
# options are not strictly required for the existing tests to pass -- but
# setting them matches documented real-world usage and protects against
# future tests that may use sparser or domain-subset synthetic data.
#
# NOTE: helper-*.R files are sourced by testthat BEFORE any test context
# exists, so scoping helpers like withr::local_options(.local_envir =
# teardown_env()) cannot be used here -- teardown_env() is only valid
# inside an active test_that()/test file run, not at helper-sourcing time.
# We therefore set these options as plain, session-wide options() calls.
# They will persist for the remainder of the R session (or until the next
# devtools::test()/R CMD check run, which starts a fresh session), which is
# the standard and accepted pattern for testthat helper files that need to
# set global options for the whole test suite.
options(survey.adjust.domain.lonely = TRUE, survey.lonely.psu = "adjust")

make_test_data <- function(n = 240, seed = 1){
  set.seed(seed)
  
  # 4 strata, each containing 3 distinct PSUs (12 total strata x PSU
  # combinations). n must be divisible by 12 (4 strata x 3 PSUs) for the
  # rep() calls below to produce exactly n rows for both SDMVSTRA and
  # SDMVPSU -- n = 240 gives 60 rows per stratum, 20 rows per stratum x PSU
  # combination. Widened from an earlier 2x2 (4 total) design specifically
  # to make bootstrap/subbootstrap's "full coverage" edge case negligible:
  # with only 4 distinct PSUs, a B-replicate bootstrap resample is highly
  # likely to draw every PSU at least once in at least one replicate,
  # leaving that replicate with zero true out-of-bag (OOB) units -- an
  # all-zero sw_r_1_test_* weight column, which then fails deep inside
  # xgboost (or produces a degenerate error calculation) rather than
  # reflecting an actual package bug. With 12 distinct PSUs, the
  # probability of full coverage in any single bootstrap replicate is
  # astronomically small, making this edge case a non-issue for routine
  # testing.
  SDMVSTRA <- rep(1:4, each = n / 4)
  SDMVPSU  <- rep(rep(1:3, each = n / 12), times = 4)
  
  # Deterministic, perfectly balanced binary outcome: alternates 0/1 so
  # every stratum x cluster combination, and every k-fold split, is
  # guaranteed to contain both classes regardless of k or the random fold
  # assignment draw.
  HBP <- rep(c(0, 1), length.out = n)
  
  data.frame(
    SDMVPSU  = SDMVPSU,
    SDMVSTRA = SDMVSTRA,
    WTSAF2YR = runif(n, 500, 5000),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rbinom(n, 1, 0.4),
    HBP = HBP
  )
}

# Shared fixtures used across test-wElnet.R, test-par_wElnet.R,
# test-wXGBoost.R, and test-wRandomforest.R.
test_data <- make_test_data()
col.x <- which(names(test_data) %in% c("x1", "x2", "x3", "x4", "x5"))