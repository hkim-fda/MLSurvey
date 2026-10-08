# tests/testthat/helper-wmodels.R
#
# Shared synthetic test data, auto-loaded by testthat before any test-*.R
# file runs (files prefixed "helper-" are sourced automatically).
#
# Mirrors the real-world column structure (cluster/strata/weights +
# predictors + binary outcome) without depending on any external file,
# so tests are fully portable and reproducible on CRAN's check machines.
#
# IMPORTANT: strata/cluster combinations are kept reasonably well-populated
# (n = 200, only 2 strata x 2 clusters = 4 combinations) rather than finely
# subdivided. Stratified resampling inside randomForest::randomForest() (and
# fold assignment inside replicate_weights()) operates on these combinations
# directly; if any combination has too few rows, compiled C code in
# randomForest can crash the R session outright rather than raising a
# catchable R error. Keep this generous unless stress-testing sparse strata
# specifically.
make_test_data <- function(n = 200, seed = 1){
  set.seed(seed)
  
  data.frame(
    SDMVPSU   = rep(1:2, length.out = n),
    SDMVSTRA  = rep(1:2, length.out = n),
    WTSAF2YR  = runif(n, 500, 5000),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rbinom(n, 1, 0.4),
    HBP = rbinom(n, 1, 0.3)
  )
}