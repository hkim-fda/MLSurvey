# tests/testthat/test-par_wElnet.R
# Smoke tests for par_wElnet(). Uses make_test_data() from helper-wmodels.R.
#
# NOTE: these tests spawn a PSOCK cluster. They are skipped both on CRAN
# (skip_on_cran) and when run interactively inside RStudio (skip_if
# interactive()), since spinning up a parallel cluster from a live
# interactive RStudio session has been observed to crash the R session
# outright rather than raising a catchable error. These tests still run
# fine under devtools::check() or Rscript, which execute in a separate,
# non-interactive R process.

test_data <- make_test_data()
col.x <- which(names(test_data) %in% c("x1", "x2", "x3", "x4", "x5"))


test_that("par_wElnet() runs across a small alpha grid and picks a best alpha", {
  
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")
  skip_on_cran()
  skip_if(interactive(), "Spawning PSOCK clusters inside an interactive RStudio session can crash the session; run via devtools::check() or Rscript instead.")
  
  alpha.grid <- c(0, 0.5, 1)
  
  fit <- par_wElnet(alpha = alpha.grid, n.cores = 2,
                    data = test_data, col.y = "HBP", col.x = col.x,
                    family = "binomial",
                    cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                    method = "dCV", k = 4, R = 1)
  
  expect_s3_class(fit, "par.w.elnet")
  expect_equal(nrow(fit$summary), length(alpha.grid))
  expect_true(fit$best.alpha %in% alpha.grid)
  expect_s3_class(fit$best, "w.elnet")
})

test_that("print.par.w.elnet() runs without error", {
  
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")
  skip_on_cran()
  skip_if(interactive(), "Spawning PSOCK clusters inside an interactive RStudio session can crash the session; run via devtools::check() or Rscript instead.")
  
  fit <- par_wElnet(alpha = c(0, 1), n.cores = 2,
                    data = test_data, col.y = "HBP", col.x = col.x,
                    family = "binomial",
                    cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                    method = "dCV", k = 4, R = 1)
  
  expect_output(print(fit), "Parallel Weighted Elastic Net")
})

test_that("par_wElnet() runs with method = 'JKn'", {
  
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")
  skip_on_cran()
  skip_if(interactive(), "Spawning PSOCK clusters inside an interactive RStudio session can crash the session; run via devtools::check() or Rscript instead.")
  
  alpha.grid <- c(0, 0.5, 1)
  
  fit <- par_wElnet(alpha = alpha.grid, n.cores = 2,
                    data = test_data, col.y = "HBP", col.x = col.x,
                    family = "binomial",
                    cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                    method = "JKn")
  
  expect_s3_class(fit, "par.w.elnet")
  expect_equal(nrow(fit$summary), length(alpha.grid))
  expect_true(fit$best.alpha %in% alpha.grid)
})