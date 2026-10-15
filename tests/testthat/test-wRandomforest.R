# tests/testthat/test-wRandomforest.R
# Smoke tests for wRandomforest(). Uses make_test_data() from helper-wmodels.R.
#
# If RStudio's R session crashes (not a normal testthat failure) while
# running this file specifically, see helper-wmodels.R's comment on strata
# sparsity -- randomForest::randomForest()'s stratified sampling is
# implemented in compiled code and can segfault rather than error cleanly
# when a strata x cluster combination has too few observations.

test_data <- make_test_data()
col.x <- which(names(test_data) %in% c("x1", "x2", "x3", "x4", "x5"))


test_that("wRandomforest() runs on small synthetic classification data", {

  skip_if_not_installed("randomForest")
  skip_if_not_installed("survey")

  fit <- wRandomforest(data = test_data, y = as.factor(test_data$HBP), col.x = col.x,
                       cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                       method = "dCV", k = 4, R = 1,
                       ntree = 50)

  expect_s3_class(fit, "w.randomforest")
  expect_true(is.numeric(fit$mtry$all))
  expect_true(is.numeric(fit$mtry$optimal.mtry))
  expect_true(is.numeric(fit$evaluation_log$weighted.test.error))
  expect_s3_class(fit$model, "randomForest")
})

test_that("wRandomforest() respects a user-supplied design object", {

  skip_if_not_installed("randomForest")
  skip_if_not_installed("survey")

  des <- survey::svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                           weights = ~WTSAF2YR, nest = TRUE, data = test_data)

  fit <- wRandomforest(y = as.factor(test_data$HBP), col.x = col.x, design = des,
                       method = "dCV", k = 4, R = 1, ntree = 50)

  expect_s3_class(fit, "w.randomforest")
})

test_that("wRandomforest() runs with method = 'JKn'", {

  skip_if_not_installed("randomForest")
  skip_if_not_installed("survey")

  fit <- wRandomforest(data = test_data, y = as.factor(test_data$HBP), col.x = col.x,
                       cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                       method = "JKn", ntree = 50)

  expect_s3_class(fit, "w.randomforest")
  expect_true(is.numeric(fit$mtry$optimal.mtry))
  expect_true(is.numeric(fit$evaluation_log$weighted.test.error))
})

test_that("wRandomforest() rejects method = 'bootstrap' and 'subbootstrap' with an informative error", {
  # wRandomforest() intentionally does not support top-level bootstrap/
  # subbootstrap replicate-weight methods, since randomForest::randomForest()
  # already performs bootstrap resampling internally for every tree. See
  # wRandomforest()'s @details for the rationale.

  skip_if_not_installed("randomForest")
  skip_if_not_installed("survey")

  expect_error(
    wRandomforest(data = test_data, y = as.factor(test_data$HBP), col.x = col.x,
                 cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                 method = "bootstrap", ntree = 50),
    "not supported by wRandomforest"
  )

  expect_error(
    wRandomforest(data = test_data, y = as.factor(test_data$HBP), col.x = col.x,
                 cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                 method = "subbootstrap", ntree = 50),
    "not supported by wRandomforest"
  )
})

test_that("REGRESSION: eval.error() computes bootstrap error against each replicate's OOB holdout (not the full sample), even though wRandomforest() currently never calls it with this method", {
  # This mirrors the equivalent error.f() regression test for wElnet(). The
  # same weights.name bug existed here: for bootstrap/subbootstrap,
  # weights.name was never reconstructed per-fold, so it stayed fixed at
  # the plain full-sample weights column for all B replicates, silently
  # computing error against the entire original sample instead of each
  # replicate's actual out-of-bag (OOB) held-out units.
  #
  # NOTE: wRandomforest() itself explicitly rejects method = "bootstrap"/
  # "subbootstrap" (see the test above) and therefore never reaches this
  # code path today. This test calls eval.error() directly to verify the
  # underlying utility function is correct regardless, since it is a
  # standalone function that could be reused or called elsewhere.

  skip_if_not_installed("survey")

  set.seed(42)
  rw_data <- replicate_weights(data = test_data, method = "subbootstrap",
                               cluster = "SDMVPSU", strata = "SDMVSTRA",
                               weights = "WTSAF2YR", B = 5, rw.test = TRUE)

  # Build l.yhat the same way wRandomforest() would, for fold 1 only.
  train_col <- "rw_r_1_train_1"
  test_col  <- "sw_r_1_test_1"
  expect_true(test_col %in% colnames(rw_data))  # column must exist at all post-fix

  idx_train <- which(rw_data[, train_col] > 0)
  y_fac <- as.factor(rw_data$HBP)

  rf_fold1 <- randomForest::randomForest(
    x = rw_data[idx_train, col.x], y = y_fac[idx_train],
    xtest = rw_data[-idx_train, col.x],
    weights = as.numeric(rw_data[idx_train, train_col])
  )

  l.yhat <- list()
  l.yhat[["yhat_rw_r_1_train_1"]] <- rf_fold1$test$predicted

  error_vec <- eval.error(data = rw_data, l.yhat = l.yhat, method = "subbootstrap",
                         cv.error.ind = FALSE, R = 1, k = 5, B = 5,
                         y = y_fac, objective = "binary:logistic",
                         weights = "WTSAF2YR")

  # Manually recompute fold 1's expected error using only the actual OOB
  # units (positive sw_r_1_test_1 weight), independent of eval.error()'s
  # internal logic, as ground truth.
  idx_test <- which(rw_data[, test_col] > 0)
  expected_loss <- as.numeric(y_fac[idx_test] != l.yhat[["yhat_rw_r_1_train_1"]])
  expected_error_fold1 <- sum(expected_loss * rw_data[idx_test, test_col]) / sum(rw_data[idx_test, test_col])

  expect_equal(unname(error_vec[1]), expected_error_fold1, tolerance = 1e-6)
})
