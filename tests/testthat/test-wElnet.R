# tests/testthat/test-wElnet.R
# Smoke tests for wElnet(). test_data and col.x come from helper-wmodels.R.


test_that("wElnet() runs on small synthetic data and returns expected structure", {

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  fit <- wElnet(data = test_data, col.y = "HBP", col.x = col.x,
               family = "binomial", alpha = 0.5,
               cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
               method = "dCV", k = 4, R = 1)

  expect_s3_class(fit, "w.elnet")
  expect_true(is.list(fit$lambda))
  expect_true(is.numeric(fit$lambda$grid))
  expect_true(is.numeric(fit$lambda$min))
  expect_true(is.numeric(fit$error$average))
  expect_equal(fit$alpha, 0.5)
  expect_s3_class(fit$model$final_model, "glmnet")
})

test_that("wElnet() respects a user-supplied design object", {

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  des <- survey::svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                           weights = ~WTSAF2YR, nest = TRUE, data = test_data)

  fit <- wElnet(col.y = "HBP", col.x = col.x, design = des,
               family = "binomial", alpha = 1,
               method = "dCV", k = 4, R = 1)

  expect_s3_class(fit, "w.elnet")
  expect_equal(fit$alpha, 1)
})

test_that("print.w.elnet() runs without error", {

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  fit <- wElnet(data = test_data, col.y = "HBP", col.x = col.x,
               family = "binomial", alpha = 0.5,
               cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
               method = "dCV", k = 4, R = 1)

  expect_output(print(fit), "Weighted Elastic Net")
})

test_that("wElnet() runs with method = 'JKn'", {

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  fit <- wElnet(data = test_data, col.y = "HBP", col.x = col.x,
               family = "binomial", alpha = 0.5,
               cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
               method = "JKn")

  expect_s3_class(fit, "w.elnet")
  expect_true(is.numeric(fit$lambda$min))
  expect_true(is.numeric(fit$error$average))
  expect_equal(fit$alpha, 0.5)
})

test_that("REGRESSION: wElnet() runs with standalone method = 'subbootstrap'", {
  # Prior to the fix, replicate_weights() only generated "_train_" columns
  # for standalone (non-split) bootstrap/subbootstrap -- no "_test_"
  # out-of-bag columns were produced. Since wElnet()'s per-fold training
  # loop (Step 3) only iterates over "_train_" columns and error.f()
  # expects matching "_test_" columns to compute held-out error, this
  # would previously fail. The fix generalized the existing JKn OOB
  # test-weight block in replicate_weights() to also cover bootstrap/
  # subbootstrap.

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  expect_no_error(
    fit <- wElnet(data = test_data, col.y = "HBP", col.x = col.x,
                 family = "binomial", alpha = 0.5,
                 cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                 method = "subbootstrap", B = 20)
  )

  expect_s3_class(fit, "w.elnet")
  expect_true(is.numeric(fit$lambda$min))
  expect_true(is.numeric(fit$error$average))
})

test_that("REGRESSION: error.f() computes subbootstrap error against each replicate's actual OOB holdout, not the full sample", {
  # Prior to the fix, error.f()'s weights.name for bootstrap/subbootstrap
  # was fixed at the plain full-sample weights column for every one of the
  # B replicates (it was never reconstructed per-fold, unlike JKn/dCV/BRR).
  # This did NOT error -- it silently computed each replicate's error
  # against the ENTIRE original sample's weights instead of that
  # replicate's actual out-of-bag (OOB) held-out units, producing a
  # plausible-looking but numerically wrong error value. A basic smoke
  # test (checking only that a number comes back) would not catch this;
  # this test independently recomputes the expected error from the
  # generated OOB columns and compares it directly against wElnet()'s
  # internal result$error$all.

  skip_if_not_installed("glmnet")
  skip_if_not_installed("survey")

  set.seed(42)
  fit <- wElnet(data = test_data, col.y = "HBP", col.x = col.x,
               family = "binomial", alpha = 0.5,
               cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
               method = "subbootstrap", B = 5, print.rw = TRUE)

  rw_data <- fit$data.rw
  lambda_idx <- 1  # check the first lambda in the grid as a representative case
  lambda_val <- fit$lambda$grid[lambda_idx]

  # Manually recompute fold 1's weighted error using the actual OOB test
  # column (sw_r_1_test_1), independent of wElnet()'s internal error.f()
  # call, to serve as ground truth.
  train_col <- "rw_r_1_train_1"
  test_col  <- "sw_r_1_test_1"

  expect_true(test_col %in% colnames(rw_data))  # column must exist at all post-fix

  model_fold1 <- glmnet::glmnet(
    y = as.numeric(rw_data$HBP),
    x = as.matrix(rw_data[, col.x]),
    weights = as.numeric(rw_data[, train_col]),
    lambda = fit$lambda$grid, alpha = 0.5, family = "binomial"
  )
  yhat_fold1 <- predict(model_fold1, newx = as.matrix(rw_data[, col.x]), type = "response")

  oob_weights <- rw_data[, test_col]
  oob_idx <- which(oob_weights > 0)

  # Expected: weighted binomial loss computed ONLY over OOB units, divided
  # by the sum of OOB weights -- NOT divided by the full sample's weights.
  loss_fold1 <- ifelse(
    rw_data$HBP[oob_idx] == 1,
    -log(yhat_fold1[oob_idx, lambda_idx]),
    -log(1 - yhat_fold1[oob_idx, lambda_idx])
  )
  expected_error_fold1 <- sum(loss_fold1 * oob_weights[oob_idx]) / sum(oob_weights[oob_idx])

  actual_error_fold1 <- unname(fit$error$all[1, lambda_idx])

  # Allow for small floating point / refit variation, but this should be
  # close -- under the old bug, actual_error_fold1 would instead reflect
  # loss computed over ALL units divided by the full sample's total
  # weight, which is a substantially different (and wrong) quantity.
  expect_equal(actual_error_fold1, expected_error_fold1, tolerance = 1e-6)
})
