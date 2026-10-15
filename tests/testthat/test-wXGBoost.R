# tests/testthat/test-wXGBoost.R
# Smoke tests for wXGBoost(). test_data and col.x come from helper-wmodels.R.


test_that("wXGBoost() runs on small synthetic data and returns expected structure", {

  skip_if_not_installed("xgboost")
  skip_if_not_installed("survey")

  params <- list(objective = "binary:logistic", max_depth = 3,
                eta = 0.3, nthread = 1)

  fit <- wXGBoost(data = test_data, y = test_data$HBP, col.x = col.x,
                  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                  params = params, nrounds = 20, verbose = 0,
                  early_stopping_rounds = 5,
                  method = "dCV", k = 4, R = 1)

  expect_s3_class(fit, "w.xgboost")
  expect_true(is.numeric(fit$CV.iterations$best_iteration))
  expect_true(is.data.frame(fit$CV.eval_log$CV))
  expect_true(!is.null(fit$final.model))
  expect_true(!is.null(fit$predicted))
})

test_that("wXGBoost() runs with final.model = FALSE (used internally by optim_wxgb_para())", {

  skip_if_not_installed("xgboost")
  skip_if_not_installed("survey")

  params <- list(objective = "binary:logistic", max_depth = 3,
                eta = 0.3, nthread = 1)

  fit <- wXGBoost(data = test_data, y = test_data$HBP, col.x = col.x,
                  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                  params = params, nrounds = 20, verbose = 0,
                  early_stopping_rounds = 5, final.model = FALSE,
                  method = "dCV", k = 4, R = 1)

  expect_s3_class(fit, "w.xgboost")
  expect_null(fit$final.model)
})

test_that("wXGBoost() runs with method = 'JKn'", {

  skip_if_not_installed("xgboost")
  skip_if_not_installed("survey")

  params <- list(objective = "binary:logistic", max_depth = 3,
                eta = 0.3, nthread = 1)

  fit <- wXGBoost(data = test_data, y = test_data$HBP, col.x = col.x,
                  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                  params = params, nrounds = 20, verbose = 0,
                  early_stopping_rounds = 5,
                  method = "JKn")

  expect_s3_class(fit, "w.xgboost")
  expect_true(is.numeric(fit$CV.iterations$best_iteration))
  expect_true(!is.null(fit$final.model))

  # REGRESSION: fold.best (renamed from k_fold.best) must be a list whose
  # per-replicate vector length matches the actual number of folds
  # produced for JKn (number of PSUs), not the 'k' argument. Previously
  # this was a matrix pre-sized nrow = k (default 10), which errored with
  # "subscript out of bounds" whenever the real fold count exceeded k, as
  # is typical for JKn.
  expect_type(fit$CV.iterations$fold.best, "list")
  expect_length(fit$CV.iterations$fold.best, 1)  # R = 1 by default
  n_psus <- length(unique(interaction(test_data$SDMVSTRA, test_data$SDMVPSU, drop = TRUE)))
  expect_length(fit$CV.iterations$fold.best[[1]], n_psus)
})

test_that("REGRESSION: wXGBoost() runs with standalone method = 'bootstrap'", {
  # Prior to the fix, this failed in two ways:
  # (1) replicate_weights() did not generate "_test_" OOB columns for
  #     standalone bootstrap/subbootstrap, causing wXGBoost()'s dtest
  #     construction to fail on an empty rwtestcols.
  # (2) Even after (1) was fixed, fold.best (then k_fold.best) was a
  #     matrix pre-sized nrow = k (default 10), which errored with
  #     "subscript out of bounds" for any B > 10 (bootstrap's default is
  #     B = 200).

  skip_if_not_installed("xgboost")
  skip_if_not_installed("survey")

  params <- list(objective = "binary:logistic", max_depth = 3,
                eta = 0.3, nthread = 1)

  expect_no_error(
    fit <- wXGBoost(data = test_data, y = test_data$HBP, col.x = col.x,
                    cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                    params = params, nrounds = 20, verbose = 0,
                    early_stopping_rounds = 5,
                    method = "bootstrap", B = 20)
  )

  expect_s3_class(fit, "w.xgboost")
  expect_true(is.numeric(fit$CV.iterations$best_iteration))
  expect_type(fit$CV.iterations$fold.best, "list")
  expect_length(fit$CV.iterations$fold.best[[1]], 20)  # B = 20
})

test_that("REGRESSION: wXGBoost() final model no longer errors on empty evals when test.data is NULL", {
  # Prior to the fix, final.model's eval.list was set to an empty list()
  # whenever test.data was NULL, while early_stopping_rounds was still
  # passed through unchanged to the final xgb.train() call. xgboost's
  # cb.early.stop callback requires at least one element in 'evals', so
  # this combination previously errored with:
  #   "For early stopping, 'evals' must have at least one element"
  # The fix: (1) eval.list always includes at least 'train', and
  # (2) early_stopping_rounds is forced to NULL for the final fit, since
  # nrounds = best_iter was already selected by CV and there is nothing
  # left to stop early on.

  skip_if_not_installed("xgboost")
  skip_if_not_installed("survey")

  params <- list(objective = "binary:logistic", max_depth = 3,
                eta = 0.3, nthread = 1)

  expect_no_error(
    fit <- wXGBoost(data = test_data, y = test_data$HBP, col.x = col.x,
                    cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
                    params = params, nrounds = 20, verbose = 0,
                    early_stopping_rounds = 5,
                    test.data = NULL,          # <-- the exact condition that used to fail
                    method = "dCV", k = 4, R = 1)
  )

  expect_s3_class(fit, "w.xgboost")
  expect_true(!is.null(fit$final.model))
  expect_s3_class(fit$final.model, "xgb.Booster")
  expect_true(is.numeric(fit$predicted))
  expect_equal(length(fit$predicted), nrow(test_data))

  # Confirm the final fit actually trained to completion (best_iter rounds)
  # rather than being cut short by an unintended early stop. We check this
  # via the booster's evaluation_log attribute rather than a direct $niter
  # accessor, since the internal structure of xgb.Booster objects is not
  # guaranteed stable across xgboost package versions.
  eval_log <- attr(fit$final.model, "evaluation_log")
  if(!is.null(eval_log)){
    expect_equal(nrow(eval_log), fit$CV.iterations$best_iteration)
  }
})
