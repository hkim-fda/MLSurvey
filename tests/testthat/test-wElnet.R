# tests/testthat/test-wElnet.R
# Smoke tests for wElnet(). Uses make_test_data() from helper-wmodels.R.

test_data <- make_test_data()
col.x <- which(names(test_data) %in% c("x1", "x2", "x3", "x4", "x5"))


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