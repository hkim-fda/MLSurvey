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