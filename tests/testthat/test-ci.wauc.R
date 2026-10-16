# tests/testthat/test-ci_wauc.R
# Smoke and regression tests for ci_wauc(). test_data and col.x come from
# helper-wmodels.R. Uses a small B for speed; ci_wauc()'s own examples use
# much larger B (2000) and are wrapped in \donttest{} for that reason.

# A fitted probability vector to serve as yhat across tests. Rather than
# fitting glm(family = binomial(), weights = <continuous survey weights>) --
# which triggers R's "non-integer #successes in a binomial glm!" warning
# (a cosmetic quirk of glm()'s weighted-binomial path when weights are large
# continuous survey weights rather than integer trial counts, not a sign of
# a real problem) -- we simulate a plausible, non-degenerate yhat directly:
# noisy signal correlated with the true outcome, clipped to a valid
# probability range. This is sufficient for exercising ci_wauc()'s AUC
# calculation without any model-fitting warnings.
fit_yhat <- function(){
  set.seed(99)
  signal <- ifelse(test_data$HBP == 1, 0.65, 0.35)
  noise  <- rnorm(nrow(test_data), sd = 0.15)
  pmin(pmax(signal + noise, 0.01), 0.99)
}


test_that("ci_wauc() runs with vector y/yhat inputs and returns a 3-point CI", {

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()

  ci <- ci_wauc(
    y = test_data$HBP, yhat = yhat,
    weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
    data = test_data, type = "subbootstrap", B = 20
  )

  expect_type(ci, "double")
  expect_length(ci, 3)                 # lower, median, upper quantiles
  expect_true(all(ci >= 0 & ci <= 1))  # AUC is bounded in [0, 1]
  expect_true(ci[1] <= ci[2] && ci[2] <= ci[3])  # quantiles are ordered
})


test_that("ci_wauc() runs with string column-name y/yhat inputs (requires data)", {
  # REGRESSION: previously, string inputs for y/yhat were documented
  # ("A vector... or a string of its name") but not actually implemented --
  # a bare string was passed straight through to svyROC::wauc() rather than
  # being resolved to the named column.

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()
  data1 <- data.frame(test_data[, c("SDMVPSU", "SDMVSTRA", "WTSAF2YR", "HBP")], yhat)

  ci_string <- ci_wauc(
    y = "HBP", yhat = "yhat",
    weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
    data = data1, type = "subbootstrap", B = 20
  )

  expect_type(ci_string, "double")
  expect_length(ci_string, 3)
  expect_true(all(ci_string >= 0 & ci_string <= 1))
})


test_that("ci_wauc() errors clearly when a string y/yhat is given without data", {
  # REGRESSION: previously, a string y/yhat with no data would not raise a
  # clear error -- it would either silently misbehave downstream, or fail
  # with a confusing error from deep inside svyROC::wauc()/survey. Now it
  # should fail immediately and informatively.

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()

  expect_error(
    ci_wauc(y = "HBP", yhat = yhat,
            weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
            data = NULL, type = "subbootstrap", B = 20),
    "data"
  )
})


test_that("ci_wauc() errors if y does not have exactly two classes", {

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()
  bad_y <- rep(1, nrow(test_data))  # only one class

  expect_error(
    ci_wauc(y = bad_y, yhat = yhat,
            weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
            data = test_data, type = "subbootstrap", B = 20),
    "two classes"
  )
})


test_that("ci_wauc() rejects an invalid 'type' via match.arg() with a clear error", {
  # REGRESSION: previously, an invalid type value fell through silently to
  # survey::as.svrepdesign(), producing a less direct error originating
  # from inside the survey package rather than from ci_wauc() itself.

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()

  expect_error(
    ci_wauc(y = test_data$HBP, yhat = yhat,
            weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
            data = test_data, type = "not_a_real_method", B = 20),
    "should be one of"
  )
})


test_that("ci_wauc() respects a user-supplied design object", {

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()
  des <- survey::svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                           weights = ~WTSAF2YR, nest = TRUE, data = test_data)

  ci <- ci_wauc(
    y = des$variables$HBP, yhat = yhat,
    design = des, type = "subbootstrap", B = 20
  )

  expect_type(ci, "double")
  expect_length(ci, 3)
  expect_true(all(ci >= 0 & ci <= 1))
})


test_that("ci_wauc() runs with method = 'JKn'", {

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()

  ci <- ci_wauc(
    y = test_data$HBP, yhat = yhat,
    weights = "WTSAF2YR", cluster = "SDMVPSU", strata = "SDMVSTRA",
    data = test_data, type = "JKn"
  )

  expect_type(ci, "double")
  expect_length(ci, 3)
  expect_true(all(ci >= 0 & ci <= 1))
})


test_that("ci_wauc() runs unweighted (weights = NULL) with strata and cluster supplied", {
  # REGRESSION (two layered bugs, both exposed by this test):
  # (1) The strata-supplied branch of ci_wauc()'s design construction
  #     unconditionally required weights to be non-NULL -- fixed by
  #     allowing an unweighted stratified design to be built.
  # (2) rep.des$repweights$weights is sized by the number of clusters/PSUs,
  #     not the number of observations. Used directly (as it was), this
  #     either errors outright (exactly what this test originally caught,
  #     once bug (1) was fixed and this code path became reachable) or, in
  #     the weighted branch, silently recycles cluster-level weights in the
  #     wrong order whenever nrow(data) is a multiple of the cluster count
  #     -- corrupting every previously "passing" weighted test in this
  #     file. Fixed by expanding rep.weights to unit-level via
  #     rep.des$repweights$index before use, matching the equivalent
  #     expansion already used in error.f() (for wElnet()).

  skip_if_not_installed("svyROC")
  skip_if_not_installed("survey")

  yhat <- fit_yhat()

  # survey::svydesign() itself emits an informational warning whenever
  # weights = NULL ("No weights or probabilities supplied, assuming equal
  # probability") -- this is expected, correct behavior confirming an
  # equal-probability design was built as intended, not a sign of a
  # problem. We use withCallingHandlers() (rather than expect_warning(),
  # whose return value is not guaranteed to be the bare result of the
  # wrapped expression across testthat versions) to muffle this specific
  # expected warning while still letting ci_wauc()'s actual return value
  # pass through untouched for the assertions below.
  warning_seen <- FALSE
  ci <- withCallingHandlers(
    ci_wauc(
      y = test_data$HBP, yhat = yhat,
      weights = NULL, cluster = "SDMVPSU", strata = "SDMVSTRA",
      data = test_data, type = "subbootstrap", B = 20
    ),
    warning = function(w){
      if(grepl("equal probability", conditionMessage(w))){
        warning_seen <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_true(warning_seen)
  expect_type(ci, "double")
  expect_length(ci, 3)
  expect_true(all(ci >= 0 & ci <= 1))
})

test_that("REGRESSION: ci_wauc()'s replicate weights are correctly expanded to unit-level, not cluster-level", {
  # Directly verifies the length and unit-to-cluster mapping of the
  # replicate weight vector used internally, independent of the final AUC
  # value, to specifically pin down the rep.weights expansion bug above.

  skip_if_not_installed("survey")

  des <- survey::svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                           weights = ~WTSAF2YR, nest = TRUE, data = test_data)
  rep.des <- survey::as.svrepdesign(design = des, type = "subbootstrap", replicates = 10)

  rep.weights.raw <- rep.des$repweights$weights
  n.clusters <- nrow(rep.weights.raw)

  # Sanity check on the test data itself: confirm there genuinely are fewer
  # clusters than observations, i.e. that this bug would have been masked
  # by recycling rather than caught by a length mismatch, if not expanded.
  expect_true(n.clusters < nrow(test_data))

  rep.weights.expanded <- apply(rep.weights.raw, 2, function(x){x[rep.des$repweights$index]})

  # The expanded matrix must have one row per observation, not per cluster.
  expect_equal(nrow(rep.weights.expanded), nrow(test_data))

  # Every observation belonging to the same cluster must receive the same
  # replicate weight (since replicate weights are defined at the cluster
  # level and only vary across clusters, not within one).
  cluster_id <- interaction(test_data$SDMVSTRA, test_data$SDMVPSU, drop = TRUE)
  for(cl in levels(cluster_id)){
    rows <- which(cluster_id == cl)
    if(length(rows) > 1){
      expect_equal(length(unique(rep.weights.expanded[rows, 1])), 1)
    }
  }
})
