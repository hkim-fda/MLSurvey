#' Parallel cross-validated Weighted Elastic Net across a sequence of alpha values
#'
#' @description Fits [wElnet()] once per value of \code{alpha}, assigning each
#'              alpha to its own CPU core via the \code{parallel} package, and
#'              then selects the overall best (alpha, lambda) combination by
#'              minimizing the average cross-validated error across all fits.
#'
#'              This function always uses a \code{"PSOCK"} cluster, on every
#'              platform (Windows, Mac, Linux), because it is designed to run
#'              safely inside RStudio. RStudio manages a graphics device and
#'              other GUI state in the main R process; forking that process
#'              (\code{"FORK"} clusters) is explicitly discouraged by R's own
#'              \code{parallel} documentation in GUI-based sessions and can
#'              crash the session. \code{"PSOCK"} workers are fresh, independent
#'              R processes, so this risk does not apply, at the cost of a
#'              small amount of serialization overhead when the cluster starts.
#'
#' @param alpha A numeric vector of elastic net mixing parameters to evaluate
#'              in parallel, one per core. Default is the standard 11-point
#'              grid \code{c(0, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216,
#'              0.343, 0.512, 0.729, 1)}.
#' @param n.cores Integer number of CPU cores to use. Defaults to
#'                \code{min(parallel::detectCores() - 1, length(alpha))}.
#' @param blas.threads Integer number of threads each worker's BLAS library is
#'                      allowed to use. Defaults to \code{1}. This is unrelated
#'                      to the FORK/PSOCK issue above, but is included as a
#'                      safe default: nested parallelism (multiple worker
#'                      processes each also spawning multithreaded BLAS calls
#'                      inside \code{glmnet}) can oversubscribe CPU cores and
#'                      slow everything down. Requires the \code{RhpcBLASctl}
#'                      package; if it is not installed, this is silently
#'                      skipped with a message.
#' @param ... All other arguments accepted by [wElnet()] (\code{data}, \code{col.y},
#'            \code{col.x}, \code{cluster}, \code{strata}, \code{weights}, \code{design},
#'            \code{family}, \code{lambda}, \code{nlambda}, \code{lambda.min.ratio},
#'            \code{method}, \code{k}, \code{R}, \code{B}, \code{dCV.sw.test},
#'            \code{train.prob}, \code{method.split}, \code{print.rw},
#'            \code{standardize}, \code{offset}).
#'
#' @return An object of class \code{cv.w.elnet}, a list with:
#' - `results`: a named list of \code{w.elnet} objects (the output of [wElnet()]), one per alpha.
#' - `summary`: a data frame with columns \code{alpha}, \code{lambda.min}, and \code{min.error},
#'              one row per alpha, sorted in the order \code{alpha} was supplied.
#' - `best`: the single \code{w.elnet} object with the lowest \code{min.error} across all alphas.
#' - `best.alpha`: the alpha value corresponding to \code{best}.
#'
#' @seealso [wElnet()] for the single-alpha model fit that this function parallelizes.
#'
#' @examples
#' \dontrun{
#' data(nhanes2013_sbc)
#' alpha <- c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000)
#'
#' cv.en <- cv.wElnet(alpha = alpha,
#'                     data = nhanes2013_sbc,
#'                     col.y = "HBP", col.x = 2:61,
#'                     family = "binomial",
#'                     cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'                     method = "dCV", k = 10, R = 20)
#'
#' cv.en$summary      # error per alpha
#' cv.en$best.alpha    # winning alpha
#' cv.en$best          # winning w.elnet object (same structure as wElnet() output)
#' }
#'
#' @export
cv.wElnet <- function(alpha = c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216,
                                 0.343, 0.512, 0.729, 1.000),
                       n.cores = NULL,
                       blas.threads = 1,
                       data = NULL, col.y = NULL, col.x = NULL,
                       cluster = NULL, strata = NULL, weights = NULL, design = NULL,
                       family = c("gaussian", "binomial"),
                       lambda = NULL,
                       nlambda = 100,
                       lambda.min.ratio = NULL,
                       method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
                       k = 10, R = 1, B = 200,
                       dCV.sw.test = FALSE,
                       train.prob = 0.7, method.split = c("dCV", "bootstrap", "subbootstrap"),
                       print.rw = FALSE, standardize = TRUE, offset = NULL, ...){

  family      <- match.arg(family)
  method      <- match.arg(method)
  method.split<- match.arg(method.split)

  if(is.null(data) & is.null(design)){
    stop("Information about either the data set ('data') or the sampling design ('design') needed.")
  }
  if(length(alpha) < 1){
    stop("'alpha' must contain at least one value.")
  }

  n.alpha <- length(alpha)

  if(is.null(n.cores)){
    n.cores <- max(1, min(parallel::detectCores() - 1, n.alpha))
  }
  n.cores <- min(n.cores, n.alpha)  # never spin up more workers than alphas

  # --- Always PSOCK, on every platform -----------------------------------
  # Safe to run inside RStudio on Windows, Mac, and Linux alike: PSOCK
  # workers are independent fresh R processes, so forking the RStudio
  # session's GUI/graphics-device state (a risk with "FORK" clusters) never
  # comes into play.
  cat("Fitting", n.alpha, "alpha value(s) across", n.cores,
      "CPU core(s) using a PSOCK cluster (safe for RStudio on all platforms)...\n")

  cl <- parallel::makeCluster(n.cores, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Ship the needed functions to each fresh worker process, and load both
  # glmnet (used directly by wElnet()) and survey (used internally by
  # replicate_weights()) on every worker, regardless of whether the caller
  # supplied a `design` object or plain data/cluster/strata/weights.
  parallel::clusterExport(cl,
                          varlist = c("wElnet", "replicate_weights", "error.f"),
                          envir = environment())
  parallel::clusterEvalQ(cl, { library(glmnet); library(survey); TRUE })

  # Cap each worker's BLAS threads to avoid oversubscribing cores when
  # n.cores workers are each also trying to run multithreaded linear algebra
  # inside glmnet. Skipped silently if RhpcBLASctl isn't installed.
  if(requireNamespace("RhpcBLASctl", quietly = TRUE)){
    parallel::clusterExport(cl, varlist = "blas.threads", envir = environment())
    parallel::clusterEvalQ(cl, {
      RhpcBLASctl::blas_set_num_threads(blas.threads)
      TRUE
    })
  } else {
    message("Package 'RhpcBLASctl' not installed; skipping per-worker BLAS thread limiting. ",
            "Install it with install.packages('RhpcBLASctl') to avoid potential CPU oversubscription.")
  }

  # --- One wElnet() call per alpha, run on its own core -----------------
  fit_one_alpha <- function(a){

    call.args <- list(data = data, col.y = col.y, col.x = col.x,
                      cluster = cluster, strata = strata, weights = weights,
                      design = design, family = family, lambda = lambda,
                      alpha = a, nlambda = nlambda,
                      method = method, k = k, R = R, B = B,
                      dCV.sw.test = dCV.sw.test, train.prob = train.prob,
                      method.split = method.split, print.rw = print.rw,
                      standardize = standardize, offset = offset)

    # Only pass lambda.min.ratio through if the user actually set it;
    # otherwise let wElnet() apply its own internal default.
    if(!is.null(lambda.min.ratio)){
      call.args$lambda.min.ratio <- lambda.min.ratio
    }

    extra.args <- list(...)
    call.args <- c(call.args, extra.args)

    do.call(wElnet, call.args)
  }

  results <- parallel::parLapply(cl, alpha, fit_one_alpha)
  names(results) <- paste0("alpha_", alpha)

  # --- Summarize across alphas -------------------------------------------
  summary.df <- data.frame(
    alpha      = alpha,
    lambda.min = vapply(results, function(r) r$lambda$min, numeric(1)),
    min.error  = vapply(results, function(r) min(r$error$average), numeric(1))
  )

  best.idx    <- which.min(summary.df$min.error)
  best.result <- results[[best.idx]]

  cat("Best alpha:", alpha[best.idx],
      "| lambda.min =", signif(summary.df$lambda.min[best.idx], 4),
      "| average error =", signif(summary.df$min.error[best.idx], 4), "\n")

  out <- list(
    results    = results,
    summary    = summary.df,
    best       = best.result,
    best.alpha = alpha[best.idx]
  )
  class(out) <- "cv.w.elnet"

  return(out)
}

#' @export
print.cv.w.elnet <- function(x, ...){
  cat("Cross-validated Weighted Elastic Net over", nrow(x$summary), "alpha value(s)\n\n")
  print(x$summary[order(x$summary$alpha), ])
  cat("\nBest alpha:", x$best.alpha, "\n")
  invisible(x)
}
