#' Plot method for \code{par.w.elnet} objects
#'
#' @description A plot method to visualize the results of [par_wElnet()]: the weighted
#'              error curve across the lambda grid for each alpha, and/or a
#'              summary of the best (minimum) error achieved by each alpha,
#'              with the overall best (alpha, lambda) combination highlighted.
#'
#'              For the single-alpha output of \code{\link{wElnet}} itself, see
#'              [plot.w.elnet()].
#'
#' @param x An object of class \code{par.w.elnet}, the output of [par_wElnet()].
#' @param type A string indicating what to plot. One of:
#' - \code{"profile"}: error vs. \code{log(lambda)}, one curve per alpha (base plot,
#'   similar in spirit to \code{glmnet}'s own CV plot, but overlaying every alpha
#'   on a single panel so they can be compared directly).
#' - \code{"summary"}: minimum error achieved vs. alpha, one point per alpha,
#'   with the overall best alpha highlighted.
#' - \code{"both"} (default): both plots side by side.
#' @param log.lambda A logical value. If \code{TRUE} (default), the x-axis of the
#'                    \code{"profile"} plot is \code{log(lambda)}, matching \code{glmnet}
#'                    convention. If \code{FALSE}, lambda is plotted on its original scale.
#' @param legend.pos A string passed to \code{legend()} for the \code{"profile"} panel.
#'                    Default is \code{"topright"}. Set to \code{NULL} to suppress the legend
#'                    (useful when there are many alphas and the legend gets crowded).
#' @param palette A vector of colors to cycle through for the different alphas in the
#'                \code{"profile"} plot. Defaults to \code{grDevices::hcl.colors(n, "Zissou 1")},
#'                where \code{n} is the number of alphas.
#' @param main.profile,xlab.profile,ylab.profile Optional strings overriding the title
#'                and axis labels of the \code{"profile"} panel. If \code{NULL} (default),
#'                sensible defaults are used ("Error profile by alpha", "log(lambda)" or
#'                "lambda", and "Weighted error").
#' @param main.summary,xlab.summary,ylab.summary Optional strings overriding the title
#'                and axis labels of the \code{"summary"} panel. If \code{NULL} (default),
#'                sensible defaults are used ("Best error by alpha", "alpha", and
#'                "Minimum weighted error").
#' @param ... Additional graphical parameters (e.g. \code{col}, \code{pch}, \code{cex},
#'            \code{lwd}) passed to the underlying \code{plot()} calls. Do not pass
#'            \code{main}, \code{xlab}, or \code{ylab} here -- use the dedicated arguments
#'            above instead.
#'
#' @return Invisibly returns \code{x}. Called for its side effect of producing a plot.
#'
#' @examples
#' \dontrun{
#' library(MLSurvey)
#' data(nhanes2013_sbc)
#' alpha <- c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000)
#'
#' en.par <- par_wElnet(alpha = alpha,
#'                       data = nhanes2013_sbc,
#'                       col.y = "HBP", col.x = 2:61,
#'                       family = "binomial",
#'                       cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'                       method = "dCV", k = 10, R = 20)
#'
#' plot(en.par)                    # both panels
#' plot(en.par, type = "profile")  # just the error-vs-lambda curves
#' plot(en.par, type = "summary")  # just the best-error-vs-alpha curve
#' plot(en.par, type = "profile", main.profile = "My custom title", xlab.profile = "Penalty")
#' }
#'
#' @export
plot.par.w.elnet <- function(x, type = c("both", "profile", "summary"),
                             log.lambda = TRUE, legend.pos = "topright",
                             palette = NULL,
                             main.profile = NULL, xlab.profile = NULL, ylab.profile = NULL,
                             main.summary = NULL, xlab.summary = NULL, ylab.summary = NULL,
                             ...){

  type <- match.arg(type)

  if(!inherits(x, "par.w.elnet")){
    stop("'x' must be an object of class 'par.w.elnet', the output of par_wElnet().")
  }

  alphas   <- x$summary$alpha
  n.alpha  <- length(alphas)

  if(is.null(palette)){
    palette <- grDevices::hcl.colors(n.alpha, "Zissou 1")
  }
  palette <- rep(palette, length.out = n.alpha)

  best.idx   <- which.min(x$summary$min.error)
  best.alpha <- x$summary$alpha[best.idx]
  best.lambda<- x$summary$lambda.min[best.idx]
  best.error <- x$summary$min.error[best.idx]

  # --- Layout for "both" ---------------------------------------------------
  if(type == "both"){
    old.par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old.par), add = TRUE)
  }

  # --- Panel 1: error profile across the lambda grid, one curve per alpha --
  if(type %in% c("both", "profile")){

    lambda.list <- lapply(x$results, function(r) r$lambda$grid)
    error.list  <- lapply(x$results, function(r) r$error$average)

    x.list <- if(log.lambda) lapply(lambda.list, log) else lambda.list

    xlim <- range(unlist(x.list), na.rm = TRUE)
    ylim <- range(unlist(error.list), na.rm = TRUE)

    xlab.p <- if(!is.null(xlab.profile)) xlab.profile else if(log.lambda) "log(lambda)" else "lambda"
    ylab.p <- if(!is.null(ylab.profile)) ylab.profile else "Weighted error"
    main.p <- if(!is.null(main.profile)) main.profile else "Error profile by alpha"

    graphics::plot(NA, xlim = xlim, ylim = ylim,
                  xlab = xlab.p, ylab = ylab.p, main = main.p, ...)

    for(i in seq_len(n.alpha)){
      graphics::lines(x.list[[i]], error.list[[i]], col = palette[i], lwd = 2)
    }

    # Mark the overall best (alpha, lambda) point
    best.x <- if(log.lambda) log(best.lambda) else best.lambda
    graphics::points(best.x, best.error, pch = 8, cex = 1.5, lwd = 2, col = "black")
    graphics::abline(v = best.x, lty = 2, col = "grey50")

    if(!is.null(legend.pos)){
      graphics::legend(legend.pos,
                       legend = c(paste0("alpha=", signif(alphas, 3)), "best"),
                       col = c(palette, "black"),
                       lwd = c(rep(2, n.alpha), NA),
                       pch = c(rep(NA, n.alpha), 8),
                       bty = "n", cex = 0.75)
    }
  }

  # --- Panel 2: minimum error achieved, per alpha --------------------------
  if(type %in% c("both", "summary")){

    ord <- order(x$summary$alpha)
    a.ord <- x$summary$alpha[ord]
    e.ord <- x$summary$min.error[ord]

    xlab.s <- if(!is.null(xlab.summary)) xlab.summary else "alpha"
    ylab.s <- if(!is.null(ylab.summary)) ylab.summary else "Minimum weighted error"
    main.s <- if(!is.null(main.summary)) main.summary else "Best error by alpha"

    graphics::plot(a.ord, e.ord, type = "b", pch = 16, lwd = 2,
                  xlab = xlab.s, ylab = ylab.s, main = main.s, ...)

    graphics::points(best.alpha, best.error, pch = 8, cex = 1.8, lwd = 2, col = "red")
    graphics::abline(v = best.alpha, lty = 2, col = "grey50")

    graphics::legend("topright",
                     legend = paste0("best: alpha=", signif(best.alpha, 3),
                                     ", lambda=", signif(best.lambda, 3)),
                     pch = 8, col = "red", bty = "n", cex = 0.8)
  }

  invisible(x)
}


#' Plot method for \code{w.elnet} objects (single-alpha \code{wElnet()} output)
#'
#' @description A method to visualize the weighted error curve across the lambda grid for
#'              a single \code{\link{wElnet}} fit (i.e. a single, fixed alpha), marking
#'              the selected \code{lambda.min}. This is the single-alpha counterpart to
#'              [plot.par.w.elnet()], which handles the multi-alpha output of [par_wElnet()].
#'
#' @param x An object of class \code{w.elnet}, the output of \code{\link{wElnet}}.
#' @param log.lambda A logical value. If \code{TRUE} (default), the x-axis is
#'                    \code{log(lambda)}, matching \code{glmnet} convention. If \code{FALSE},
#'                    lambda is plotted on its original scale.
#' @param main,xlab,ylab Optional strings overriding the plot title and axis labels. If
#'                \code{NULL} (default), sensible defaults are used ("Error profile
#'                (alpha = ...)", "log(lambda)" or "lambda", and "Weighted error").
#' @param ... Additional graphical parameters (e.g. \code{col}, \code{lwd}, \code{lty})
#'            passed to the underlying \code{plot()} call. Do not pass \code{main},
#'            \code{xlab}, or \code{ylab} here -- use the dedicated arguments above instead.
#'
#' @return Invisibly returns \code{x}. Called for its side effect of producing a plot.
#'
#' @examples
#' \dontrun{
#' library(MLSurvey)
#' data(nhanes2013_sbc)
#'
#' en.dcv <- wElnet(data = nhanes2013_sbc,
#'                  col.y = "HBP", col.x = 2:61,
#'                  family = "binomial", alpha = 0.729,
#'                  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'                  method = "dCV", k = 10, R = 20)
#'
#' plot(en.dcv)
#' plot(en.dcv, main = "My custom title", xlab = "Penalty", ylab = "Error")
#' }
#'
#' @export
plot.w.elnet <- function(x, log.lambda = TRUE,
                         main = NULL, xlab = NULL, ylab = NULL, ...){

  if(!inherits(x, "w.elnet")){
    stop("'x' must be an object of class 'w.elnet', the output of wElnet().")
  }

  lambda <- x$lambda$grid
  error  <- x$error$average
  x.vals <- if(log.lambda) log(lambda) else lambda
  best.x <- if(log.lambda) log(x$lambda$min) else x$lambda$min
  best.error <- error[which.min(abs(lambda - x$lambda$min))]

  # alpha is stored directly on the w.elnet object (added in wElnet()).
  main.default <- paste0("Error profile (alpha = ", signif(x$alpha, 3), ")")

  main.txt <- if(!is.null(main)) main else main.default
  xlab.txt <- if(!is.null(xlab)) xlab else if(log.lambda) "log(lambda)" else "lambda"
  ylab.txt <- if(!is.null(ylab)) ylab else "Weighted error"

  graphics::plot(x.vals, error, type = "l", lwd = 2,
                xlab = xlab.txt, ylab = ylab.txt, main = main.txt, ...)

  graphics::points(best.x, best.error, pch = 8, cex = 1.5, lwd = 2, col = "red")
  graphics::abline(v = best.x, lty = 2, col = "grey50")

  graphics::legend("topright",
                   legend = paste0("lambda.min = ", signif(x$lambda$min, 3)),
                   pch = 8, col = "red", bty = "n", cex = 0.8)

  invisible(x)
}


