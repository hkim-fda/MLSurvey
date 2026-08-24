#' ggplot2 plotting method for \code{par.w.elnet} objects
#'
#' @description A \code{ggplot2}-based counterpart to [plot.par.w.elnet()] which visualizes
#'              the results of [par_wElnet()]: the weighted error curve across
#'              the lambda grid for each alpha, and/or a summary of the best (minimum)
#'              error achieved by each alpha, with the overall best (alpha, lambda)
#'              combination highlighted.
#'
#'              Registered as an \code{\link[ggplot2]{autoplot}} method, so it dispatches
#'              automatically via \code{autoplot(en.par)} once \code{ggplot2} is loaded,
#'              without masking the base-\code{R} \code{plot()} method in [plot.par.w.elnet()].
#'
#' @param object An object of class \code{par.w.elnet}, the output of [par_wElnet()].
#' @param type A string indicating what to plot. One of:
#' - \code{"profile"}: error vs. \code{log(lambda)}, one curve per alpha, faceted or
#'   overlaid depending on \code{facet}.
#' - \code{"summary"}: minimum error achieved vs. alpha, one point per alpha, with the
#'   overall best alpha highlighted.
#' - \code{"both"} (default): both plots combined into one figure (requires \code{patchwork};
#'   see Details).
#' @param log.lambda A logical value. If \code{TRUE} (default), the x-axis of the
#'                    \code{"profile"} plot is \code{log(lambda)}, matching \code{glmnet}
#'                    convention. If \code{FALSE}, lambda is plotted on its original scale.
#' @param facet A logical value. If \code{FALSE} (default), all alphas are overlaid on a
#'              single panel in the \code{"profile"} plot, colored by alpha. If \code{TRUE},
#'              each alpha gets its own facet panel (useful when there are many alphas and
#'              overlaid curves are hard to distinguish).
#' @param palette A string naming a \code{viridisLite}/\code{viridis}-style option passed to
#'                \code{ggplot2::scale_color_viridis_d()}, or \code{NULL} to use ggplot2's
#'                default discrete color scale. Default is \code{"viridis"}.
#' @param origin A logical value. If \code{TRUE} (default), solid black reference lines are
#'               drawn at x=0 and y=0, and the plot's coordinate range is expanded if needed
#'               so the origin is always visible. If \code{FALSE}, no origin lines are drawn
#'               and the axes are scaled to the data range as usual.
#' @param main.profile,xlab.profile,ylab.profile Optional strings overriding the title
#'                and axis labels of the \code{"profile"} panel. If \code{NULL} (default),
#'                sensible defaults are used ("Error profile by alpha", "log(lambda)" or
#'                "lambda", and "Weighted error").
#' @param main.summary,xlab.summary,ylab.summary Optional strings overriding the title
#'                and axis labels of the \code{"summary"} panel. If \code{NULL} (default),
#'                sensible defaults are used ("Best error by alpha", "alpha", and
#'                "Minimum weighted error").
#' @param ... Currently unused; included for S3 consistency with \code{autoplot()}.
#'
#' @details
#' When \code{type = "both"}, the two panels are combined side by side using the
#' \code{patchwork} package. If \code{patchwork} is not installed, \code{autoplot()} falls
#' back to returning a list of the two individual \code{ggplot} objects (\code{$profile} and
#' \code{$summary}) instead of a single combined plot, along with a message explaining how to
#' get the combined figure.
#'
#' Since the return value(s) are ordinary \code{ggplot} objects, further customization beyond
#' the \code{main.*}/\code{xlab.*}/\code{ylab.*} arguments (e.g. themes, color scales, facet
#' arguments) can always be added afterward with \code{+}, e.g.
#' \code{autoplot(en.par, type = "profile") + ggplot2::theme_bw()}.
#'
#' @return If \code{type} is \code{"profile"} or \code{"summary"}, a single \code{ggplot}
#'         object. If \code{type = "both"} and \code{patchwork} is installed, a combined
#'         \code{patchwork} object. If \code{type = "both"} and \code{patchwork} is not
#'         installed, a named list of two \code{ggplot} objects: \code{$profile} and \code{$summary}.
#'
#' @examples
#' \dontrun{
#' library(MLSurvey)
#' library(ggplot2)
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
#' autoplot(en.par)                       # both panels (needs patchwork)
#' autoplot(en.par, type = "profile")     # error-vs-lambda, overlaid by alpha
#' autoplot(en.par, type = "profile", facet = TRUE)  # one panel per alpha
#' autoplot(en.par, type = "summary")     # best-error-vs-alpha
#' autoplot(en.par, type = "profile", main.profile = "My custom title", xlab.profile = "Penalty")
#' }
#'
#' @export
autoplot.par.w.elnet <- function(object, type = c("both", "profile", "summary"),
                                 log.lambda = TRUE, facet = FALSE,
                                 palette = "viridis", origin = TRUE,
                                 main.profile = NULL, xlab.profile = NULL, ylab.profile = NULL,
                                 main.summary = NULL, xlab.summary = NULL, ylab.summary = NULL,
                                 ...){

  type <- match.arg(type)

  if(!inherits(object, "par.w.elnet")){
    stop("'object' must be an object of class 'par.w.elnet', the output of par_wElnet().")
  }

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package 'ggplot2' is required for autoplot(). Install it with install.packages('ggplot2').")
  }

  best.idx    <- which.min(object$summary$min.error)
  best.alpha  <- object$summary$alpha[best.idx]
  best.lambda <- object$summary$lambda.min[best.idx]
  best.error  <- object$summary$min.error[best.idx]

  # --- Build long-format data frame for the profile plot -------------------
  build_profile_df <- function(){
    df.list <- lapply(seq_along(object$results), function(i){
      r <- object$results[[i]]
      data.frame(
        alpha  = object$summary$alpha[i],
        lambda = r$lambda$grid,
        error  = r$error$average
      )
    })
    do.call(rbind, df.list)
  }

  # --- Panel 1: error profile across the lambda grid, by alpha -------------
  make_profile_plot <- function(){

    df <- build_profile_df()
    df$alpha.f <- factor(df$alpha)
    df$x <- if(log.lambda) log(df$lambda) else df$lambda
    best.x <- if(log.lambda) log(best.lambda) else best.lambda

    xlab.p <- if(!is.null(xlab.profile)) xlab.profile else if(log.lambda) "log(lambda)" else "lambda"
    ylab.p <- if(!is.null(ylab.profile)) ylab.profile else "Weighted error"
    main.p <- if(!is.null(main.profile)) main.profile else "Error profile by alpha"

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$error,
                                          color = .data$alpha.f, group = .data$alpha.f))

    if(origin){
      p <- p +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.4)
    }

    p <- p +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_vline(xintercept = best.x, linetype = "dashed", color = "grey40") +
      ggplot2::annotate("point", x = best.x, y = best.error,
                        shape = 8, size = 3, color = "black") +
      ggplot2::labs(title = main.p, x = xlab.p, y = ylab.p, color = "alpha") +
      ggplot2::theme_minimal()

    if(origin){
      p <- p + ggplot2::expand_limits(x = 0, y = 0)
    }

    if(!is.null(palette)){
      p <- p + ggplot2::scale_color_viridis_d(option = palette)
    }

    if(facet){
      p <- p + ggplot2::facet_wrap(~ alpha.f, scales = "free_y") +
        ggplot2::theme(legend.position = "none")
    }

    p
  }

  # --- Panel 2: minimum error achieved, per alpha ---------------------------
  make_summary_plot <- function(){

    df <- object$summary
    df$is.best <- seq_len(nrow(df)) == best.idx

    xlab.s <- if(!is.null(xlab.summary)) xlab.summary else "alpha"
    ylab.s <- if(!is.null(ylab.summary)) ylab.summary else "Minimum weighted error"
    main.s <- if(!is.null(main.summary)) main.summary else "Best error by alpha"

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$alpha, y = .data$min.error))

    if(origin){
      p <- p +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.4)
    }

    p <- p +
      ggplot2::geom_line(color = "grey50") +
      ggplot2::geom_point(ggplot2::aes(color = .data$is.best), size = 3) +
      ggplot2::geom_vline(xintercept = best.alpha, linetype = "dashed", color = "grey40") +
      ggplot2::scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "red"), guide = "none") +
      ggplot2::labs(title = main.s,
                   subtitle = paste0("Best: alpha = ", signif(best.alpha, 3),
                                     ", lambda = ", signif(best.lambda, 3)),
                   x = xlab.s, y = ylab.s) +
      ggplot2::theme_minimal()

    if(origin){
      p <- p + ggplot2::expand_limits(x = 0, y = 0)
    }

    p
  }

  if(type == "profile") return(make_profile_plot())
  if(type == "summary") return(make_summary_plot())

  # type == "both"
  p1 <- make_profile_plot()
  p2 <- make_summary_plot()

  if(requireNamespace("patchwork", quietly = TRUE)){
    combined <- p1 + p2
    return(combined)
  } else {
    message("Package 'patchwork' not installed; returning a list of two separate ggplot ",
           "objects ($profile and $summary) instead of one combined figure. Install ",
           "patchwork with install.packages('patchwork') to combine them automatically.")
    return(list(profile = p1, summary = p2))
  }
}


#' ggplot2 plotting method for \code{w.elnet} objects (single-alpha \code{wElnet()} output)
#'
#' @description A \code{ggplot2}-based counterpart to [plot.w.elnet()] that visualizes the
#'              weighted error curve across the lambda grid for a single
#'              \code{\link{wElnet}} fit (i.e. a single, fixed alpha), marking the
#'              selected \code{lambda.min}. This is the single-alpha counterpart to
#'              [autoplot.par.w.elnet()], which handles the multi-alpha output of
#'              [par_wElnet()].
#'
#'              Registered as an \code{\link[ggplot2]{autoplot}} method, so it dispatches
#'              automatically via \code{autoplot(en.dcv)} once \code{ggplot2} is loaded.
#'
#' @param object An object of class \code{w.elnet}, the output of \code{\link{wElnet}}.
#' @param log.lambda A logical value. If \code{TRUE} (default), the x-axis is
#'                    \code{log(lambda)}, matching \code{glmnet} convention. If \code{FALSE},
#'                    lambda is plotted on its original scale.
#' @param origin A logical value. If \code{TRUE} (default), solid black reference lines are
#'               drawn at x=0 and y=0, and the plot's coordinate range is expanded if needed
#'               so the origin is always visible. If \code{FALSE}, no origin lines are drawn
#'               and the axes are scaled to the data range as usual.
#' @param main,xlab,ylab Optional strings overriding the plot title and axis labels. If
#'                \code{NULL} (default), sensible defaults are used ("Error profile
#'                (alpha = ...)", "log(lambda)" or "lambda", and "Weighted error").
#' @param ... Currently unused; included for S3 consistency with \code{autoplot()}.
#'
#' @return A single \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' library(MLSurvey)
#' library(ggplot2)
#' data(nhanes2013_sbc)
#'
#' en.dcv <- wElnet(data = nhanes2013_sbc,
#'                  col.y = "HBP", col.x = 2:61,
#'                  family = "binomial", alpha = 0.729,
#'                  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'                  method = "dCV", k = 10, R = 20)
#'
#' autoplot(en.dcv)
#' autoplot(en.dcv, main = "My custom title", xlab = "Penalty", ylab = "Error")
#' }
#'
#' @export
autoplot.w.elnet <- function(object, log.lambda = TRUE, origin = TRUE,
                             main = NULL, xlab = NULL, ylab = NULL, ...){

  if(!inherits(object, "w.elnet")){
    stop("'object' must be an object of class 'w.elnet', the output of wElnet().")
  }

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package 'ggplot2' is required for autoplot(). Install it with install.packages('ggplot2').")
  }

  df <- data.frame(lambda = object$lambda$grid, error = object$error$average)
  df$x <- if(log.lambda) log(df$lambda) else df$lambda
  best.x <- if(log.lambda) log(object$lambda$min) else object$lambda$min
  best.error <- df$error[which.min(abs(df$lambda - object$lambda$min))]

  # alpha is stored directly on the w.elnet object (added in wElnet()).
  main.default <- paste0("Error profile (alpha = ", signif(object$alpha, 3), ")")

  main.txt <- if(!is.null(main)) main else main.default
  xlab.txt <- if(!is.null(xlab)) xlab else if(log.lambda) "log(lambda)" else "lambda"
  ylab.txt <- if(!is.null(ylab)) ylab else "Weighted error"

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$error))

  if(origin){
    p <- p +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
      ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.4)
  }

  p <- p +
    ggplot2::geom_line(linewidth = 0.9, color = "steelblue") +
    ggplot2::geom_vline(xintercept = best.x, linetype = "dashed", color = "grey40") +
    ggplot2::annotate("point", x = best.x, y = best.error, shape = 8, size = 3, color = "red") +
    ggplot2::annotate("text", x = best.x, y = best.error,
                      label = paste0("  lambda.min = ", signif(object$lambda$min, 3)),
                      hjust = 0, vjust = -0.5, size = 3.2, color = "red") +
    ggplot2::labs(title = main.txt, x = xlab.txt, y = ylab.txt) +
    ggplot2::theme_minimal()

  if(origin){
    p <- p + ggplot2::expand_limits(x = 0, y = 0)
  }

  p
}
