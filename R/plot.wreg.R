#' Plotting weighted Elastic Net/LASSO object
#'
#' @description
#' A plot function generating a graph of the optimal number of variables selected in the final model by LASSO and Elastic Net
#' for complex survey data.
#'
#' @param x an object of class "wlasso" or "w.elnet".  This is extended from \code{svyVarSel::wlasso.plot()} (or \code{wlasso::wlasso.plot()}).
#'
#'
#' @return a graph
#'
#' @examples
#' # For weighted LASSO by either wlasso or welnet function,
#' \dontrun{
#' wlas <- svyVarSel::wlasso(data = nhanes2013_sbc, col.y = "HBP", col.x = 2:61,
#'               family = "binomial", cluster = "SDMVPSU", strata = "SDMVSTRA",
#'               weights = "WTSAF2YR", method = "dCV", k=10, R=20)
#' svyVarSel::wlasso.plot(wlas)
#'
#'  # Equivalently,
#'
#' plot_wreg(wlas)
#'}
#' # Or equivalently, with weighted Elastic Net,
#' wen <- wElnet(data = nhanes2013_sbc, col.y = "HBP", col.x = 2:61,alpha =1,
#'               family = "binomial", cluster = "SDMVPSU", strata = "SDMVSTRA",
#'               weights = "WTSAF2YR", method = "dCV", k=10, R=20)
#' plot_wreg(wen)
#'
#' @export
plot_wreg <- function(x){

  if(inherits(x, "w.elnet")){DF=x$model$final_model$df}
    else{DF=x$model$min$df}
  plot(x = log(x$lambda$grid), y = x$error$average, col = "red", pch = 20,
       xlab = bquote("log("~lambda~")"), ylab = "Average error")
  abline(v = log(x$lambda$min), lty = 2, col = "black")
  mtext(text = paste0("The optimal number of variables: ", DF), side = 3)

}
