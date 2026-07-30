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


#' Plot Optimization Results for Weighted Elastic Net
#'
#' @param optimization_output The output list returned by `run_wElnet_optimization()`.
#' @import ggplot2
#' @return A ggplot object showing error curves across lambda for each alpha.
#' @export
plot_wElnet_optimization <- function(optimization_output) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting. Please install it.")
  }
  
  # Extract components from the optimization output wrapper
  cv_results  <- optimization_output$full_cv_results
  best_params <- optimization_output$best_parameters
  
  plot_data_list <- list()
  
  # Build a combined dataset from the nested results structure
  for (alpha_name in names(cv_results)) {
    model <- cv_results[[alpha_name]]
    alpha_val <- as.numeric(gsub("alpha_", "", alpha_name))
    
    df <- data.frame(
      Lambda = model$lambda$grid,
      Error = model$error$average,
      Alpha = factor(paste0("alpha = ", alpha_val)),
      Alpha_Numeric = alpha_val
    )
    plot_data_list[[alpha_name]] <- df
  }
  
  plot_data <- do.call(rbind, plot_data_list)
  
  # Create data frame to mark the single absolute best parameter location on the plot
  best_point_df <- data.frame(
    Lambda = best_params$optimal_lambda,
    Error  = best_params$minimum_error,
    Alpha  = factor(paste0("alpha = ", best_params$optimal_alpha))
  )
  
  # Base plot building
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Lambda, y = Error)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
    ggplot2::scale_x_log10() + 
    ggplot2::facet_wrap(~ Alpha, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Weighted Elastic Net Optimization Landscapes",
      subtitle = paste0("Optimal Global Config: Alpha = ", best_params$optimal_alpha, 
                        " | Lambda = ", round(best_params$optimal_lambda, 5)),
      x = "Lambda Grid (Log Scale)",
      y = "Average Cross-Validation Error"
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#f5f5f5", color = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Overlay a red target point pinpointing the overall winning hyperparameter set
  p <- p + ggplot2::geom_point(
    data = best_point_df, 
    ggplot2::aes(x = Lambda, y = Error), 
    color = "firebrick", size = 3, shape = 19
  ) +
    ggplot2::geom_point(
      data = best_point_df, 
      ggplot2::aes(x = Lambda, y = Error), 
      color = "firebrick", size = 5, shape = 1
    )
  
  return(p)
}
