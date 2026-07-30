#' Execute Parallel Weighted Elastic Net Optimization
#'
#' @description Runs `wElnet` in parallel over an alpha vector, extracts the overall
#'              optimal hyperparameter combination, and isolates the final fitted model.
#'
#' @inheritParams cv.wElnet
#' @return A list containing three elements:
#'         - `best_parameters`: A list with the optimal alpha, lambda, and minimum error.
#'         - `final_model`: The fitted S3 class `glmnet` model object corresponding to the optimal hyperparameters.
#'         - `full_cv_results`: The raw list containing the full `wElnet` output for every alpha.
#'         
#' @examples
#' # 1. Execute parallelized modeling, optimization, and final model extraction
#' model_run <- run_wElnet_optimization(
#'  data = nhanes2013_sbc, col.y = "HBP", col.x = 2:61, family = "binomial",
#'  cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'  method = "dCV", k = 10, R = 20
#' )
#'
#' # 2. View isolated optimal parameters
#' print(model_run$best_parameters)
#'
#'# 3. Access standard glmnet functions directly on your final model
#'# (e.g., plot the coefficient path profile)
#'   plot(model_run$final_model, xvar = "lambda")
#'
#'# 4. View selected variables and their clean impact coefficients
#'  active_covariates <- get_final_coefficients(model_run)
#'  print(active_covariates)
#'
#'# 5. Generate validation diagnostic graphs
#' error_grid_plot <- plot_wElnet_optimization(model_run)
#' print(error_grid_plot)          
#'         
#'         
#' @export
run_wElnet_optimization <- function(data = NULL, col.y = NULL, col.x = NULL,
                                    cluster = NULL, strata = NULL, weights = NULL, design = NULL,
                                    family = c("gaussian", "binomial"),
                                    lambda = NULL, alpha = c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000),
                                    nlambda = 100, lambda.min.ratio = NULL,
                                    method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
                                    k = 10, R = 1, B = 200,
                                    dCV.sw.test = FALSE, n_cores = NULL) {
  
  # Step 1: Run parallelized cross-validation function
  message(">> Initializing parallel cluster and running cross-validation grids...")
  cv_results <- cv.wElnet(
    data = data, col.y = col.y, col.x = col.x,
    cluster = cluster, strata = strata, weights = weights, design = design,
    family = family, lambda = lambda, alpha = alpha,
    nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
    method = method, k = k, R = R, B = B,
    dCV.sw.test = dCV.sw.test, n_cores = n_cores
  )
  
  # Step 2: Extract minimum average error from each alpha model
  message(">> Extracting optimal parameters...")
  min_errors <- sapply(cv_results, function(model) {
    idx <- which.min(model$error$average)
    if(length(idx) == 0) return(NA)
    model$error$average[idx]
  })
  
  # Find which alpha yielded absolute lowest cross-validation error
  best_alpha_idx <- which.min(min_errors)
  best_alpha_name <- names(best_alpha_idx)
  
  # Pull winning model details
  best_model_output <- cv_results[[best_alpha_name]]
  best_lambda <- best_model_output$lambda$min
  lowest_error <- min_errors[best_alpha_idx]
  numeric_alpha <- as.numeric(gsub("alpha_", "", best_alpha_name))
  
  # Extract pre-computed final model from wElnet structure
  extracted_final_model <- best_model_output$model$final_model
  
  # Pack results together
  optimization_output <- list(
    best_parameters = list(
      optimal_alpha = numeric_alpha,
      optimal_lambda = best_lambda,
      minimum_error  = as.numeric(lowest_error)
    ),
    final_model = extracted_final_model,
    full_cv_results = cv_results
  )
  
  message(paste0(">> Optimization Complete! Best Alpha: ", numeric_alpha, " | Best Lambda: ", round(best_lambda, 5)))
  return(optimization_output)
}


#' Extract Non-Zero Coefficients from Final Model
#'
#' @param optimization_output The output list returned by `run_wElnet_optimization()`.
#' @return A data frame containing selected feature names and their corresponding coefficients.
#' @export
get_final_coefficients <- function(optimization_output) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("The 'glmnet' package is required. Please install it.")
  }
  
  # Extract components
  fit <- optimization_output$final_model
  best_lambda <- optimization_output$best_parameters$optimal_lambda
  
  # Extract coefficient matrix at the optimal lambda value
  coef_matrix <- glmnet::coef.glmnet(fit, s = best_lambda)
  
  # Convert sparse matrix format to a clean data frame
  coef_df <- data.frame(
    Feature = rownames(coef_matrix),
    Coefficient = as.numeric(coef_matrix)
  )
  
  # Filter out non-selected features (zeros)
  selected_features <- coef_df[coef_df$Coefficient != 0, ]
  rownames(selected_features) <- NULL
  
  return(selected_features)
}
