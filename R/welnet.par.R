#' Parallel Cross-Validation for Weighted Elastic Net Models
#'
#' @description Fits weighted Elastic Net models across a sequence of alpha values
#'              in parallel, assigning each CPU core to one alpha value.
#'
#' @inheritParams wElnet
#' @param alpha A numeric vector of alpha values to parallelize over.
#' @param n_cores An integer indicating the number of CPU cores to use. 
#'                Defaults to 1 less than the total available cores.
#'
#' @return A named list where each element corresponds to the `wElnet` output 
#'         for a specific alpha value.
#'         
#'         
#' @examples
#' # 1. Define your alpha sequence
#' alpha_grid <- c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000)
#'
#' # 2. Call the parallelized function
#'parallel_models <- cv.wElnet(
#'  data = nhanes2013_sbc,
#'  col.y = "HBP", 
#'  col.x = 2:61,
#'  family = "binomial", 
#'  alpha = alpha_grid,
#'  cluster = "SDMVPSU", 
#'  strata = "SDMVSTRA", 
#'  weights = "WTSAF2YR",
#'  method = "dCV", 
#'  k = 10, 
#'  R = 20
#')
#' 
#' # 3. Access results for a specific alpha (e.g., alpha = 0.729)
#' print(parallel_models$alpha_0.729$lambda$min)

#'          
#' @export
cv.wElnet <- function(data = NULL, col.y = NULL, col.x = NULL,
                      cluster = NULL, strata = NULL, weights = NULL, design = NULL,
                      family = c("gaussian", "binomial"),
                      lambda = NULL, alpha = c(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000),
                      nlambda = 100, lambda.min.ratio = NULL,
                      method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
                      k = 10, R = 1, B = 200,
                      dCV.sw.test = FALSE, n_cores = NULL) {
  
  # 1. Check and match arguments
  family <- match.arg(family)
  method <- match.arg(method)
  
  # 2. Determine the number of cores to use
  avail_cores <- parallel::detectCores()
  if (is.null(n_cores)) {
    n_cores <- max(1, avail_cores - 1) 
  }
  # Do not use more cores than the number of alphas
  n_cores <- min(n_cores, length(alpha))
  
  # 3. Set up the parallel cluster
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  # Ensure the cluster safely shuts down when the function finishes or errors out
  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  })
  
  # 4. Execute wElnet in parallel across the alpha vector
  # .packages ensures required dependencies are loaded on each worker node
  results <- foreach::foreach(
    a = alpha,
    .packages = c("survey", "glmnet") 
  ) %dopar% {
    
    # Dynamic default for lambda.min.ratio if not provided by user
    if (is.null(lambda.min.ratio)) {
      # Fallback logic mimicking glmnet behavior within the worker thread
      nobs <- if (!is.null(data)) nrow(data) else nrow(design$variables)
      nvars <- length(col.x)
      l_ratio <- ifelse(nobs < nvars, 0.01, 1e-04)
    } else {
      l_ratio <- lambda.min.ratio
    }
    
    # Call your core function for the single alpha
    wElnet(
      data = data, col.y = col.y, col.x = col.x,
      cluster = cluster, strata = strata, weights = weights, design = design,
      family = family, lambda = lambda, alpha = a,
      nlambda = nlambda, lambda.min.ratio = l_ratio,
      method = method, k = k, R = R, B = B,
      dCV.sw.test = dCV.sw.test
    )
  }
  
  # 5. Name the output list elements by their respective alpha value
  names(results) <- paste0("alpha_", alpha)
  
  return(results)
}
