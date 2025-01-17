#' Weighted Random Forest prediction models for complex survey data
#'
#'@description A function to fit wRF prediction (linear or logistic) models for complex survey data with sampling weights in the estimation process and
#'            to select tuning parameters minimizing the weighted error for a selected replicating weights method.  Detailed arguments are referred to in R-\code{\link{randomForest}}.
#'
#' @param data A data frame with information about independent variables, as well as sampling weights and strata and cluster indicators. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.
#' @param y A vector of the response variable. If a factor, classification is assumed, otherwise, regression is assumed. If omitted, randomForest will run in a unsupervised mode.
#' @param col.x A numeric vector indicating indices of columns for covariates or independent variables or a vector of character strings indicating names of these columns.
#' @param xtest A data frame or matrix containing predictors for the test set.
#' @param ytest A vector of response variable for the test set.
#' @param ntree The number of trees to grow. This should not set to be too small a number, to ensure that every input row gets predicted at least a few times.
#' @param mtry  The number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
#' @param cluster A character string indicating the name of cluster identifiers. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.
#' @param strata A character string indicating the name of strata identifiers. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.
#' @param weights A character string indicating the name of sampling weights. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.
#' @param design An object of class \code{survey.design} generated by \code{survey::svydesign()}. It could be \code{NULL} if information about \code{cluster}, \code{strata}, \code{weights} and \code{data} were given.
#' @param method A character string indicating a method of replicate weights. Choose one of these: \code{JKn}, \code{dCV}, \code{bootstrap}, \code{subbootstrap}, \code{BRR}, \code{split}, \code{extrapolation}.
#' @param k An integer. The number of folds for the \code{dCV} method. Default is \code{k=10}.
#' @param R An integer. The number of times the sample is partitioned for \code{dCV}, \code{split} or \code{extrapolation} method. Default is \code{R=1}.
#' @param B An integer. The number of bootstrap re-samples for \code{bootstrap} and \code{subbootstrap} methods. Default is \code{B=200}.
#' @param dCV.sw.test A logical value indicating the method for estimating the error for \code{dCV} method. \code{FALSE}, (the default option) estimates the error for each test set and defines the cross-validated error based on the average strategy. Option \code{TRUE} estimates the cross-validated error based on the pooling strategy
#' @param train.prob A numeric between 0 and 1, indicating the proportion of clusters (for the method \code{split}) or strata (for the method \code{extrapolation}) to be set in the training sets. Default is \code{train.prob = 0.7}. Only applies for \code{split} and \code{extrapolation} methods.
#' @param method.split A string of one the following replicate weights methods to be implemented under the \code{split} method in the \code{method} argument: \code{dCV}, \code{bootstrap} or \code{subbootstrap}.
#' @param print.rw A logical value. If \code{TRUE}, the data set with the replicate weights is saved in the output object. Default \code{print.rw=FALSE}.
#' @param replace,classwt,cutoff,sampsize,nodesize,maxnodes,importance,proximity,oob.prox,norm.votes,do.trace,keep.forest,corr.bias,keep.inbag,... Optional parameters to be passed to the low level function [randomForest::randomForest()].
#'
#'
#' @seealso [randomForest::randomForest()] for arguments and return values in detail.
#'
#' @return The output object of the function \code{wRandomforest()} is an object of class \code{w.randomforest}:
#' - `mtry`: A list containing information on `mtry` as a tuning parameter (the number of predictors sampled for splitting at each node):
#'   - `all`: A numeric vector of `k*R` `mtry`- values, i.e. `mtry` for each fold per replicate.
#'   - `optimal.mtry`: An optimal tuning parameter, `mtry` among \code{mtry$all} that minimizes the average error.
#' - `evaluation_log`: A list containing information of two elements:
#'   - `weighted.test.error`: A vector of the average errors corresponding to tuning parameters in \code{mtry$all}.
#'   - `min.weighted.test.error`: An minimum error for the optimal tuning parameter from \code{mtry$optimal.mtry} to select the final model.
#' - `model`: A list containing information on the fitted model by \code{optimal.mtry}: an object of class \code{\link[randomForest]{randomForest}}.
#'           Note that the selected model is fitted by the original \code{data}, not one of the generated training sets by replicate weights.
#' - `data.rw`: A data frame containing the original data set and the replicate weights added to define training and test sets. Only included in the output object if \code{print.rw=TRUE}.
#' - `call`: The call that executes this function producing the object of \code{w.randomforest}.
#'
#'@note
#' `final.model` can leverage R-\code{randomForest} functionality for comprehensive data analysis for linear/logistic regression but will be
#' extensively used for other models, i.e., Cox, Poisson, etc.
#'
#' @examples
#'  # Global options to avoid only one PSU in a stratum in a particular domain or subpopulation
#' options(survey.adjust.domain.lonely=TRUE)
#' options(survey.lonely.psu="adjust")
#'
#'
#'  # For classification: y should be a factor.
#'  # If a test set is provided, one can plug it into `xtest`, `ytest`
#'  # just as implemented in \code{randomForest::randomForest()}.
#'
#'  data(nhanes2013_sbc)
#' dcv.rf <- wRandomforest(data = nhanes2013_sbc,
#'               y = as.factor(nhanes2013_sbc$HBP), col.x = 2:61,
#'               cluster = "SDMVPSU", strata = "SDMVSTRA", weights = "WTSAF2YR",
#'               method = "dCV", k=5, R=5,importance=TRUE,proximity = TRUE)
#'
#' # Or equivalently:
#' des <- survey::svydesign(ids=~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTSAF2YR,
#'                               nest = TRUE, data =nhanes2013_sbc)
#' dcv.rf <- wRandomforest(y = as.factor(nhanes2013_sbc$HBP), col.x = 2:61, design =des,
#'                         importance=TRUE,proximity = TRUE,
#'                         method = "dCV", k=5, R=5)
#'
#' @export
wRandomforest <- function(data = NULL, col.x = NULL, y = NULL, xtest=NULL,ytest=NULL,
                   cluster = NULL, strata = NULL, weights = NULL, design = NULL,
                   method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
                   k = 10, R = 1, B = 200, dCV.sw.test = FALSE, train.prob = 0.7,
                   method.split = c("dCV", "bootstrap", "subbootstrap"), print.rw = FALSE,
                   ntree=500, mtry=if (!is.null(y) && !is.factor(y)) max(floor(ncol(data[,col.x])/3), 1) else floor(sqrt(ncol(data[,col.x]))),
                   replace=TRUE, classwt=NULL, cutoff,
                   sampsize = if (replace) nrow(data) else ceiling(.632*nrow(data)),
                   nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1, maxnodes=NULL,
                   importance = FALSE, proximity = FALSE, oob.prox=proximity,
                   norm.votes=TRUE, do.trace=FALSE,
                   keep.forest=TRUE, corr.bias=FALSE,
                   keep.inbag=FALSE, ...){

  # Stops and messages:
  if(is.null(data) & is.null(design)){stop("Information about either the data set ('data') or the sampling design ('design') needed.")}

  if(method == "split"){
    if(is.null(train.prob)){stop("Selected replicate weights method: 'split'.\nPlease, set a value between 0 and 1 for the argument 'train.prob'.")}
    if(train.prob < 0 | train.prob > 1){stop("Selected replicate weights method: 'split'.\nPlease, set a value between 0 and 1 for the argument 'train.prob'.")}
    if(length(method.split)!=1){stop("Selected replicate weights method: 'split'.\nPlease, set a valid method for the argument 'method.split'. Choose between: 'dCV', 'bootstrap' or 'subbootstrap'.")}
  }

  if(method == "extrapolation"){
    if(is.null(train.prob)){stop("Selected replicate weights method: 'extrapolation'.\nPlease, set a value between 0 and 1 for the argument 'train.prob'.")}
    if(train.prob < 0 | train.prob > 1){stop("Selected replicate weights method: 'extrapolation'.\nPlease, set a value between 0 and 1 for the argument 'train.prob'.")}
  }

  if(method %in% c("JKn", "bootstrap", "subbootstrap", "BRR")){
    if(R!=1){cat("Selected method:", method,". For this method, R = 1. Thus, the argument R =",R, "has been ignored.")}
  }

  if(method %in% c("dCV", "split", "extrapolation")){
    if(R != round(R)){stop("The argument 'R' must be an integer greater or equal to 1. R=",R," is not an integer.\nPlease, set a valid value for 'R' or skip the argument to select the default option R=1.")}
    if(R < 1){stop("The argument 'R' must be an integer greater or equal to 1. R=",R," lower than 1.\nPlease, set a valid value for 'R' or skip the argument to select the default option R=1.")}
  }

  if(method != "dCV"){
    if(!is.null(k) & k!=10){cat("Selected method:", method,". The argument k =",k, "is not needed and, hence, has been ignored.")}
  }

  if(method == "dCV"){
    if(k != round(k)){stop("The argument 'k' must be an integer. k=",k," is not an integer.\nPlease, set a valid value for 'k' or skip the argument to select the default option k=10.")}
    if(k < 1){stop("The argument 'k' must be a positive integer. k=",k," is not a positive integer.\nPlease, set a valid value for 'k' or skip the argument to select the default option k=10.")}
  }

  if(!(method %in% c("bootstrap", "subbootstrap"))){
    if(!is.null(B) & B!=200){cat("Selected method:", method,". The argument B =",B, "is not needed and, hence, has been ignored.")}
  }

  if(method %in% c("bootstrap", "subbootstrap")){
    if(B != round(B)){stop("The argument 'B' must be an integer. B=",B," is not an integer.\nPlease, set a valid value for 'B' or skip the argument to select the default option B=200.")}
    if(B < 1){stop("The argument 'B' must be a positive integer. B=",B," is not a positive integer.\nPlease, set a valid value for 'B' or skip the argument to select the default option B=200.")}
  }



  # Step 0: Notation
  if(!is.null(design)){
    cluster <- as.character(design$call$id[2])
    if(cluster == "1" || cluster == "0"){
      cluster <- NULL
    }
    strata <- as.character(design$call$strata[2])
    weights <- as.character(design$call$weights[2])
    data <- get(design$call$data)
  }
  if (is.factor(y)) obj <- "binary:logistic" else obj <- "reg:squarederror"

  # Step 1: Generate replicate weights based on the method
  newdata <- replicate_weights(data = data, method = method,
                               cluster = cluster, strata = strata, weights = weights,
                               k = k, R = R, B = B,
                               train.prob = train.prob, method.split = method.split,
                               rw.test = TRUE, dCV.sw.test = dCV.sw.test)




  # Step 2: Fit the training models and estimate yhat for units in the sample
  hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
  rwtraincols <- grep("_train", colnames(newdata))

  l.yhat <- list(); opt_mtry <- NULL

  for(col.w in rwtraincols){
    idx<- which(newdata[,col.w]>0)
    model <- randomForest::randomForest(x = newdata[idx,col.x],y=y[idx],xtest=newdata[-idx,col.x],
                            weights = as.numeric(newdata[idx,col.w]), mtry = mtry, strata = hclus,
                            importance = importance, proximity = proximity, ...)

    # Sample yhat
    l.yhat[[length(l.yhat) + 1]] <- model$test$predicted
    names(l.yhat)[[length(l.yhat)]] <- paste0("yhat_", colnames(newdata)[col.w])
    opt_mtry <- c(opt_mtry,model$mtry)
    # print(opt_mtry)
  }

  # Step 3: estimate the error in the test sets


  error <- eval.error(data = newdata, l.yhat = l.yhat,
                     method = method, cv.error.ind = dCV.sw.test,
                     R = R, k = k, B = B,
                     y = y, objective = obj, weights = weights)
  # mean.error <- mean(error)
  # print(paste0("test error",error))
  opt.mtry <- opt_mtry[which.min(error)]

  model <- randomForest::randomForest(x = data[,col.x],y=y,xtest=xtest, ytest=ytest,
                            weights = data[,weights], mtry = opt.mtry, strata = hclus,
                            importance = importance, proximity = proximity, ...)

  result <- list()
  result$mtry <- list(all = opt_mtry,
                      optimal.mtry = opt.mtry)
  result$evaluation_log <- list(weighted.test.error = error,
                                min.weighted.test.error = min(error)       )
  result$model <- model
  result$call <- match.call()

  if(print.rw == TRUE){result$data.rw <- newdata}

  class(result) <- "w.randomforest"

  return(result)

}
