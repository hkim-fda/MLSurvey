% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optim.wxgb.para.R
\name{optim_wxgb_para}
\alias{optim_wxgb_para}
\title{Optimal (hyper-) parameter search for weighted XGBoost (wXGBoost) for complex survey data}
\usage{
optim_wxgb_para(
  y,
  col.x,
  param = list(),
  nitr = 100,
  nfolds = 10,
  R = 1,
  nRounds = 10000,
  nstop = 5,
  method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
  missing = NA,
  maximize = FALSE,
  cluster = NULL,
  strata = NULL,
  weights = NULL,
  design = NULL,
  data = NULL,
  print_every_n = 50L,
  ...
)
}
\arguments{
\item{y}{A numeric vector of response variable.}

\item{col.x}{A numeric vector indicating indices of the covariates or a string vector indicating these column names.}

\item{param}{The list of parameters.  Should include \code{objective} to be a string of either \code{binary:logistic} for binary classification or \code{reg:squarederror} for linear regression.
The complete list of parameters is found on \code{\link[xgboost:xgb.train]{xgboost::xgb.train()}} or \url{https://xgboost.readthedocs.io/en/latest/parameter.html}.}

\item{nitr}{The number of iterations in the \code{for}-loop to search for optimal (hyper-) parameters.  Default is \code{100}.}

\item{nfolds}{The number of folds to be defined for \code{dCV} method. Default is \code{k=10}.}

\item{R}{The number of times the sample is partitioned for the \code{dCV} method. Default is \code{R=1}.}

\item{nRounds}{The maximum number of boosting iterations.}

\item{nstop}{\code{NULL} does not trigger the early stopping function. Setting to an integer k will stop training with a validation set unless the performance improve for k rounds.  Setting this parameter engages the \code{cb.early.stop} callback.}

\item{method}{A character string indicating a method of replicate weights. Choose one of these: \code{JKn}, \code{dCV}, \code{bootstrap}, \code{subbootstrap}, \code{BRR}, \code{split}, \code{extrapolation}.}

\item{missing}{Default is set to \code{NA}, which means that \code{NA} values should be considered as 'missing' by the algorithm.  Sometimes, \code{0} or other extreme values might be used to represent missing values. This parameter is only used when input is a dense matrix.}

\item{maximize}{\code{nstop} is set, then this parameter must be set as well. When it is TRUE, it means the larger the evaluation score the better. This parameter is passed to the cb.early.stop callback.}

\item{cluster}{A character string of a cluster identifier. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.}

\item{strata}{A character string of a strata identifier. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.}

\item{weights}{A character string of a sampling weights' identifier. It could be \code{NULL} if the sampling design were plugged in the \code{design} argument.}

\item{design}{An object of class \code{survey.design} generated by \code{survey::svydesign()}. It could be \code{NULL} if information about \code{cluster}, \code{strata}, \code{weights} and \code{data} were given.}

\item{data}{A data frame with information about the response variable and covariates, as well as sampling weights, strata, and cluster indicators. It could be \code{NULL} if the sampling design were added to the \code{design} argument.}

\item{print_every_n}{Print each n-th iteration evaluation messages when verbose>0. Default is \code{50L} printing every 50th iteration message. This parameter is passed to the \code{cb.print.evaluation} callback.}

\item{...}{optional parameters to be passed to the low level function \code{xgboost::xgb.train()} or \code{xgboost::xgboost()}.}
}
\value{
A list of return values for optimal (hyper-)parameters as follows:
\itemize{
\item \code{nrounds} : The best iteration number having the best evaluation metric value
\item \code{snumber} : A seed number for \code{set.seed()} yielding the optimal (hyper-)parameters.
\item \code{params}  : A list of optimal (hyper-) parameters selected among \code{nitr} models implemented by \code{wxgboost()} with \code{dCV} on every iteration.
}
}
\description{
A function to search for optimal (hyper-) parameters by replicate weights to implement cross-validation (CV) for XGBoost models.
}
\examples{
 ## Set user defined (hyper-)parameters:
 param <- list(objective = "binary:logistic",
                 max_depth = sample(4:8, 1),
                  eta = runif(1, .01, .3),
                  gamma = runif(1, 0.0, 1),
                  subsample = 0.5, # much smaller for large N
                  colsample_bytree = runif(1, .5, .8),
                  min_child_weight = sample(1:40, 1),
                  max_delta_step = sample(1:10, 1) ) # for very imbalanced case

  ## search for optimal parameters
 data(nhanes2013_sbc)
 Mydesign <- survey::svydesign(ids=~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTSAF2YR,
                              nest = TRUE, data = nhanes2013_sbc)
 opt.par<- optim_wxgb_para(nhanes2013_sbc$HBP,2:61,param,method="dCV",
                           design=Mydesign,print_every_n=250L)
\dontrun{
 ## fitting the optimal model
 set.seed(opt.par$snumber)

 ### The data needs to be a sparse matrix without intercept.
 ### Generate a sparse matrix from a regular dataset
 unwtData<-nhanes2013_sbc[,-c(62:66)]   #excluding weight related vectors
 Mat<- sparse.model.matrix(HBP~. ,data = unwtdata )[,-1]   #dropping intercept
 wtData <- xgb.DMatrix(data = nhanes2013_sbc,label=nhanes2013_sbc$HBP,
                       weight=nhanes2013_sbc$WTSAF2YR)

 wxgb<-xgb.train(data=wtData, params=opt.par$params, nrounds=opt.para$nrounds,feval = evalerror.bin)
}
}
\seealso{
\code{\link[xgboost:xgb.train]{xgboost::xgb.train()}} for relevant arguments and return values in detail.

\code{\link[=wXGBoost]{wXGBoost()}}, \code{\link[=replicate_weights]{replicate_weights()}} for detailed information on implementation.
}
