% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wxgb.error.R
\name{evalerror_bin}
\alias{evalerror_bin}
\title{Weighted mean error of binary outcome for complex survey data}
\usage{
evalerror_bin(preds, dtrain)
}
\arguments{
\item{preds}{Predicted binary values (\code{0/1}) from the model.}

\item{dtrain}{An object of \code{xgboost::xgb.DMatrix} including \code{label}(response variable or binary outcome), \code{data}, and \code{weight}.}
}
\value{
A list of two components:
\itemize{
\item \code{metric} : A string labelled as \code{weighted.error}.
\item \code{value}  : A numeric value of weighted mean error.
}
}
\description{
A function to evaluate XGBoost models by weighted mean error for binary outcome (\code{0/1}).  This function is a customized error function to
plug in the function \code{wXGBoost()} for complex survey data.
}
\seealso{
\code{\link[xgboost:xgb.train]{xgboost::xgb.train()}} for relevant arguments and return values in detail.

\code{\link[=wXGBoost]{wXGBoost()}} for detailed information on implementation.

\code{\link[=optim_wxgb_para]{optim_wxgb_para()}} for detailed optimization and examples.
}
