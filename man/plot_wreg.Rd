% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.wreg.R
\name{plot_wreg}
\alias{plot_wreg}
\title{Plotting weighted Elastic Net/LASSO object}
\usage{
plot_wreg(x)
}
\arguments{
\item{x}{an object of class "wlasso" or "w.elnet".  This is extended from \code{svyVarSel::wlasso.plot()} (or \code{wlasso::wlasso.plot()}).}
}
\value{
a graph
}
\description{
A plot function generating a graph of the optimal number of variables selected in the final model by LASSO and Elastic Net
for complex survey data.
}
\examples{
# For weighted LASSO by either wlasso or welnet function,
\dontrun{
wlas <- svyVarSel::wlasso(data = nhanes2013_sbc, col.y = "HBP", col.x = 2:61,
              family = "binomial", cluster = "SDMVPSU", strata = "SDMVSTRA",
              weights = "WTSAF2YR", method = "dCV", k=10, R=20)
svyVarSel::wlasso.plot(wlas)

 # Equivalently,

plot_wreg(wlas)
}
# Or equivalently, with weighted Elastic Net,
wen <- wElnet(data = nhanes2013_sbc, col.y = "HBP", col.x = 2:61,alpha =1,
              family = "binomial", cluster = "SDMVPSU", strata = "SDMVSTRA",
              weights = "WTSAF2YR", method = "dCV", k=10, R=20)
plot_wreg(wen)

}
