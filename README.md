# MLSurvey

## *M*achine *L*earning for Complex *S*urvey Data
<!-- badges: start -->
[![R-CMD-check](https://github.com/hkim-fda/MLsurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hkim-fda/MLsurvey/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


**MLSurvey** is a R-package to develop Machine Learning models for
complex survey data using suitable Cross-Validation methods:
replicate weights methods. This ML tool currently offers weighted
Elastic Net (**wElnet**) for a single $\alpha$, its CPU parallel computing
(**par_wElnet**) for a sequence of $\alpha$'s, generic plots (*plot*) and
ggplot2 supported counterparts (*autoplot*) for both computation
results. It also supports weighted random forest (**wRandomforest**),
and weighted XGBoost (**wXGBoost**) with random optimal
(hyper-)parameter search (*optim_wxgb_para*). Confidential interval
(*ci_wauc*) for weighted area under the ROC curve (wAUC) is computed by
bootstrap.

Since all the weighted ML methods are extended by existing algorithms,
[glmnet](https://github.com/cran/glmnet),
[xgboost](https://github.com/dmlc/xgboost/tree/master), and
[randomForest](https://www.stat.berkeley.edu/~breiman/RandomForests/),
 final models can benefit the functionality of corresponding original
packages to get more comprehensive results, such as importance plots,
prediction, etc. *NOTE* : The package is developed for linear/logistic
regression. For other models supported by their parent packages, the
weighted counterparts can be developed depending on demand.

FYI, all weighted performance metrics-- `wAUC`, `wROC`, `wSensitivity`,
and `wSpecificity`--can be computed by
R-[svyROC](https://github.com/aiparragirre/svyROC). All the ML methods
are incorporated with replicate weights methods: `JKn`, `bootstrap`,
`subbootstrap`, `BRR` by
R-[survey](https://r-survey.r-forge.r-project.org/survey/), `dCV`,
`split`, and `extrapolation` by
R-[surVarSel](https://github.com/aiparragirre/svyVarSel).

## Paper

- Kim, H., Rogers, P. and Wang, D. (2026) 'Machine learning models for
  chronic disease risks using complex survey data: the impact of sample
  weights', *Front. Digit. Health*, 8:1834715. [doi:
  10.3389/fdgth.2026.1834715](https://doi.org/10.3389/fdgth.2026.1834715)

## Installation

To install from GitHub,

``` r
library(devtool)
install_github("hkim-fda/MLSurvey")
```
