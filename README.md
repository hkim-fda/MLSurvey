# MLSurvey

## *M*achine *L*earning for Complex *S*urvey Data

*MLSurvey* is a R-package for developing Machine Learning models for complex survey data using appropriate Cross-
Validation methods: replicate weights methods.  This ML tool currently offers weighted Elastic Net (wElNet),
weighted random forest (wRandomforest), and weighted XGBoost (wXGBoost) for linear/logistic regression incorporated
with replicate weights methods by R-[survey](https://r-survey.r-forge.r-project.org/survey/) and 
R-[surVarSel](https://github.com/aiparragirre/svyVarSel).  It also provides the visualization for optimal variable 
selection for wElNet (the extension of R-surVarSel), computation of confidential interval (CI) for 
weighted area under the ROC curve (wAUC) based on R-[svyROC](https://github.com/aiparragirre/svyROC).  
Since all the ML methods are extended by existing algorithms, [glmnet](https://github.com/cran/glmnet), 
[xgboost](https://github.com/dmlc/xgboost/tree/master), and 
[randomForest](https://www.stat.berkeley.edu/~breiman/RandomForests/), the final ML models can take advantage of 
their corresponding original packages to get more comprehensive results, such as importance plots, prediction, etc.

**Note**: This will be extended to wider model developments, such as Cox, Poisson, etc. and will be available for Julia programming language. 

## Installation

To install from GitHub,

```r
library(devtool)
install_github("hkim-fda/MLSurvey")
```
