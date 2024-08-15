#' Weighted mean error of binary outcome for complex survey data
#'
#' @description
#' A function to evaluate XGBoost models by weighted mean error for binary outcome (`0/1`).  This function is a customized error function 
#' for complex survey data.
#' 
#' @param preds Predicted binary values (`0/1`) from the model.
#' @param dtrain An object of \code{xgb.DMatrix} including `label`(response variable or binary outcome), `data`,`weight`.
#' @return A list of two components: 
#' - `metric` : A string labelled as `weighted.error`.
#' - `value`  : A numeric value of weighted mean error.
#' 
#' @seealso [xgboost::xgb.train()] for relevant arguments and return values in detail. 
#' @seealso [wxgboost()] for detailed information on implementation.
#' 
#' @examples
#' # Do not run!!
#'  Mat<- sparse.model.matrix(y~. ,data = MyData_unweighted)[,-1] #dropping intercept
#'  wtData <- xgb.DMatrix(data = Mat,label=Mydata$y,weight=Mydata$weights) 
#'  wxgb<-xgb.train(data=wtData, params=opt.par$params, nrounds=opt.para$nrounds,feval = evalerror.bin)
#'
#'@export
evalerror.bin<-function(preds,dtrain){ 
  labels<-getinfo(dtrain,"label") 
  weights <- getinfo(dtrain,"weight")
  err <- stats::weighted.mean(labels!=(preds>0),weights,na.rm=TRUE)
  
  return(list(metric="weighted.error", value=err)) }


#' Weighted mean loss of logistic regression for complex survey data
#'
#' @description
#' A function to evaluate XGBoost models by weighted mean loss for logistic regression. This function estimates predicted 
#' probabilities from the predicted binary outcomes and computes the weighted mean of log likelihood values for Bernoulli distribution. 
#' This function is a customized error function for complex survey data.
#' 
#' @param preds Predicted binary values (`0/1`) from the model.
#' @param dtrain An object of \code{xgb.DMatrix} including `label`(response variable or binary outcome), `data`,`weight`.
#' @return A list of two components:
#' - `metric`: A string labelled as `weighted.mean.logloss`.
#' - `value` : A numeric value of weighted mean of log loss.
#' @seealso [xgboost::xgb.train()] for relevant arguments and return values in detail. 
#' @seealso [wxgboost()] for detailed information on implementation.
#' 
#' @examples
#' # Do not run!!
#'  Mat<- sparse.model.matrix(y~. ,data = MyData_unweighted)[,-1] #dropping intercept
#'  wtData <- xgb.DMatrix(data = Mat,label=Mydata$y,weight=Mydata$weights) 
#'  wxgb<-xgb.train(data=wtData, params=opt.par$params, nrounds=opt.para$nrounds,feval = eval.loss)
#' @export
eval.loss<- function(preds,dtrain){
  labels<-getinfo(dtrain,"label") 
  weights <- getinfo(dtrain,"weight")
  preds <- 1/(1+exp(-preds))
  loss<- Loss(preds,labels,"binary:logistic")
  wloss <- stats::weighted.mean(loss,weights,na.rm=TRUE)
  # wloss<- sum(loss*weights)/sum(weights)
  return(list(metric="weighted.mean.logloss",value=wloss))
}  


#' Weighted mean squared error for complex survey data
#'
#' @description
#'  A function to evaluate XGBoost models by weighted mean squared error for linear regression.  
#'  This function is a customized error function for complex survey data.
#' 
#' @param preds Predicted values from the model
#' @param dtrain An object of \code{xgb.DMatrix} including `label`(response variable), `data`,`weight`.
#' @return A list of two components:
#'- `metric`: A string labelled as `weighted.mse`.
#' - `value` : A numeric value of weighted mean squared error (MSE).
#' @seealso [xgboost::xgb.train()] for relevant arguments and return values in detail. 
#' @seealso [wxgboost()] for detailed information on implementation.
#' 
#' @examples
#' # Do not run!!
#'  Mat<- sparse.model.matrix(y~. ,data = MyData_unweighted)[,-1] #dropping intercept
#'  wtData <- xgb.DMatrix(data = Mat,label=Mydata$y,weight=Mydata$weights) 
#'  wxgb<-xgb.train(data=wtData, params=opt.par$params, nrounds=opt.para$nrounds,feval = evalerror.reg)
#'
#' @export
evalerror.reg <- function(preds,dtrain){
  labels <- getinfo(dtrain,"label")
  weights<- getinfo(dtrain,"weight")
  err<- stats::weighted.mean((labels-preds)^2,weights,na.rm=TRUE)
  
  return(list(metric="weighted.mse", value=err))
  
}

Loss <- function(y.est, y, objective = c("reg:squarederror","binary:logistic")){
  
  if(objective == "reg:squarederror"){
    l <- (y.est - y)^2
  }
  
  if(objective == "binary:logistic"){
    
    
    l <- rep(NA, length(y))
    l[which(y==1)] <- -log(y.est[which(y==1)])
    l[which(y==0)] <- -log(1-y.est[which(y==0)])
    
  }
  
  return(l)
  
}
