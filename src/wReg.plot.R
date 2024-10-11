#' Plot weighted Elastic Net/LASSO object
#' 
#' @description
#' This plot function generates a graph of the optimal number of variables selected in the final model.
#' 
#' @param x an object of class "wlasso" or "w.elnet".  This is an extension of \code{wlasso::wlasso.plot()} 
#'
#' @return a graph
#' 
#' @importFrom graphics abline mtext
#' 
#' @examples
#' # For weighted LASSO by either wlasso or welnet function,
#' wlas <- wlasso(data = Mydata, col.y = "y", col.x = 1:50,
#'               family = "binomial", cluster = "cluster", strata = "strata",
#'               weights = "weights", method = "dCV", k=10, R=20)
#' wlasso.plot(wlas)
#' 
#'  # Equivalently,
#'  
#' wReg.plot(wlas)
#' 
#' # Or equivalently, with weighted Elastic Net,
#' wen <- welnet(data = Mydata, col.y = "y", col.x = 1:50,alpha =1,
#'               family = "binomial", cluster = "cluster", strata = "strata",
#'               weights = "weights", method = "dCV", k=10, R=20)
#' wReg.plot(wen)    
#'            
#' @export            
wReg.plot <- function(x){
  
  if(inherits(x, "w.elnet")){DF=x$model$final_model$df}
    else{DF=x$model$min$df}   
  plot(x = log(x$lambda$grid), y = x$error$average, col = "red", pch = 20,
       xlab = bquote("log("~lambda~")"), ylab = "Average error")
  abline(v = log(x$lambda$min), lty = 2, col = "black")
  mtext(text = paste0("The optimal number of variables: ", DF), side = 3)
  
}