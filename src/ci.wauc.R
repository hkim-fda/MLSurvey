## CI for wAUC by bootstrap

library(wROC)
library(survey)
options(survey.lonely.psu='remove')

#set.seed(234)
ci.wauc<- function(y,yhat,tag.event = 1, tag.nonevent = 0,weights=NULL,cluster=NULL,strata=NULL,design=NULL,
                   type=c("JKn", "bootstrap", "subbootstrap", "BRR"), 
                   fpc=NULL,fpctype=NULL,..., compress=TRUE,
                   mse=getOption("survey.replicates.mse"),B,conf.level=0.95){
  
  
  # Step 0: Notation
  if(!is.null(design)){
    cluster <- as.character(design$call$id[2])
    if(cluster == "1" || cluster == "0"){
      cluster <- NULL
    }
    strata <- as.character(design$call$strata[2])
    weights <- as.character(design$call$weights[2])
    data <- get(design$call$data)
  } else {
  
  # Define cluster formula
  if(is.null(cluster)) {
    formula.cluster <- as.formula("~1")
  } else {
    formula.cluster <- as.formula(paste0("~", cluster))
  }
  
  # Define strata formula
  if(!is.null(strata)) {
    formula.strata <- as.formula(paste0("~", strata))
  }
  
  # Define weights formula
  if(!is.null(weights)){
    formula.weights <- as.formula(paste0("~", weights))
  }}
  
  # Define the design
  des <- survey::svydesign(ids = formula.cluster,
                           strata = formula.strata,
                           weights = formula.weights,
                           data = data, nest=TRUE)
  
  
  # Generate replicate weights based on the selected method
  if(type %in% c("JKn", "bootstrap", "subbootstrap", "BRR")){
    
    if(type %in% c("bootstrap", "subbootstrap")){
      rep.des <- survey::as.svrepdesign(design = des, type = type, replicates = B)
    } else {
      rep.des <- survey::as.svrepdesign(design = des, type = type)
    }
  }
      rep.weights<-rep.des$repweights$weights
  
  # Compute wAUC replicates by the selected method    
      wauc.B<- rep(0,B)
      for(b in 1:B){
        
        wauc.B[b]<- wROC::wauc(response.var=y, phat.var=yhat, weights.var = weights*rep.weights[,b], 
                         tag.event = tag.event, tag.nonevent = tag.nonevent)$AUCw
        
      }
      CI= quantile(wauc.B,c(0+(1-conf.level)/2,.5,1-(1-conf.level)/2))
      
  return(CI)
  
 }