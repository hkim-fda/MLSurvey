## CI for wAUC by bootstrap

library(wROC)
library(survey)
options(survey.lonely.psu='remove')

### weights,cluster,strata are names in data (character) unless design is included.
#set.seed(234)
ci.wauc<- function(y,yhat,tag.event = "1", tag.nonevent = "0",weights=NULL,cluster=NULL,strata=NULL,design=NULL,data=NULL,
                   type=c("JK1","JKn", "bootstrap", "subbootstrap","mrbbootstrap", "BRR"),B=2000,conf.level=0.95, 
                   fpc=NULL,fpctype=NULL,..., compress=TRUE,
                   mse=getOption("survey.replicates.mse")){
  
  
  # Step 0: Notation
  if(!is.null(design)){
    # cluster <- as.character(design$call$id[2])
    # if(cluster == "1" || cluster == "0"){
    #   cluster <- NULL
    # }
    # strata <- as.character(design$call$strata[2])
    weights <- as.character(design$call$weights[2])
    # data <- get(design$call$data)
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
  }
  
  # Define the design
  design <- survey::svydesign(ids = formula.cluster,
                           strata = formula.strata,
                           weights = formula.weights,
                           data = data, nest=TRUE)
  
  }
  # Generate replicate weights based on the selected method
  
    if(type %in% c("bootstrap", "subbootstrap","mrbbootstrap")){
      rep.des <- survey::as.svrepdesign(design = design, type = type, replicates = B)
    } else {
      rep.des <- survey::as.svrepdesign(design = design, type = type)
      B<-dim(rep.des$repweights$weights)[2]
    }
  
      rep.weights<-rep.des$repweights$weights
      WTs<-design$variables[,weights]
  # Compute wAUC replicates by the selected method    
      wauc.B<- rep(0,B)
      for(b in 1:B){
        
        wauc.B[b]<- wROC::wauc(response.var=y, phat.var=yhat, weights.var = WTs*rep.weights[,b], 
                         tag.event = tag.event, tag.nonevent = tag.nonevent)$AUCw
        
      }
      CI= quantile(wauc.B,c(0+(1-conf.level)/2,.5,1-(1-conf.level)/2))
      
  return(CI)
  
 }