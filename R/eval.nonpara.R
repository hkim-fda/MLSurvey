# 
eval.error <- function(data, l.yhat, method, cv.error.ind = c(TRUE, FALSE),
                    R = NULL, k = NULL, B = NULL,
                    y, objective, weights = NULL){

  if(method == "JKn"){
    R <- 1
    k <- length(grep("rw", colnames(data)))
    initial.name <- "sw"
    cv.error.ind <- TRUE
  }

  if(method %in% c("bootstrap", "subbootstrap")){
    R <- 1
    k <- B
    weights.name <- weights
    cv.error.ind <- FALSE
  }

  if(method == "dCV"){
    if(cv.error.ind){
      initial.name <- "sw"
    } else {
      initial.name <- "rw"
    }
  }

  if(method == "BRR"){
    R <- 1
    k <- length(l.yhat)
    initial.name <- "rw"
    cv.error.ind <- FALSE
  }

  if(method %in% c("split","extrapolation")){
    k <- 1
    initial.name <- "rw"
    cv.error.ind <- FALSE
  }

  if(!cv.error.ind){
    error.lambda.r <- rep(0,R*k)
    names(error.lambda.r) <- 1:(R*k)
  }

  l.loss <- l.loss.w <- list()

  for(r in 1:R){
    

    for(kk in 1:k){

      # Define the names of the weights' columns
      if(method %in% c("JKn", "dCV", "BRR", "bootstrap", "subbootstrap")){
        yhat.name <- paste0("yhat_rw_r_",r,"_train_",kk)
      }

      if(method %in% c("JKn", "dCV", "BRR")){
        weights.name <- paste0(initial.name, "_r_",r,"_test_",kk)
      }

      if(method %in% c("split","extrapolation")){
        yhat.name <- paste0("yhat_rw_r_",r,"_train")
        weights.name <- paste0(initial.name, "_r_",r,"_test")
      }
       idx<- which(data[,weights.name]>0)  
      #  print(length(idx))
      # Calculate the loss and the weighted loss
      l.loss[[yhat.name]] <- Error(l.yhat[[yhat.name]],y[idx],objective=objective)
      # apply(l.yhat[[yhat.name]], 2, loss.f, y = y, family = family)
      l.loss.w[[yhat.name]] <- l.loss[[yhat.name]]*data[idx,weights.name]
      # apply(l.loss[[yhat.name]], 2, function(x){x*data[, weights.name]})
      # print(paste0("test wloss", head(l.loss.w)))
    }

      # Calculate the error
      if(!cv.error.ind){

         rcol.yhat <- grep(paste0("r_",r,"_"), names(l.loss.w))
         rcol.rw.newdata <- grep(paste0(initial.name,"_r_",r,"_test"), names(data))
         
         sumwi.r <- apply(data[, rcol.rw.newdata],2,sum)
         sum.wi.li.r <- unlist(lapply(l.loss.w[rcol.yhat],sum))

        error.lambda.r[(r-1)*k+1:k] <- sum.wi.li.r/sumwi.r
        # apply(l.loss.w[[yhat.name]],2,sum)/sumwi.k
        names(error.lambda.r)[(r-1)*k+1:k] <- paste0(method, "_r_", r, "_k_", 1:k)
        # print(paste0("weighted.error", error.lambda.r))
      }

    

  }

  if(cv.error.ind){
            error.lambda.r <- rep(0,R)

    for(r in 1:R){
       
      rcol.yhat <- grep(paste0("r_",r,"_"), names(l.loss.w))
      rcol.sw.newdata <- grep(paste0(initial.name,"_r_",r,"_test"), names(data))
    

      sum.wi.li.r <- unlist(lapply(l.loss.w[rcol.yhat],sum))
      sum.wi.r <- apply(data[,rcol.sw.newdata],2,sum)
      error.lambda.r <- sum.wi.li.r/sum.wi.r

    }
  
  }

  return(error.lambda.r)
}




Error <- function(y.est,y,objective = c("reg:squarederror","binary:logistic")){

   if(objective == "reg:squarederror"){
          l <- (y.est - y)^2
    }

   if(objective == "binary:logistic"){
      
      l <- as.numeric(y!= y.est)
   }

   return(l)
}


