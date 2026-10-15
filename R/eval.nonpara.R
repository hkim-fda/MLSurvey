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
    # initial.name is "sw" because replicate_weights() generates
    # out-of-bag (OOB) test weight columns for bootstrap/subbootstrap using
    # the same "sw_r_1_test_*" naming convention as JKn (a unit's original
    # sampling weight is assigned to the test column iff that unit's PSU
    # was NOT drawn into the given replicate; 0 otherwise). weights.name is
    # NOT fixed here to the plain 'weights' column -- it must be
    # reconstructed per-fold below (see the loop), exactly like JKn/dCV/BRR,
    # so that each of the B replicates' error is computed against its own
    # OOB held-out units rather than against the entire original sample.
    #
    # NOTE: as of this version, wRandomforest() (wrf.R) explicitly rejects
    # method = "bootstrap"/"subbootstrap" before eval.error() is ever
    # called, so this branch is not currently reachable from wrf.R. It is
    # fixed here anyway for correctness, since eval.error() is a standalone
    # utility that could be reused or called directly elsewhere, and to
    # keep this file consistent with the equivalent fix applied to
    # error.f() (used by wElnet()).
    initial.name <- "sw"
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

      # Reconstruct weights.name per-fold for every method with genuine
      # fold-specific OOB/held-out test weights. This now includes
      # bootstrap/subbootstrap -- previously omitted here, which caused
      # weights.name to remain fixed at the plain full-sample 'weights'
      # column for all B replicates (see note above).
      if(method %in% c("JKn", "dCV", "BRR", "bootstrap", "subbootstrap")){
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
    

      # BUG FIX: previously `sum.wi.li.r <- unlist(lapply(l.loss.w[rcol.yhat],sum))`
      # and `sum.wi.r <- apply(data[,rcol.sw.newdata],2,sum)` each returned a
      # vector of length k (one entry per fold), and dividing them produced
      # a k-length vector of PER-FOLD ratios rather than one pooled value.
      # Assigning that k-length vector into the single slot
      # error.lambda.r[r] then failed with "number of items to replace is
      # not a multiple of replacement length" whenever k > 1 (e.g. JKn with
      # more than one PSU). The correct pooled-error calculation (matching
      # error.f()'s equivalent branch) sums the per-fold weighted losses
      # and per-fold weight totals ACROSS ALL FOLDS FIRST, then divides --
      # producing one true pooled scalar per replicate r, as the pooling
      # strategy (cv.error.ind = TRUE) is supposed to do.
      sum.wi.li.r <- sum(unlist(lapply(l.loss.w[rcol.yhat],sum)))
      sum.wi.r <- sum(data[,rcol.sw.newdata])
      error.lambda.r[r] <- sum.wi.li.r/sum.wi.r

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
