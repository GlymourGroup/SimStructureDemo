function(aggOptions=NULL){
  aggOptions <- aggOptions
  f <- function(estimates,truth, simFails, ns){
    results <- estimates[0,]
    results["truth",] <- truth
    results["mean",] <- apply(estimates,2,mean,na.rm=TRUE)
    results["bias",] <- results["mean",] - truth
    results["var",] <- apply(estimates,2,var,na.rm=TRUE)
    results["mse",] <- results["bias",]^2 + results["var",]
    results[c("lb","ub"),]<- apply(estimates,2,quantile,probs=c(0.025,0.975))
    results["bias_lb",] <- results["lb",]-truth
    results["bias_ub",] <- results["ub",]-truth
    results["percBias",] <- 100*results["bias",]/results["truth",]
    results["propNA",] <- apply(estimates,2,is.na)/nrow(estimates)
    results["propSimFail",] <- apply(simFails,2,mean,na.rm=FALSE)
    results["mean_n",] <- apply(ns,2,mean,na.rm=FALSE)
    results["sd_n",] <- apply(ns,2,sd,na.rm=FALSE)
    results["min_n",] <- apply(ns,2,min,na.rm=FALSE)
    results["max_n",] <- apply(ns,2,max,na.rm=FALSE)

    return(results)
  }
  return(f)
}