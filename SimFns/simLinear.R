function(d,
         varName,
         parents,
         simOptions,
         u=NA){

  #Generate exogenous variable
  if(is.na(u)){
    u <- rnorm(nrow(d),0,sd=simOptions$coefs$U)
  }
  v <- u

  #Generate effects of each parent
  for(parent in parents){
    v <- v + simOptions$coefs[[parent]]*d[,parent]
  }

  #Add ixns here
  if("ixnCoefs" %in% names(simOptions)){
    for(ixnCoef in names(simOptions$ixnCoefs)){
      ixnCoefs <- simOptions$ixnCoefs[[ixnCoef]]
      parent1 <- ixnCoefs$var1
      parent2 <- ixnCoefs$var2
      v <- v + ixnCoefs$size*d[,parent1]*d[,parent2]
    }
  }

  #Return both the exogenous and endogenous variables to allow for counterfactual calculation
  return(list(v=v,u=u))
}