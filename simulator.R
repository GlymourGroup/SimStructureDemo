function(options,
         dag,
         simFns,
         msg){
  calcVarOrder <- function(dag){
    #Calculate the order for simulating
    remaining <- names(dag)
    result <- c()
    iter <- 0
    while(length(remaining)>0 & iter < 100){
      for(var in names(dag)){
        if(prod(dag[[var]]$parents %in% result)==1){
          if(!(var %in% result)){
            result <- c(result,var)
            remaining <- setdiff(remaining,var)
          }
        }
      }
      iter <- iter + 1
    }
    if(iter==100){
      stop("Error: couldn't calculate variable simulation order. Check graph for cycles.")
    }
    msg$print("Variable Order:")
    msg$print(result)
    return(result)
  }
  varOrder <- calcVarOrder(dag)

  f <- list()
  f$simulate <- function(n,
                         interventions=list(),
                         uMx=NA){
    #Set up data frame to hold simulated data
    d <- data.frame(matrix(nrow=n,ncol=length(dag)))
    colnames(d) <- names(dag)

    #Set up data frame to hold random error terms
    u <- d

    #Simulate each variable
    for(var in varOrder){
      if(is.na(uMx)){
        uVec <- NA
      }else{
        uVec <- uMx[,paste0("U_",var)]
      }

      varSim <- simFns[[dag[[var]]$simFn]](d,
                                           var,
                                           dag[[var]]$parents,
                                           dag[[var]]$simOptions,
                                           u=uVec)

      #Apply interventions
      if(var %in% names(interventions)){
        d[,var] <- interventions[[var]]
      }else{
        d[,var] <- varSim$v
      }
      u[,var] <- varSim$u
    }

    #Format and output
    colnames(u) <- paste0("U_",colnames(u))
    return(list(d=d,u=u))
  }
  return(f)
}