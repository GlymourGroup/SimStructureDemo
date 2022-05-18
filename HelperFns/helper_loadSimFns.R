#Wrapper function: Orients functions inside it to the relevant directories ("dirs")
function(dirs){

  #Helper function returned by initializing the wrapper with the dirs
  f<-function(dags){

    #Set up a list to hold the functions
    simFns <- list()

    #Loop over variables in dags and load any simulation functions needed.
    for(dag in names(dags)){
      for(var in names(dags[[dag]])){
        simFnName <- dags[[dag]][[var]]$simFn

        #Do not load the functions that were already loaded
        if(!(simFnName %in% names(simFns))){
          simFns[[simFnName]] <- dget(file.path(dirs$SimFns,paste0(simFnName,".R")))
        }
      }
    }
    return(simFns)
  }

  return(f)
}