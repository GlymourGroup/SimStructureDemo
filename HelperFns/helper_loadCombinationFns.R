function(dirs){
  f<-function(instructions){
    #Ensure no duplicate names
    combNames <- names(instructions$combining)
    if(length(combNames) != length(unique(combNames))){
      stop("Error: Duplicate combination specifiaction names")
    }

    #Figure out what needs to be loaded and check that the instructions exist
    toLoad <- c()
    for(dag in instructions$dags){
      for(combinationSpec in dag$combining){
        if(!(combinationSpec %in% names(instructions$combining))){
          stop(paste0("Error: Missing combination specification ",combinationSpec))
        }
        toLoad  <- c(toLoad,combinationSpec)
      }
    }

    #Load combination functions
    combFns <- list()
    for(combName in toLoad){
      if(!(combName %in% names(combFns))){
        combOptions <- instructions$combining[[combName]]$options
        combFnName <- instructions$combining[[combName]]$combFn
        combFns[[combName]] <- dget(file.path(dirs$CombFns,paste0(combFnName,".R")))(combOptions)
      }
    }
    return(combFns)
  }
  return(f)
}