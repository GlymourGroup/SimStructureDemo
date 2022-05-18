function(dirs){
  f<-function(instructions){
    #Ensure no duplicate names
    filterNames <- names(instructions$matchQuality)
    if(length(filterNames) != length(unique(filterNames))){
      stop("Error: Duplicate match quality filter specifiaction names")
    }

    #Figure out what needs to be loaded and check that the instructions exist
    toLoad <- c()
    for(dag in instructions$dags){
      for(qualSpec in names(dag$matchQuality)){
        if(!(dag$matchQuality[[qualSpec]]$qualFn %in% names(instructions$matchQuality))){
          stop(paste0("Error: Missing match quality specification ",qualSpec))
        }
        toLoad  <- c(toLoad,qualSpec)
      }
    }

    #Load match quality functions
    qualFns <- list()
    for(qualName in toLoad){
      if(!(qualName %in% names(qualFns))){
        qualFnOptions <- instructions$matchQuality[[qualName]]$options
        qualFnName <- instructions$matchQuality[[qualName]]$qualFn
        qualFns[[qualName]] <- dget(file.path(dirs$MatchQualFns,paste0(qualFnName,".R")))(qualFnOptions)
      }
    }
    return(qualFns)
  }
  return(f)
}