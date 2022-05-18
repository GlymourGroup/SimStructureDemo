function(dirs){
  f<-function(instructions){
    #Ensure no duplicate names
    analysisNames <- names(instructions$analysis)
    if(length(analysisNames) != length(unique(analysisNames))){
      stop("Error: Duplicate analysis specifiaction names")
    }

    #Figure out what needs to be loaded and check that the instructions exist
    toLoad <- c()
    for(dag in instructions$dags){
      for(measurementName in names(dag$measurement)){
        for(analysisSpec in dag$measurement[[measurementName]]){
          if(!(analysisSpec %in% names(instructions$analysis))){
            stop(paste0("Error: Missing analysis specification ",analysisSpec))
          }
          toLoad  <- c(toLoad,analysisSpec)
        }

      }
    }

    #Load combination functions
    analysisFns <- list()
    for(analysisName in names(instructions$analysis)){
      if(!(analysisName %in% names(analysisFns))){
        analysisFnName <- instructions$analysis[[analysisName]]$analysisFn
        analysisOptions <- instructions$analysis[[analysisName]]$options
        analysisFns[[analysisName]] <- dget(file.path(dirs$AnalysisFns,paste0(analysisFnName,".R")))(analysisOptions)
      }
    }
    return(analysisFns)
  }
  return(f)
}