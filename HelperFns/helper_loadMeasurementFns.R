function(dirs){
  f<-function(instructions){
    #Ensure no duplicate names
    measNames <- names(instructions$measurement)
    if(length(measNames) != length(unique(measNames))){
      stop("Error: Duplicate measurement specifiaction names")
    }

    #Figure out what needs to be loaded and check that the instructions exist
    toLoad <- c()
    for(dag in instructions$dags){
      for(measurementSpec in names(dag$measurement)){
        if(!(measurementSpec %in% names(instructions$measurement))){
          stop(paste0("Error: Missing measurement specification ",measurementSpec))
        }
        toLoad  <- c(toLoad,measurementSpec)
      }
    }

    #Load combination functions
    measFns <- list()
    for(measName in names(instructions$measurement)){
      if(!(measName %in% names(measFns))){
        meas <- instructions$measurement[[measName]]
        measFnName <- meas$measFn
        measOptions <- meas$options
        measFns[[measName]] <- dget(file.path(dirs$MeasFns,paste0(measFnName,".R")))(measOptions)
      }
    }
    return(measFns)
  }
  return(f)
}