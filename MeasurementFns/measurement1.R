function(measOptions=NULL){
  measOptions <- measOptions

  f <- function(simulator){
    #Simulate full data
    fullCohortData <- list()
    for(cohortName in names(measOptions$cohorts)){
      n <- measOptions$cohorts[[cohortName]]$n
      fullCohortData[[cohortName]] <- simulator$simulate(n)
    }

    #Determine only observed data
    cohorts <- list()
    for(cohortName in names(measOptions$cohorts)){
      obsVars <- measOptions$cohorts[[cohortName]]$obs
      cohorts[[cohortName]] <- fullCohortData[[cohortName]]$d[,obsVars]
    }
    return(list(cohorts=cohorts,fullCohortData=fullCohortData))
  }
  return(f)
}