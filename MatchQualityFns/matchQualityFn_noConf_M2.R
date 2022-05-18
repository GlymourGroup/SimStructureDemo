function(matchQualOptions=NULL){
  matchQualOptions <- matchQualOptions
  calculateMetrics <- function(bestMatches,
                measuredData){
    diffData <- data.frame(M=rep(NA,nrow(bestMatches)))

    d_younger <- measuredData$fullCohortData$younger$d
    d_older <- measuredData$fullCohortData$older$d
    colnames(d_younger) <- paste0("younger_",colnames(d_younger))
    colnames(d_older) <- paste0("older_",colnames(d_older))
    d_younger$younger_ID <- 1:nrow(d_younger)
    d_older$older_ID <- 1:nrow(d_older)
    d_younger$older_ID <- bestMatches[,1]

    combinedData <- d_younger %>% left_join(d_older)
    diffData$younger_ID <- combinedData$younger_ID
    diffData$older_ID <- combinedData$older_ID
    diffData$M = combinedData$older_M - combinedData$younger_M
    diffData$M2 = combinedData$older_M2 - combinedData$younger_M2

    diffData$manhattan = diffData$M + diffData$M2
    diffData$euclidian = diffData$M^2 + diffData$M2^2
    return(diffData)
  }
  filterPoorQual <- function(diffData,
                             caliper,
                             qualMetric){
    diffData$quality <- abs(diffData[,qualMetric])
    diffData <- diffData %>%
      filter(quality <= caliper) %>%
      dplyr::select(younger_ID,older_ID,quality)
    return(diffData)
  }
  return(list(calculateMetrics=calculateMetrics,
              filterPoorQual=filterPoorQual))
}