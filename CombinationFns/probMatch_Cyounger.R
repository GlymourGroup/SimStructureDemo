function(combOptions=NULL){
  combOptions <- combOptions

  f <- function(cohorts){
    #DEBUGGING ONLY
    #cohorts <- measuredData$cohorts
    #cohorts$younger <- cohorts$younger[1:5,]
    #cohorts$older <- cohorts$older[1:5,]
    #/DEBUGGING ONLY

    #Determine variables shared in the two cohorts
    sharedVars <- intersect(colnames(cohorts$younger),colnames(cohorts$older))

    #Calculate distances
    distanceMx <- matrix(0,
                         nrow=nrow(cohorts$younger),
                         ncol=nrow(cohorts$older))

    #Loop through young people and calculate their distances to older
    for(i in 1:nrow(cohorts$younger)){
      youngerVals <- cohorts$younger[i,sharedVars,drop=FALSE]
      temp <- cohorts$older[,sharedVars,drop=FALSE]
      for(var in sharedVars){
        temp[,var] <- abs(temp[,var]-youngerVals[1,var])
      }
      distances <- apply(temp,1,sum)

      distanceMx[i,] <- distances
    }

    #Select best matches
    bestMatches <- as.data.frame(matrix(nrow=nrow(cohorts$younger),
                                        ncol=combOptions$nMatches),
                                 drop=FALSE)
    colnames(bestMatches) <- paste0("older", 1:combOptions$nMatches)
    rownames(bestMatches) <- paste0("younger", 1:nrow(bestMatches))


    matches <- t(apply(distanceMx,1,function(x){order(x)}))
    lookup <- data.frame(younger=numeric(),
                         older=numeric())
    for(j in 1:combOptions$nMatches){
      matches_j <- matches[,j]
      bestMatches[,j] <- matches_j
      lookup <- rbind(lookup,
                      data.frame(younger_ID=1:nrow(bestMatches),
                                 older_ID=matches_j))
    }

    cohorts$younger$younger_ID <- 1:nrow(cohorts$younger)
    cohorts$older$ID <- 1:nrow(cohorts$older)

    colnames(cohorts$older) <- paste0("older_",colnames(cohorts$older))
    d <- cohorts$younger %>%
      left_join(lookup) %>%
      left_join(cohorts$older) %>%
      dplyr::rename(Y=older_Y)

    return(list(d=d,
                bestMatches=bestMatches,
                distanceMx=distanceMx))
  }
  return(f)
}