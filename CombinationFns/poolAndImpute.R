function(combOptions=NULL){
  library("mice")
  combOptions <- combOptions

  f <- function(cohorts){
    d <- plyr::rbind.fill(cohorts)
    imp <- mice(d,m=1)
    d <- complete(imp,1)
    return(list(d=d))
  }
  return(f)
}