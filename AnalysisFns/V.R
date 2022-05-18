function(analysisOptions=NULL){
  f <- function(d){
    model <- lm(Y~V,d)
    est <- model$coefficients[["V"]]
    return(list(est=est))
  }
  return(f)
}