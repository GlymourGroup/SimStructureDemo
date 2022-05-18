function(){
  messager.eng <- environment()
  status <- 1

  f <- list()
  f$setStatus <- function(status){
    assign("status",status,envir=messager.eng)
  }
  f$print <- function(message,status_threshold=1){
    if(status >= status_threshold){
      print(message)
    }
  }

  return(f)
}