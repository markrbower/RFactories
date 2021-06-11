temp <- function() {
  
  a <- 1
  
  get_a <- function() {
    return(a)
  }
  
  obj <- list( get_a=get_a )
  class(obj) <- c('temp')
  return(obj)
}