parameterInformer <- function(...) {
  # This is part of a set of functions that are to be used with analyses.
  # I am not really sure what this one should do!
  # This process is complicated by reading a parameter file, which includes
  # parameters that are somewhat fixed given the type of detected signal.
  # Those parameter values must be available to the user, too.
  # This precludes a fixed list of fieldnames for this class of object.
  # parms <- list(centerTime=0,range=c(-3000,2000), signalType="AP")

  # 1. parse all args
  args <- list(...)
  
  # 2. read through the args
  fieldnames <- names(args)

  # Functions for class 'argumentComponent'
  isValid <- function( fieldname ) {
    if ( fieldname %in% fieldnames ) {
      return(TRUE)    
    } else {
      return(FALSE)
    }    
  }
  get <- function( fieldname ) {
    if ( isValid(fieldname) ) {
      return( args[[fieldname]] )
    } else {
      return( character(0) )
    }
  }
  
  obj <- list(isValid=isValid,get=get)
  class(obj) <- c( 'parameterInformer', 'argumentComponent' )
  return( obj )
  
}
