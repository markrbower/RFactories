analysisInformer <- function(...) {
  # Exactly like parameterInformer, except it doesn't load the parameter file.
  #' @export
  #' @examples
  #' \dontrun{
  #' }
  
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
  add <- function( named_value ) {
    args[[names(named_value)]] <<- named_value
    fieldnames <<- names(args)
  }
  
  obj <- list(isValid=isValid,get=get,add=add)
  class(obj) <- c( 'analysisInformer', 'argumentComponent' )
  return( obj )
}


