parameterInformer <- function(...) {
  # This is part of a set of functions that are to be used with analyses.
  # I am not really sure what this one should do!
  # This process is complicated by reading a parameter file, which includes
  # parameters that are somewhat fixed given the type of detected signal.
  # Those parameter values must be available to the user, too.
  # This precludes a fixed list of fieldnames for this class of object.
  # parms <- list(centerTime=0,range=c(-3000,2000), signalType="AP")
  #' @export
  #' @examples
  #' \dontrun{
  #' }

  # 1. parse all args
  parameters <- list(...)
  args <- parameters[[1]]

  loadParameters <- function( dbp, aInf ) {
    if ( isValid("signalType") ) {
#      conn <- dbp$connect()
#      context <- topconnect::getContextFromTaskTable( conn, args )
      parameters <- append( parameters, RFactories:::loadParameters( dbp, args, aInf ) )
      # Incorporate these parameters into the object's list
      args <<- append( args, parameters )
#      DBI::dbDisconnect( conn )
    }
  }
  
  # Functions for class 'argumentComponent'
  isValid <- function( fieldname ) {
    if ( fieldname %in% names(args) ) {
      return(TRUE)
    } else {
      return(FALSE)
    }    
  }
  
  get <- function( fieldname ) {
    if ( isValid(fieldname) ) {
      value <- args[[fieldname]]
      if (class(value)=='list') {
        return( value[[1]])
      } else {
        return( value )
      }
    } else {
      return( character(0) )
    }
  }

  obj <- list(isValid=isValid,get=get,loadParameters=loadParameters)
  class(obj) <- c( 'parameterInformer', 'argumentComponent' )
  return( obj )
}
