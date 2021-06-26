metadataInformer <- function(...) {
  # Holds data related to a the processing status of a single data file.
  # I was thinking of using the name "fileMetadataInformer", instead,
  # but that seemed too long.
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
  
  obj <- list(isValid=isValid,get=get)
  class(obj) <- c( 'metadataInformer', 'argumentComponent' )
  return( obj )
}

