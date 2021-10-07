metadataInformer <- function( filename, compArgs ) {
  # Holds data related to a the processing status of a single data file.
  # I was thinking of using the name "fileMetadataInformer", instead,
  # but that seemed too long.
  #' @export
  #' @examples
  #' \dontrun{
  #' }
  

  # 1. parse all args
  args <- list(filename=filename)
  
  # Functions for class 'argumentComponent'
  isValid <- function( fieldname ) {
    if ( fieldname %in% names(args) ) {
      return(TRUE)    
    } else {
      return(FALSE)
    }    
  }
  
  get <- function( fieldname ) {
    if ( fieldname == 'service' ) {
      fieldname <- 'dbname'
    }
    if ( isValid(fieldname) ) {
      return( args[[fieldname]] )
    } else {
      return( character(0) )
    }
  }

  set <- function( name, value) {
    args[[name]] <<- value
  } 
  
  getPassword <- function( compArgs ) {
    password_key <- paste0( compArgs$get('dbname'), '_password' )
    vault <- topsecret::get_secret_vault()
    if ( !( tolower(password_key) %in% tolower(secret::list_secrets(vault=vault)$secret))) {
      password_key <- paste0( compArgs$get('dbname'), '_password' )
      vault <- topsecret::get_secret_vault()
      add_secret(name=password_key,value=readline("Enter MEF file password: "),users=Sys.info()['user'],vault=vault)
    }
    value <- secret::get_secret(name=password_key,key=secret::local_key(),vault=vault)
    return( value )
  }

  # If args contains a "filename" field, strip of the last string as the "channel".
  if ( 'filename' %in% names(args) ) {
    filename <- args[['filename']]
#    args <- append( args, list( channel=basename(filename) ) )
    info <- meftools::mef_info( c(filename, getPassword( compArgs )) )
    args <- append( args, list( info=info ) )
  }

  obj <- list(isValid=isValid,get=get,set=set,getPassword=getPassword)
  class(obj) <- c( 'metadataInformer', 'argumentComponent' )
  return( obj )
}

