fileProvider <- function(...) {
  # What are some things that you would need to locate files?
  # path
  # regular expression (subject? date? channel number? file extension? )
  # You might need several things and those needs may change.
  #
  # The available inputs may change, but the output must be the same:
  #    a valid "fileLocation" object that can return data files.
  # That last part is the important part! I don't have to complete some
  # template with this object; it just needs to be able to returrn
  # a list of data files to me.
  # That is an advantage of objects over "laundry lists"; i.e., as long as
  # this object can provide me with the list of files I need, then that
  # is all I care about!
  #
  # Perhaps it is better to think of the returned object as a "fileProvider";
  # you ask for "hasNext" and "next" and it provides.
  #' @export
  #' @examples
  #' \dontrun{
  #' }
  
  args <- list(...)
  
  L <- list()
  it <- NULL
  
  if ( 'path' %in% names(args) ) {path <- RFactories:::parseArg( args, 'path' ); args[['path']] <- path}
  if ( 'dirPath' %in% names(args) ) {path <- RFactories:::parseArg( args, 'dirPath' ); args[['path']] <- path}
  
  pattern <- glob2rx("*")
  if ( 'pattern' %in% names(args) ) {pattern <- glob2rx(RFactories:::parseArg( args, 'pattern' ) ); args[['pattern']] <- pattern}
  
  if ( !is.null(path) & !is.null(pattern) ) {
    L <- list.files(path=path,pattern=pattern,full.names = TRUE)
    it <- itertools::ihasNext( iterators::iter(L) )
  } else {
    print( "Please supply a path and a file pattern." )
    return()
  }

  # Make a custom iterator
  nextEl <- function() {
    filename <- iterators::nextElem( it )
    if ( 'filename' %in% names(args) ) {
      args[['filename']] <<- filename
    } else {
      args <<- append( args, list(filename=filename) )
    }
    return( filename )
  }
  
  hasNxt <- function() {
    return( itertools::hasNext(it) )
  }
  
  # Functions for class 'argumentComponent'
  isValid <- function( fieldname ) {
    if ( fieldname == 'channel' ) {
      return(TRUE)
    }
    if ( fieldname %in% names(args) ) {
      return(TRUE)    
    } else {
      return(FALSE)
    }    
  }
  get <- function( fieldname ) {
    if ( fieldname == 'channel' ) {
      if ( isValid('filename') ) {
        return( tools::file_path_sans_ext( basename( args[['filename']])) )
      } else {
        return( character(0) )
      }
    }
    if ( isValid(fieldname) ) {
      return( args[[fieldname]] )
    } else {
      return( character(0) )
    }
  }
  
  listFiles <- function() {
    L
  }
  
  # This is the standard way of returning an iterator, but it prevents adding other functiions.
  #obj <- ihasNext(it)
  #class(obj) <- c('ihasNext', 'abstractiter', 'iter', 'fileProvider', 'argumentComponent')
  #obj    
  
  # R, however, only cares about what functiions are defined. Any object with 'nextElem' and 'hasNext'
  # defind will act like an iterator.
  obj <- list( nextElem=nextEl, hasNext=hasNxt, isValid=isValid, get=get, listFiles=listFiles )
  class(obj) <- c( 'ihasNext', 'abstractiter', 'iter', 'fileProvider', 'argumentComponent' )
  obj
  
}

