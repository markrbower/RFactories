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
  args <- list(...)
  
  path <- NULL
  if ( 'path' %in% names(args) ) {path <- RFactories:::parseArg( args, 'path' ); args[['path']] <- NULL}
  if ( 'dirPath' %in% names(args) ) {path <- RFactories:::parseArg( args, 'dirPath' ); args[['dirPath']] <- NULL}
  
  if (is.null(path)) {
    print( "Please provide a path" )
    return;
  }

  pattern <- glob2rx("*")
  if ( 'pattern' %in% names(args) ) {pattern <- glob2rx(RFactories:::parseArg( args, 'pattern' ) ); args[['pattern']] <- NULL}
  
  L <- list.files(path=path,pattern=pattern)
  it <- itertools::ihasNext( iterators::iter(L) )

  # Make a custom iterator
  nextEl <- function() {
    iterators::nextElem( it )
  }
  
  ihasNext <- function(it) {
    if (!is.null(it$hasNext)) return(it)
    cache <- NULL
    has_next <- NA
    
    nextEl <- function() {
      if (!hasNx())
        stop('StopIteration', call.=FALSE)
      has_next <<- NA
      cache
    }
    
    hasNx <- function() {
      if (!is.na(has_next)) return(has_next)
      tryCatch({
        cache <<- iterators::nextElem(it)
        has_next <<- TRUE
      },
      error=function(e) {
        if (identical(conditionMessage(e), 'StopIteration')) {
          has_next <<- FALSE
        } else {
          stop(e)
        }
      })
      has_next
    }
    
    obj <- list(nextElem=nextEl, hasNext=hasNx)
    class(obj) <- c('ihasNext', 'abstractiter', 'iter')
    obj
  }
  
  # Functions for class 'argumentComponent'
  fieldnames <- c('path','pattern')
  isValid <- function( fieldname ) {
    if ( fieldname %in% fieldnames ) {
      return(TRUE)    
    } else {
      return(FALSE)
    }    
  }
  get <- function( fieldname ) {
    if ( isValid(fieldname) ) {
      return( eval(parse(text=paste0("get_",fieldname,"()"))) )
    } else {
      return( character(0) )
    }
  }

  obj <- ihasNext(it)
  class(obj) <- c('ihasNext', 'abstractiter', 'iter', 'fileProvider', 'argumentComponent')
  obj    
  
}

