compositeVariables <- function(...) {
  #' @export
  #' 
  library(rlist)
  
  inputArguments <- list(...)

  iterationCount <- 0
  components <- list()
  
  add <- function( x ) {
    if ( "argumentComponent" %in% class(x) ) {
      # Look through existing components to see if any match this class.
      # If so, overwrite them ...
      isAbsent <- TRUE
      for ( idx in seq_along(components) ) {
        if ( setequal( class(components[[idx]]), class(x) ) ) {
          components[[idx]] <<- x
          isAbsent <- FALSE
        }
      }
      if ( isAbsent ) {
        components <<- rlist::list.prepend( components, x )
      }
    }
  }
  
  isValid <- function( fieldname ) {
    # Run through each component until you find one with a field called "fieldname"    
    for ( component in components ) {
      if ( component$isValid( fieldname ) ) {
        return( TRUE )
      }
    }
    return( FALSE )
  }
  
  get <- function( fieldname ) {
    # Run through each component and return the one named 'fieldname'   
    for ( component in components ) {
      if ( component$isValid( fieldname ) ) {
        #        print( class(component) )
        return( component$get( fieldname ) )
      }      
    }
    return( NULL )
  }
  
  replace <- function( fieldname, value ) {
    # Run through each component and return the one named 'fieldname'   
    for ( component in components ) {
      if ( component$isValid( fieldname ) ) {
        component[[fieldname]] <- value
      }      
    }
    return( 1 )
  }
  
  remove <- function( fieldname ) {
    # Run through each component and return the one named 'fieldname'   
    for ( component in components ) {
      if ( component$isValid( fieldname ) ) {
        #        print( class(component) )
        return( component$get( fieldname ) )
      }      
    }
    return(  )
  }
  
  setIterationMax <- function() {
    tmp <- get( "iterationMax" )
    if ( !is.null(tmp) ) {
      iterationMax <<- tmp
    }
  }
  
  # Add a database reading object here ...
  # This is for "parametereInforrmer" object!
  loadParameters <- function() {
    # This requires the presence of BOTH a paramaterInformer AND a databaseInformer.
    if ( findClass('parameterInformer') & findClass('databaseInformer') ) {
      pi <- findClass('parameterInformer')
      di <- findClass('databaseInformer')
      conn <- di$connect()
      pi$loadParameters( pi )
    }
  }
  
  findClass <- function( className ) {
    # Useful for returning a "fileProvider", which is a custom iterator over filenames.   
    it <- itertools::ihasNext( iterators::iter( components ) )
    while ( itertools::hasNext(it) ) {
      component <- iterators::nextElem( it )
      if ( className %in% class(component) ) {
        return( component )
      }
    }
    return( character(0) )    
  }
  
  continue <- function() {
    iterationCount <<- iterationCount + 1
    if ( iterationMax > 0 ) {
      if ( iterationCount <= iterationMax ) {
        return( TRUE )
      } else {
        return( FALSE )
      }
    } else {
      return( TRUE )
    }
  }
  
  new_compositeVariables <- function( args ) {
    stopifnot(is.list(args))
    components <<- args
    obj <- list(isValid=isValid,add=add,get=get,setIterationMax=setIterationMax,findClass=findClass,loadParameters=loadParameters,continue=continue)
    class(obj) <- c( 'compositeVariables' )
    return(obj)
  }
  
  variables <- new_compositeVariables(inputArguments)
  return(variables)
}