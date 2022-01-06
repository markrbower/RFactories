argumentComposite <- function(...) {
  # Based on the "Builder" Design Pattern, but is it really a Builder? Does it matter?
  # I imagine this function has two tasks: 1) flexible components, 2) value checking.
  #
  # The components COULD include: fileProvider, databaseInformer, parameterInformer, and analysisInformer.
  #
  # Is it worthwhile to have a partial argumentBuilder? Probably: You could have an analysis without a database.
  #
  # The product is an Arguments "Composite" design pattern, which becomes the input to an analysis program.
  # An analysis program should access it's Arguments directly; e.g., args$subject, args$user, args$get_password().
  # How do I do this when it may not know exactly what is available? What about allowing a user to query for the
  # presence of specific components? What to do with the "fileProvider"? Do I download a specific handle by which
  # to refer to it? For example, fp <- args$fileProvider(); while ( hasNext(fp) ) { nextElem(fp) }.
  #' @export
  #' @examples
  #' \dontrun{
  #' }
  library(rlist)

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
  
  obj <- list(isValid=isValid,add=add,get=get,setIterationMax=setIterationMax,findClass=findClass,loadParameters=loadParameters,continue=continue)
  class(obj) <- c( 'argumentComposite' )
  return( obj )
}

