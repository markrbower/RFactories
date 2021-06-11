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
  
  components <- list()
  
  add <- function( x ) {
    if ( "argumentComponent" %in% class(x) ) {
      # Look through all field names and find if any existing component
      # contains the same field. If so, they must hold the same value.
      L <- length(components)
      components[[L+1]] <<- x
    }
  }
  
  get <- function( fieldName ) {
    # Run through each component until you find one with a field called "fieldname"    
        
  }

  findClass <- function( className ) {
    # Useful for returning a "fileProvider", which is a custom iterator over filenames.   
    it <- ihasNext( iter( components ) )
    while ( hasNext(it) ) {
      component <- nextElem( it )
      if ( "argumentComponent" %in% class(component) ) {
        return( component )
      }
    }
    return( character(0) )    
  }
  
  obj <- list(add=add,get=get,findClass=findClass)
  class(obj) <- c( 'argumentComponent' )
  return( obj )
}

