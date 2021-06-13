databaseProvider <- function(...) {

  # Ideally, this object is an intermediary between the user and the "topconnect" package.
  # This object accepts user input and determines whether all required fields are present.
  #
  # An RMySQL database connection requires four inputs: user, password, host, dbname
  
  # There are seven possible inputs:
  # project:    the name of the project (default: working directory name)
  # db_user:    the username for database access (default: Sys.info$user)
  # vault_user: the username for vault access (default: Sys.info$user)
  # vault_key : the keyword for the vault secret (default: <project>_password)
  # host:       the name of the database computer/server (default: localhost)
  # dbname:     the name of the database to access (default: <project>)
  # password:   NOT RECOMMENDED, but this would provide a non-encrypted password
    
  
  # There are two special aspects that make this non-trivial:
  #   project, which allows the user to store and retrieve connection information under a single key
  #     If the user supplies a “project” name, there are two possible cases:
  #       associate the given connection values with this project
  #       retrieve a persisted tuple of connection information.
  #   vault, which allows the user to user passwords securely.
  #     If the user supplies the “vault_user”, but no “vault_key”,
  #     then this object could request the user supply the password via a pop-up.
  
  # dbi <- databaseInformer(user='markrbower',host='localhost',dbname='NV',vault_user='markrbower',vault_key="MEF_password")
  # conn <- topconnect::db(user=dbi$user(),password=dbi$password(),host=dbi$host(),dbname=dbi$dbname() )
  
  
  # 1. parse all args
  args <- list(...)
  # user
  user <- NULL
  if ( 'user' %in% names(args) ) { user <- RFactories:::parseArg( args, 'user' ); args[['user']] <- NULL}
  if ( 'dbduser' %in% names(args) ) { user <- RFactories:::parseArg( args, 'dbuser' ); args[['dbuser']] <- NULL}

  # password 
  password <- NULL
  vault_user <- NULL
  vault_key <- NULL
  if ( 'password' %in% names(args) ) { password <- RFactories:::parseArg( args, 'password' );args[['password']] <- NULL}
  if ( 'vault_user' %in% names(args) ) {
    if ( 'vault_key' %in% names(args) ) {
      # If both are available, store them for future use. Don't store the password!
      vault_user <- RFactories:::parseArg( args, 'vault_user' )
      args[['vault_user']] <- NULL
      vault_key  <-  RFactories:::parseArg( args, 'vault_key' )
      args[['vault_key']] <- NULL
    }
  }

  # host\
  host <- NULL
  if ( 'host' %in% names(args) ) { host <- RFactories:::parseArg( args, 'host' ); args[['host']] <- NULL }
  
  # dbname
  dbname <- NULL
  if ( 'dbname' %in% names(args) ) { dbname <- RFactories:::parseArg( args, 'dbname' ); args[['dbname']] <- NULL }

  # 2. check for unused args and give a warning
  if ( length(args) > 0 ) {
    print( paste0( "Leftover arguments: ", args, "\n" ) )
  }

  # 3. check if either of the needed conditions for passwords is satisfied
  if ( ! (!is.null(password) || ( !is.null(vault_user) & !is.null(vault_key) ) ) ) {
    cat( "databaseInformer :: Insufficient information to retrieve a password!\n" )
    cat( "Please supply either a 'password' or a valid 'vault_user' and 'vault_key'.")
  }

  get_user <- function() { return(user) }  
  get_host <- function() { return( host ) }
  get_dbname <- function() { return( dbname ) }
  get_password <- function() {
    if ( !is.null(password) ) {
      return( password )
    } else if ( !is.null(vault_user) & !is.null(vault_key) ) {
      # Is the secret in the vault?
      vault <- topsecret::get_secret_vault()
      if ( length( secret::list_owners(name=vault_key,vault) ) > 0 ) {
        # Do you have access to it?
        if ( vault_user==secret::list_owners(name=vault_key,vault) ) {
          return( secret::get_secret( name=vault_key,
                                      key=secret::local_key(),
                                      vault=vault ) )
        } else {
          cat( "The user ", vault_user, " does not have access to the secret ", vault_key, "\n" )
          return( character(0) )
        }
      } else { # no secret exists with the given key
        cat( "No password exists for the vault_key, ", vault_key, "\n" )
        cat( "Re-run with no password, vault_user or vault_key to enter a new key-value pair")
        return( character(0) )
      }
    } else { # Prompt the user to store a new entry
      user.input <- dlgInput("Enter a user name", Sys.info()["user"])$res
      vault_user <- user.input
      user.input <- dlgInput("Enter a key name", Sys.info()["this_password"])$res
      vault_key <- user.input
      secret::add_secret( name=vault_key,
                          value=dlgInput("Enter a key value", Sys.info()["user"])$res,
                          users=vault_user,
                          vault=topsecret::get_secret_vault() )
    }
  }
  
  getConnection <- function() {
    return( topconnect::db(user=user,password=get_password(),host=host,dbname=dbname) )
  }
  
  valid <- function() {
    flag <- TRUE
    tryCatch({
      topconnect::db(user=user(),host=host(),dbname=dbname(),password=password() )
    }, error=function(error_condition) {
      error_condition <- 0
      flag <- FALSE 
    })
    return( flag )
  }
  
  # Functions for class 'argumentComponent'
  fieldnames <- c('user','dbname','password','host')
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

  obj <- list(user=get_user,host=get_host,dbname=get_dbname,password=get_password,valid=valid,isValid=isValid,get=get,connect=getConnection)
  class(obj) <- c('databaseInformer', 'argumentComponent' )
  return( obj )
}


