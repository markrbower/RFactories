inputPopup <- function() {
  library(svDialogs)
  
  user.input <- dlgInput("Enter a number", Sys.info()["user"])$res
  
  print( user.input )
}

