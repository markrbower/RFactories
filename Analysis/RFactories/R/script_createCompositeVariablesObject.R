script_createCompositeVariablesObject <- function() {
  #' @export
  dbp <- RFactories::databaseProvider(user="root",vault_user='localadmin',vault_key='NV_password',host='localhost',dbname='NV')
  if ( dir.exists('/Volumes/Data/NV/NVC1001_24_005_2') ) {
    fp <- RFactories::fileProvider(path='/Volumes/Data/NV/NVC1001_24_005_2',iterationType='directory',pattern="*.mef")
  } else if ( dir.exists('/Volumes/eplab/Raw_Data/NV_Human/NVC1001_24_005_2') ) {
    fp <- RFactories::fileProvider(path='/Volumes/eplab/Raw_Data/NV_Human/N_24_05_2',iterationType='directory',pattern="*.mef")
  } else if ( dir.exists('/Users/markrbower/Dropbox/Documents/Concepts/2021_06_06_RFactories/RFactories/Analysis/RFactories/Data') ) {
    fp <- RFactories::fileProvider(path='/Users/markrbower/Dropbox/Documents/Concepts/2021_06_06_RFactories/RFactories/Analysis/RFactories/Data',iterationType='directory',pattern="*.mef")
  } else {
    print( "Cannot find a directory for data" )
    fp <- NULL
  }
  aInf=RFactories::analysisInformer(experiment='NV',subject='24_005',centerTime=0,pattern="*.mef",lab='RNCP')
  pInf <- RFactories::parameterInformer(signalType='IIS')
  pInf$loadParameters( databaseProvider=dbp, analysisInformer=aInf )
  compVars <- RFactories::compositeVariables(databaseProvider=dbp, fileProvider=fp, analysisInformer=aInf, parameterInformer=pInf )
  return( compVars )
}

