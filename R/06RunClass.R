#' @include 04ControlClass.R 
#' @include 01BaseClass.R
#' @include 05ReportingClass.R
NULL

#' the MAIN execution function
#' 
#' for simple exploration preproviz() takes data frame (one factor variable, other variables numeric)
#' as an argument. Two data sets can be compared by providing them as a list. For complex setups a
#' ControlClass object can be passed as an argument. See Vignette for examples. The output can be
#' visualized with PLOT functions.

#' 
#' @param controlobject (data frame/list/ControlClass object) 
#' @return (RunClass) object
#' @examples 
#' ## result1 <- preproviz(iris)
#' ## plotDENSITY(result1)
#' ##
#' ## iris2 <- iris
#' ## iris2[sample(1:150,30),1] <- NA
#' ## result2 <- preproviz(list(iris, iris2))
#' ## plotVARCLUST(result2)
#' @export

preproviz <- function(controlobject){
   
    # initializing defaul objects in case of data frame as controlobject argument
    
    if(class(controlobject)=="data.frame"){
    
    rte <- new.env(parent = emptyenv())
    
    dataobject <- initializedataobject(controlobject)
    assign("dataobject", dataobject, envir=rte)
    
    setupobject <- initializesetupclassobject("setupobject", defaultParameters, dataobject)
    assign("setupobject", setupobject, envir=rte)
    
    controlobject <- initializecontrolclassobject(list("setupobject")) 
    assign("controlobject", controlobject, envir=rte)
    
    analysislist <- reportslist <- vector("list", 1)
    
      parameterclassobject <- rte$setupobject@parameters
      dataclassobject <- rte$setupobject@data 
      subclassobjects <- getinitializedsubclassobjects(dataclassobject, parameterclassobject) 
      analysisclassobject <- initializeanalysisclassobject(subclassobjects, dataclassobject)
      
      reportclassobject <- initializeReportClass(analysisclassobject)
      analysislist[[1]] <- analysisclassobject
      reportslist[[1]] <- reportclassobject
      runclassobject <- new("RunClass", reports=reportslist, analysis=analysislist)    
      return(runclassobject)
      } # closes data frame
  
    if(class(controlobject)=="list"){
      
      A <- controlobject[[1]]
      B <- controlobject[[2]]
      
      setup1 <- initializesetupclassobject("setup1", defaultParameters, initializedataobject(A))
      setup2 <- initializesetupclassobject("setup2", defaultParameters, initializedataobject(B)) 
      controlcompare <- initializecontrolclassobject(list("setup1", "setup2"))
      
      setupslist <- controlcompare@setups
      analysislist <- reportslist <- vector("list", length(setupslist))
  
      # for each setup
      
      for (i in 1:length(setupslist)){
        parameterclassobject <- getparameters(eval(as.name(setupslist[[i]]))) 
        dataclassobject <- eval(as.name(setupslist[[i]]))@data 
        subclassobjects <- getinitializedsubclassobjects(dataclassobject, parameterclassobject) 
        analysisclassobject <- initializeanalysisclassobject(subclassobjects, dataclassobject)
        reportclassobject <- initializeReportClass(analysisclassobject)
        analysislist[[i]] <- analysisclassobject
        reportslist[[i]] <- reportclassobject
        
      }  
      
      runclassobject <- new("RunClass", reports=reportslist, analysis=analysislist)
      
    } # end list

    if(class(controlobject)=="ControlClass"){
    
    setupslist <- controlobject@setups
    
    analysislist <- reportslist <- vector("list", length(setupslist))
    
    # for each setup
    
    for (i in 1:length(setupslist)){
      parameterclassobject <- getparameters(eval(as.name(setupslist[[i]]))) 
      dataclassobject <- eval(as.name(setupslist[[i]]))@data 
      subclassobjects <- getinitializedsubclassobjects(dataclassobject, parameterclassobject) 
      analysisclassobject <- initializeanalysisclassobject(subclassobjects, dataclassobject)
      reportclassobject <- initializeReportClass(analysisclassobject)
      analysislist[[i]] <- analysisclassobject
      reportslist[[i]] <- reportclassobject
      
    }  
    
    runclassobject <- new("RunClass", reports=reportslist, analysis=analysislist)
    
    } # closes ControlClass
  
  return(runclassobject)
}
  



  
  
  
  
  












