#' @include 04ControlClass.R 
#' @include 01BaseClass.R
#' @include 05ReportingClass.R
NULL

#' the MAIN execution function
#' 
#' preproviz() can run either a ControlClass object (complex setup) or data frame (simple setup). 
#' Output can be plotted with plot-methods, e.g. plotVARCLUST(x)
#' 
#' @param controlobject (data frame or ControlClass) 
#' @return (RunClass) object
#' @export

preproviz <- function(controlobject){
   
    # initializing default objects in case of data frame as controlobject argument
    
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
  



  
  
  
  
  












