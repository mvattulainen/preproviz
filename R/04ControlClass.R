
# CONTROL COMPONENT =================================================


# RUNCLASS =====================

#' RunClass 
#' 
#' RunClass is an class contain ReportClass and AnalysisClass objects as separate lists.
#' A RunClass object is the output of running the main function preproviz() and can be
#' studied with either get methods (AnalysisClass objects) or plot methods (ReportClass
#' objects).
#'  
#' @slot reports A list of ReportClass objects
#' @slot analysis A list of AnalysisClass object
#' @return A RunClass object

setClass("RunClass", representation(reports="list", analysis="list")) #

#' getconstructeddata RunClass
#' @describeIn getconstructeddata

setMethod("getconstructeddata", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "constructeddata"))
}
)

#' getbasedata RunClass
#' @describeIn getbasedata

setMethod("getbasedata", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "basedata"))
}
)

#' getminmaxconstructeddata RunClass
#' @describeIn getminmaxconstructeddata

setMethod("getminmaxconstructeddata", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "minmaxconstructeddata"))
}
)

#' getcombineddata RunClass
#' @describeIn getcombineddata

setMethod("getcombineddata", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "combineddata"))
}
)

#' getnumericombineddata RunClass
#' @describeIn getnumericombineddata

setMethod("getnumericombineddata", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "numericombineddata"))
}
)

#' getclasslabels RunClass
#' @describeIn getclasslabels

setMethod("getclasslabels", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@analysis, function(x) slot(x, "classlabel"))
}
)

# CONTROLCLASS =====================

#' A ControlClass is an class containing SetUpClass objects.  
#'
#' @slot setups A list of SetUpClass objects 

setClass("ControlClass", representation(setups="list"))

#' initializecontrolclassobject
#'
#' initializecontrolclassobject is a constructor function for intializing ControlClass objects. 
#' @param setups (list) Name of SetUpClass objects 
#' @return (ControlClass) object 
#' @export

initializecontrolclassobject <- function(setups){
  a <- new("ControlClass")
  a@setups <- setups
  return(a)
}

# PARAMETERCLASS =====================

#' ParameterClass 
#' 
#' ParameterClass is a class containing a list of sub class objects (i.e. constructed features, inhereted
#' from BaseClass). A ParameterClass object in a SetUpClass object defines which constructed features are computed 
#' from a DataClass object  
#'
#' @slot parameters A list of sub class objects

setClass("ParameterClass", representation(parameters="list"))

#' initializeparameterclassobject
#'
#' initializeparameterclassobject is a constructor function for intializing ParameterClass objects. 
#' @param parameters (list) Name of sub classes 
#' @return (ParameterClass) object 
#' @export

initializeparameterclassobject <- function(parameters){
  a <- new("ParameterClass")
  a@parameters <- parameters
  return(a)
}


#' getparameters
#' 
#' getparameters is a method for getting the list of parameters from a ParameterClass object or a Parameter object from SetUpClass object
#' @param object (ParameterClass or SetUpClass)
#' @rdname getparameters
#' @export

setGeneric("getparameters",function(object){standardGeneric("getparameters")}) ## VALUE

#' getparameters ParameterClass
#' @describeIn getparameters

setMethod("getparameters", signature(object = "ParameterClass"), function(object) {
  return(object@parameters)}
)

# DATACLASS =====================

#' DataClass 
#' 
#' DataClass is a class containing data.  
#' DataClass object can be initialized only for a data frame that has a) one class label columns of class 'factor' and
#' b) other columns are of type 'numeric'
#'
#' @slot name (character) name of the setup object
#' @slot basedata (data frame) original data to be visualized 
#' @slot imputedbase (data frame) missing value in original data imputed with Knn imputation 
#' @slot numericdata (data frame) numeric columns of original data
#' @slot imputednumeric (data frame) imputed numeric columns
#' @export

setClass("DataClass", representation(name="character", basedata="data.frame", imputedbase="data.frame", numericdata="data.frame", imputednumeric="data.frame"))

#' initializedataObject 
#' 
#' initializedataobject is a constructor function for initializing aDataClass object from original data frame
#' @param data (data frame)
#' @return (DataClass)
#' @export

initializedataobject <- function(data){

  if(class(data)!="data.frame"){stop("Argument 'data' is not of class data frame.")}
  if(sum(sapply(data, is.factor)==TRUE)!=1) {stop("Argument 'data' must have one and only one factor column")}
  if(sum(sapply(data, is.numeric)==TRUE)!=ncol(data)-1) {stop("Argument 'data' must have only numeric columns and one factor column")}
  
  dataobject <- new("DataClass", basedata=data)
  dataobject@name <- as.character(substitute(data))
  suppressWarnings(dataobject@imputedbase <- DMwR::knnImputation(data, k=5))
  dataobject@imputednumeric <- dataobject@imputedbase[sapply(dataobject@imputedbase,is.numeric)]
  dataobject@numericdata <- data[sapply(data,is.numeric)]

  return(dataobject)

}

# SETUPCLASS =====================

#' SetUpClass 
#' 
#' SetUpClass is an class containing a DataClass object, a ParameterClass object and the name of the SetUpClass object  
#'
#' @slot data (DataClass) 
#' @slot parameters (ParameterClass) 
#' @slot objectname (character)

setClass("SetUpClass", representation(objectname="character", data="DataClass", parameters="ParameterClass"))

#' initializesetupclassobject 
#' 
#' initializesetupclassobject is a constructor function for initializing a SetUpClass object
#' @param objectname (character) Name of the setup
#' @param parameterobject (ParameterClass)
#' @param dataobject (DataClass)
#' @export

initializesetupclassobject <- function(objectname, parameterobject, dataobject){
  a <- new("SetUpClass")
  a@objectname <- objectname
  a@parameters <- parameterobject
  a@data <- dataobject
  return (a)
}


#' getparameters SetUpClass
#' @describeIn getparameters

setMethod("getparameters", signature(object = "SetUpClass"), function(object) {
  return(object@parameters)}
)

#setGeneric("saveSetUp",function(object){standardGeneric("saveSetUp")})
#
#setMethod("saveSetUp", signature(object = "SetUpClass"), function(object) {
#  fn <- tempfile()
#  save(object, ascii=FALSE, file=fn)
#}
#  )




###





