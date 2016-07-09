
# ANALYSIS CLASS DEFINITION AND INITIALIZATION ====================================

#' An S4 class representing analysis data  
#'
#' @slot objectname (character) Name of the object 
#' @slot basedata (data frame) A data frame containing the original data
#' @slot numericbasedata (data frame) A data frame containing the original data without class labels.
#' @slot classlabel (factor) A vector of class labels of the original data
#' @slot constructeddata (data frame) Constructed data. Feature vectors from computevalue combined as a data frame
#' @slot minmaxconstructeddata (data frame) Min-max normalized constructed data 
#' @slot combineddata (data frame) Basedata and constructed data combined (note: may include missing values)
#' @slot combinednumericdata (data frame) Basedata and constructed data combined without class labels 
#' @slot longformatmixmaxconstructeddata (data frame) Minmaxconstructeddata in long format
#' @slot distancematrix (matrix) Distance matrix of minmaxconstructeddata
#' @slot dendogram (dendrogram) Variable clusters (note: not in use)
#' @slot lofscores (numeric) A vector of LOF scores
#' @slot cmds (data frame) Classical multidimensional scaling two-dimensional data point computed from minmaxconstructeddata
#' @slot variableimportancedata (data frame) Constructed features and their random forest variable importance scores for predicting classlabel
#' @slot lofsumdata (data frame) mixmax normalized LOF scores of minmaxconstructed data summed with minmax normalized LOF scores of numericbasedata

setClass("AnalysisClass", representation(objectname="character", basedata="data.frame", numericbasedata="data.frame", 
                                         classlabel="factor", constructeddata="data.frame", longformatconstructeddata="data.frame", 
                                         minmaxconstructeddata="data.frame", combineddata="data.frame", numericcombineddata="data.frame", 
                                         longformatminmaxconstructeddata="data.frame", distancematrix="matrix", dendrogram="list", 
                                         lofscores="numeric", cmds="data.frame", variableimportancedata="data.frame", lofsumdata="data.frame"))


# ANALYSIS CLASS INITIALIZATION ====================================

initializeanalysisclassobject <- function(object, dataobject){

objectname <- dataobject@name
print(paste("Data in process:", objectname))

# Subsetting valid features based on function checksubclassobjectvalidity

validvectors <- object[lapply(object, function(x) slot(x, "isvalid"))==TRUE]

## Forming constructed data by combining valid feature vectors

constructeddata <- data.frame(lapply(validvectors, function(x) slot(x, "valuevector")))
colnames(constructeddata) <- unlist(lapply(validvectors, function(x) slot(x, "objectname")))

longformatconstructeddata <- reshape2::melt(as.matrix(constructeddata))

## Normalize data

minmaxconstructeddata <- data.frame(apply(constructeddata, 2, range01))

## Combined data

combineddata <- data.frame(cbind(dataobject@basedata, constructeddata))
numericcombineddata <- combineddata[sapply(combineddata,is.numeric)]

## Class label vector
classlabel <- dataobject@basedata[sapply(dataobject@basedata,is.factor)][,1]

## numericbasedata

numericbasedata <- dataobject@numericdata

## Long format weight data
longformatminmaxconstructeddata <- reshape2::melt(as.matrix(minmaxconstructeddata))

## Compute distance matrix
distancematrix <- as.matrix(dist(minmaxconstructeddata))

## Commpute LOF scores

lofscores <- DMwR::lofactor(minmaxconstructeddata, k=5)

## Compute LOF comparisons
lofscores_base <- DMwR::lofactor(dataobject@imputednumeric, k=5)
lofsumdata <- data.frame(lofsum=range01(lofscores)-range01(lofscores_base), lofbase=range01(lofscores_base), seq=seq(1,nrow(minmaxconstructeddata),1))
lofsumdata <- reshape2::melt(lofsumdata, id="seq")

### CLUSTER ANALYSIS

## Compute multidimensional scaling
cmdsfit <- data.frame(cmdscale(distancematrix,eig=TRUE, k=2)$points) # k is the number of dim

## Compute variable cluster

varclust <- suppressWarnings(ClustOfVar::hclustvar(minmaxconstructeddata))

## Compute variable importance

varimp <- randomForest::randomForest(classlabel ~ ., data=minmaxconstructeddata, ntree=1000, keep.forest=FALSE, importance=TRUE, na.action=na.omit)
variableimportancedata <- data.frame(varimp$importance[,3])
variableimportancedata$features <- rownames(variableimportancedata)
colnames(variableimportancedata)[1] <- "MeanDecreaseAccuracy"

AnalysisObject <- new("AnalysisClass", objectname=objectname, basedata=dataobject@basedata, numericbasedata=dataobject@numericdata,
                  classlabel=classlabel, constructeddata=constructeddata, longformatconstructeddata=longformatconstructeddata, minmaxconstructeddata=minmaxconstructeddata, 
                  combineddata=combineddata, numericcombineddata=numericcombineddata, longformatminmaxconstructeddata=longformatminmaxconstructeddata, 
                  dendrogram=list(varclust), distancematrix=distancematrix, lofscores=lofscores,cmds=cmdsfit, variableimportancedata=variableimportancedata, lofsumdata=lofsumdata)
return(AnalysisObject)
}

# GETTER METHODS FOR ANALYSISCLASS =========================

#' get name of an object
#' 
#' @param object (AnalysisClass or BaseClass)
#' @return (character) name of the object
#' @export

setGeneric("getname", function(object) {
  standardGeneric("getname")
})

#' @rdname getname
#' @keywords internal
setMethod("getname", signature(object = "AnalysisClass"), function(object) {
  return(object@objectname)}
)

#' @rdname getname
#' @keywords internal
setMethod("getname", signature(object = "BaseClass"), function(object) {
  return(object@objectname)}
)

##

#' getbasedata
#' 
#' get basedata, that is the original data frame to be visualized
#' @param object (AnalysisClass)
#' @return (data frame) basedata
#' @export

setGeneric("getbasedata", function(object) {
  standardGeneric("getbasedata")
})


#' @rdname getbasedata
#' @keywords internal
setMethod("getbasedata", signature(object = "AnalysisClass"), function(object) {
  return(object@basedata)}
)

##

#' getnumericbasedata
#' 
#' get numeric data of an object
#' @param object (AnalysisClass)
#' @return (data frame) numeric columns of basedata
#' @rdname getnumericbasedata
#' @export

setGeneric("getnumericbasedata", function(object) {
  standardGeneric("getnumericbasedata")
})

#' @rdname getnumericbasedata
#' @keywords internal
setMethod("getnumericbasedata", signature(object = "AnalysisClass"), function(object) {
  return(object@numericbasedata)}
)

##

#' getconstructeddata
#' 
#' get constructeddata, that is features constructed combined
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) constructed data
#' @export

setGeneric("getconstructeddata", function(object) {
  standardGeneric("getconstructeddata")
})


#' @rdname getconstructeddata
#' @keywords internal
setMethod("getconstructeddata", signature(object = "AnalysisClass"), function(object) {
  return(object@constructeddata)}
)

##

#' get constructed data in long format
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) long format constructed data
#' @export

setGeneric("getlongformatconstructeddata", function(object) {
  standardGeneric("getlongformatconstructeddata")
})

#' @rdname getlongformatconstructeddata
#' @keywords internal
setMethod("getlongformatconstructeddata", signature(object = "AnalysisClass"), function(object) {
  return(object@longformatconstructeddata)}
)

#' get contructed data that have been min-max normalized
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) min-max normalized data
#' @export

setGeneric("getminmaxconstructeddata", function(object) {
  standardGeneric("getminmaxconstructeddata")
})

#' @rdname getminmaxconstructeddata
#' @keywords internal
setMethod("getminmaxconstructeddata", signature(object = "AnalysisClass"), function(object) {
  return(object@minmaxconstructeddata)}
)

##

#' get basedata and constructed data combined
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) combined data
#' @export

setGeneric("getcombineddata", function(object) {
  standardGeneric("getcombineddata")
})

#' @rdname getcombineddata
#' @keywords internal
setMethod("getcombineddata", signature(object = "AnalysisClass"), function(object) {
  return(object@combineddata)}
)

##

#' get numeric columns of combined data 
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) numeric columns combined data
#' @export

setGeneric("getnumericombineddata", function(object) {
  standardGeneric("getnumericombineddata")
})

#' @rdname getnumericombineddata
#' @keywords internal
setMethod("getnumericombineddata", signature(object = "AnalysisClass"), function(object) {
  return(object@numericcombineddata)}
)

##

#' getclasslabels
#' 
#' get class labels of basedata 
#' @param object (AnalysisClass or RunClass)
#' @return (factor) vector of class labels
#' @export

setGeneric("getclasslabels", function(object) {
  standardGeneric("getclasslabels")
})

#' @rdname getclasslabels
#' @keywords internal
setMethod("getclasslabels", signature(object = "AnalysisClass"), function(object) {
  return(object@classlabel)}
)

##

#' get classical multidimensional scaling from minmaxconstructed data 
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) cmds points
#' @export

setGeneric("getcmdsdata", function(object) {
  standardGeneric("getcmdsdata")
})

#' @rdname getcmdsdata
#' @keywords internal
setMethod("getcmdsdata", signature(object = "AnalysisClass"), function(object) {
  return(object@cmds)}
)

##

#' getlongformatminmaxconstructeddata
#' 
#' get long format of minmax normalized constructed data
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) long format data
#' @export

setGeneric("getlongformatminmaxconstructeddata", function(object) {
  standardGeneric("getlongformatminmaxconstructeddata")
})

#' @rdname getlongformatminmaxconstructeddata
#' @keywords internal
setMethod("getlongformatminmaxconstructeddata", signature(object = "AnalysisClass"), function(object) {
  return(object@longformatminmaxconstructeddata)}
)

##

#' getlofscores
#' 
#' get LOF scores minmax normalized constructed data
#' @param object (AnalysisClass or RunClass)
#' @return (numeric) vector of LOF scores
#' @export

setGeneric("getlofscores", function(object) {
  standardGeneric("getlofscores")
})

#' @rdname getlofscores
#' @keywords internal
setMethod("getlofscores", signature(object = "AnalysisClass"), function(object) {
  return(data.frame(object@lofscores))}
)

##

#' get random forest variable importance data
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) variable importance scores and feature names 
#' @export

setGeneric("getvariableimportancedata", function(object) {
  standardGeneric("getvariableimportancedata")
})

#' @rdname getvariableimportancedata
#' @keywords internal
setMethod("getvariableimportancedata", signature(object = "AnalysisClass"), function(object) {
  return(data.frame(object@variableimportancedata))}
)

##

#' getlofsumdata
#' 
#' get LOF scores of min-max normalized constructed data plus numerical imputed base data 
#' @param object (AnalysisClass or RunClass)
#' @return (data frame) variable importance scores and feature names 
#' @export

setGeneric("getlofsumdata", function(object) {
  standardGeneric("getlofsumdata")
})

#' @rdname getlofsumdata
#' @keywords internal
setMethod("getlofsumdata", signature(object = "AnalysisClass"), function(object) {
  return(data.frame(object@lofsumdata))}
)