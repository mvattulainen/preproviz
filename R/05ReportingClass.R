
## USES: Analysis

#' An S4 class representing visualizations
#'
#' @slot density density plot of all constructed features
#' @slot heatmap heatmap
#' @slot cmds classical multidimensional scaling
#' @slot variableclusters hiearchical variable clustering
#' @slot outliers LOF outlier scores
#' @slot varimp variable importance
#' @slot lofsum sum of LOF scores
#' @export 

setClass("ReportClass", representation(density="list", heatmap="list", cmds="list",  variableclusters="list", outliers="list", varimp="list", lofsum="list"))

### CHARACTERIZATION PLOTS

initializeReportClass <- function(object){

# Creating an environment in order to input object name to ggplot object outside data
  
genv <- new.env()    
name <- object@objectname
genv$name <- name
genv$object <- object

## Bar plots
g_density <- ggplot2::ggplot(getlongformatconstructeddata(object), aes(value), environment=genv) 
g_density <- g_density + geom_density() + facet_wrap(~Var2, scales="free") + theme_bw() + ggtitle(name)

## Heat map
g_heatmap <- ggplot2::ggplot(getlongformatminmaxconstructeddata(object), aes(y=Var1,x=Var2), environment=genv) 
g_heatmap <- g_heatmap + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="black") + theme_bw() + ggtitle(name) + coord_flip()

### CLUSTERING PLOTS

## Multidimensional scaling

g_scatter <- ggplot2::ggplot(getcmdsdata(object), aes(x=X1, y=X2), environment=genv) + geom_point(shape=1)
g_scatter <- g_scatter + theme_bw() + ggtitle(name)

## Hieararchical clustering ## MINOR ISSUE: NOTE ANALYSIS DONE HERE AND NOT IN ANALYSIS CLASS

g_dendro <- suppressWarnings(ggdendro::ggdendrogram(ClustOfVar::hclustvar(getminmaxconstructeddata(object)), rotate = FALSE, size = 2) + labs(title=genv$name))
g_dendro <- g_dendro + coord_flip() 

## LOF Scores

g_outlier <- ggplot2::ggplot(getlofscores(object), aes(x=object.lofscores), environment=genv) 
g_outlier <- g_outlier + geom_density()+ theme_bw() + ggtitle(name) + xlab("LOF score")

## Variable importance

g_varimp <- ggplot2::ggplot(getvariableimportancedata(object), aes(y=MeanDecreaseAccuracy, x=features), environment=genv) 
g_varimp <- g_varimp + geom_bar(stat="identity") + coord_flip() + theme_bw() + ggtitle(name)

##

## Lofsum

g_lofsum <- ggplot2::ggplot(getlofsumdata(object), aes(x=seq, y=variable), environment=genv) + geom_tile(aes(fill=value)) 
g_lofsum <- g_lofsum + scale_fill_gradient(low="white", high="black") + theme_bw() + ggtitle(name) + ylab("") + theme(axis.title.y = element_blank())

ReportClass <- new("ReportClass", density=list(g_density), heatmap=list(g_heatmap), cmds=list(g_scatter), variableclusters=list(g_dendro), outliers=list(g_outlier), varimp=list(g_varimp), lofsum=list(g_lofsum))
return(ReportClass)
}

## METHODS

#' generic function for plotting classical multidimensional scaling
#' @param object (ReportClass or RunClass)
#' @rdname plotCMDS
#' @export

setGeneric("plotCMDS", function(object) {
  standardGeneric("plotCMDS")
})

 
#' @rdname plotCMDS
setMethod("plotCMDS", signature(object = "ReportClass"), function(object) {
  object@cdms}
)

#' @rdname plotCMDS
setMethod("plotCMDS", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "cmds"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
  }
)

###

#' generic function for  plotting density estimates of constructed features
#' @param object (ReportClass or RunClass)
#' @rdname plotDENSITY
#' @export

setGeneric("plotDENSITY", function(object) {
  standardGeneric("plotDENSITY")
})


#' @rdname plotDENSITY
setMethod("plotDENSITY", signature(object = "ReportClass"), function(object) {
  object@density}
)

#' @rdname plotDENSITY
setMethod("plotDENSITY", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "density"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

## OUTLIERS

#' generic function for plotting density of LOF scores
#' @param object (ReportClass or RunClass)
#' @rdname plotOUTLIERS
#' @export

setGeneric("plotOUTLIERS", function(object) {
  standardGeneric("plotOUTLIERS")
})

#' @rdname plotOUTLIERS
setMethod("plotOUTLIERS", signature(object = "ReportClass"), function(object) {
  object@outliers}
)

#' @rdname plotOUTLIERS
setMethod("plotOUTLIERS", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "outliers"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

##

#' generic function for plotting variable clusters
#' @param object (ReportClass or RunClass)
#' @rdname plotVARCLUST
#' @export

setGeneric("plotVARCLUST", function(object) {
  standardGeneric("plotVARCLUST")
})

#' @rdname plotVARCLUST
setMethod("plotVARCLUST", signature(object = "ReportClass"), function(object) {
  object@variableclusters}
)

#' @rdname plotVARCLUST
setMethod("plotVARCLUST", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "variableclusters"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

### HEATMAP

#' generic function for plotting heatmap
#' @param object (ReportClass or RunClass)
#' @rdname plotHEATMAP
#' @export

setGeneric("plotHEATMAP", function(object) {
  standardGeneric("plotHEATMAP")
})

#' @rdname plotHEATMAP
setMethod("plotHEATMAP", signature(object = "ReportClass"), function(object) {
  object@heatmap}
)

#' @rdname plotHEATMAP
setMethod("plotHEATMAP", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "heatmap"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

## VARIMP

#' generic function for plotting variable importance 
#' @param object (ReportClass or RunClass)
#' @rdname plotVARIMP
#' @export

setGeneric("plotVARIMP", function(object) {
  standardGeneric("plotVARIMP")
})

#' @rdname plotVARIMP
setMethod("plotVARIMP", signature(object = "ReportClass"), function(object) {
  object@varimp}
)

#' @rdname plotVARIMP
setMethod("plotVARIMP", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "varimp"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

## LOFSUM

#' generic function for plotting lof sum of constructed features
#' @param object (ReportClass or RunClass)
#' @rdname plotLOFSUM
#' @export

setGeneric("plotLOFSUM", function(object) {
  standardGeneric("plotLOFSUM")
})

#' @rdname plotLOFSUM
setMethod("plotLOFSUM", signature(object = "ReportClass"), function(object) {
  object@lofsum}
)

#' @rdname plotLOFSUM
setMethod("plotLOFSUM", signature(object = "RunClass"), function(object) {
  listcmds <- lapply(object@reports, function(x) slot(x, "lofsum"))
  listcmds <- lapply(listcmds, `[[`, 1)
  do.call(gridExtra::grid.arrange,  listcmds)
}
)

