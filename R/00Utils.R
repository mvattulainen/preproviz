
library(ggplot2)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Quick start: a<- preproviz(iris), plotBAR(a)")
}

options(repos = c(CRAN="http://cran.r-project.org"))


#' @import ggplot2
NULL

#' @importFrom methods setClass setGeneric setMethod
NULL

#' @importFrom utils globalVariables
NULL

range01 <- function(x){(x-min(x))/(max(x)-min(x))} 

globalVariables(c("value", "Var1", "Var2", "X1", "X2", "object.lofscores", "MeanDecreaseAccuracy", "features", "variable", "controlobject123"))