
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Tools for Visualization of Interdependent Data Quality Issues for Faster and More Effective Preprocessing
Demo: a <- preproviz(iris) plotVARCLUST(a) plotCMDS(a) plotVARIMP(a) plotBAR(a)")
}

# Cyclic dependency test in Travis
# options(repos = c(CRAN="http://cran.r-project.org"))


#' @import ggplot2
NULL

#' @importFrom methods setClass setGeneric setMethod
NULL

#' @importFrom utils globalVariables
NULL

range01 <- function(x){(x-min(x))/(max(x)-min(x))} 

globalVariables(c("value", "Var1", "Var2", "X1", "X2", "object.lofscores", "MeanDecreaseAccuracy", "features", "variable", "controlobject123"))