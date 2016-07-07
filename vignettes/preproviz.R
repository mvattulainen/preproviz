## ----fig.width=8, fig.height=8, message = FALSE, warnings = FALSE--------
library(preproviz)
result <- preproviz(iris)
plotDENSITY(result)

## ---- message = FALSE, warnings = FALSE----------------------------------
iris2 <- iris
iris2[sample(1:150,30), 1] <- NA # adding missing values
iris2[sample(1:150,30), 5] <- levels(iris2$Species)[2] # adding inconsistency 

## ----fig.width=8, fig.height=8, message = FALSE, warnings = FALSE--------

result <- preproviz(list(iris, iris2))
plotVARCLUST(result)

