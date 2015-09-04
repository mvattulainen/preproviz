library(preproviz)
context("Class of output")

testdata <- data.frame(matrix(rbinom(3*20, 1, .5), ncol=3),class=sample(letters[1:2], 20, replace=TRUE))
testdataobject <- initializedataobject(testdata)
testmissingvalueshare <- initializesubclassobject("MissingValueShare", testdataobject)

test_that("Class of output", {expect_is(testdataobject, "DataClass")})
test_that("Class of output", {expect_is(testmissingvalueshare, "MissingValueShare")})
