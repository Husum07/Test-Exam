library(plumber)
pr <- plumb("Test-Exam/plumber.R")
pr$run(port=8000)

