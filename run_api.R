library(plumber)
pr <- plumber::plumb("GitHub/Test Exam/Test-Exam/plumber.R")
pr$run(port=8000)

