library(plumber)
pr <- plumber::plumb("api/plumber.R")
pr$run(port=8000)
