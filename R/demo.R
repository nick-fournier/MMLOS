

library(data.table)

source("R/main_functions.R")
source("R/pedestrians.R")
source("R/bicycles.R")
source("R/automobiles.R")


#Load data in, defaults to template
dat <- func.loaddat("template")

tab <- func.loadtab()


func.MMLOS(dat)
