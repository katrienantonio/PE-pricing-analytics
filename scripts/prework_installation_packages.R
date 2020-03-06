packages <- c("tidyverse", "mgcv", "evtree", "rpart", "classInt", "rgdal", "RColorBrewer", "grid", "gridExtra", "visreg", "sf", "tmap", "rgeos", "mapview", "leaflet")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))