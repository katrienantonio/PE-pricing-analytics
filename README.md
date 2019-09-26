# PE-pricing-analytics

## Goals of the workshop

You'll work through the essential steps of the implementation in `R` of the methodology outlined in the paper "A data driven binning strategy for the construction
of insurance tariff classes", by Henckaerts, Antonio, Clijsters and Verbelen in Scandinavian Actuarial Journal (2018). The methodology is illustrated with the same data set as the one used by Henckaerts et al. (2018).

This paper develops a fully data driven strategy to incorporate continuous risk
factors and geographical information in an insurance tariff. The framework
nicely aligns flexibility with the practical requirements of an
insurance company, the policyholder and the regulator. The approach combines tools from statistical learning (GAMs, GLMs) with machine learning (clustering, evolutionary trees).

## Set-up

First of all, you specify the path where data and output will be stored. Pay attention to the way how directories are specified in `R` (with forward slash or double back slash)
```{r}
path <- file.path('C:/Users/u0043788/Dropbox/APC Module Data Science/computer labs/pricing analytics')
```
Within this folder you store the data set `P&Cdata.txt`. You also create a subfolder called `Shape file Belgie postcodes` where you unpack the zip file with the shape file of Belgium at postcode level. 
You now download, install and load the packages that will be used throughout the workshop: `data.table`, `dplyr`, `mgcv`, `evtree`, `classInt`, `rgdal`, `RColorBrewer`, `ggplot2`, `ggmap`, `grid` and `gridExtra`. You can use the following instructions to install (if necessary) and load the packages. 

```{r, eval=TRUE}
packages <- c("data.table", "dplyr", "mgcv", "evtree", "classInt", "rgdal", "RColorBrewer", "ggplot2", "ggmap", "grid", "gridExtra", "visreg")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
```
## Let's go!

You are now ready to load the data and build predictive models. 