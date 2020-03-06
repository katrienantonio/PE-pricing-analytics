# PE-pricing-analytics

## Goals of the workshop

You'll work through the essential steps of the implementation in `R` of the methodology outlined in the paper "A data driven binning strategy for the construction
of insurance tariff classes", by Henckaerts, Antonio, Clijsters and Verbelen in Scandinavian Actuarial Journal (2018). The methodology is illustrated with the same data set as the one used by Henckaerts et al. (2018).

This paper develops a fully data driven strategy to incorporate continuous risk
factors and geographical information in an insurance tariff. The framework
nicely aligns flexibility with the practical requirements of an
insurance company, the policyholder and the regulator. The approach combines tools from statistical learning (GAMs, GLMs) with machine learning (clustering, evolutionary trees).

## Set-up

You should nstall and load the packages that will be used throughout the workshop. You can use the following instructions to install (if necessary) and load the packages. These instructions are also available in `prework_installation_packages.R` from the `scripts` folder.

```{r, eval=TRUE}
packages <- c("tidyverse", "mgcv", "evtree", "classInt", "rgdal", "RColorBrewer", "grid", "gridExtra", "visreg", "sf", "tmap", "rgeos", "mapview", "leaflet")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
```
If you do not manage the install and use the packages on your local machine, then please join our project on RStudio Cloud via the following link

[https://rstudio.cloud/spaces/55584/join?access_code=hnHmPmSNsbu8BdEBJkeMXNPtHBelpnnJBAmF9XTH](https://rstudio.cloud/spaces/55584/join?access_code=hnHmPmSNsbu8BdEBJkeMXNPtHBelpnnJBAmF9XTH)

After creating an account for RStudio you will be able to work with the scripts and data sets in the cloud. 


## Lecture sheets

You can access the lecture sheets via [pricing-analytics-sheets](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs.html). These are under construction and will be updated!

An older documentation of the same workshop is [here](https://katrienantonio.github.io/PE-pricing-analytics/background/2019_04_APC_Pricing_analytics_in_R.html).


## Let's go!

You are now ready to load the data and build predictive models. 