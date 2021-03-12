
# PE-pricing-analytics

by Katrien Antonio, Roel Henckaerts and Roel Verbelen.

Course materials for the *Pricing analytics in R* course in March 2021
in Leuven and Brussels.

📆 March, 2021 <br> 🕗 approx. 3h (in company) and 5h (for MSc students in
class) <br> 📌 IA|BE in Brussels, Data science for non-life insurance
class at KU Leuven

## Goals of the workshop

You’ll work through the essential steps of the implementation in `R` of
the pricing framework proposed in the
[paper](https://www.tandfonline.com/doi/abs/10.1080/03461238.2018.1429300)
“A data driven binning strategy for the construction of insurance tariff
classes”, by Henckaerts, Antonio, Clijsters and Verbelen in Scandinavian
Actuarial Journal (2018). The methodology is illustrated with the same
data set as the one used by Henckaerts et al. (2018).

This paper develops a fully data driven strategy to incorporate
continuous risk factors and geographical information in an insurance
tariff. The framework nicely aligns flexibility with the practical
requirements of an insurance company, the policyholder and the
regulator. The approach combines tools from statistical learning (GAMs,
GLMs) with machine learning (clustering, evolutionary trees).

## Schedule and Course Material

The detailed schedule is subject to small changes.

| Description                                | Lecture material                                                                                                                                      | R script                                                                                          | R solutions                                                                |
| ------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| Prologue, the R universe and preliminaries | [sheets prologue](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html#prologue)               | [script](https://katrienantonio.github.io/PE-pricing-analytics/scripts/0_setup_prep_work.R)       | [solutions](https://katrienantonio.github.io/PE-pricing-analytics/scripts) |
| Data set                                   | [sheets data set](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html#data-sets)              | [script](https://katrienantonio.github.io/PE-pricing-analytics/scripts/1_data_exploration.R)      | [solutions](https://katrienantonio.github.io/PE-pricing-analytics/scripts) |
| Spatial data and maps                      | [sheets data set](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html#data-sets)              | [script](https://katrienantonio.github.io/PE-pricing-analytics/scripts/2_spatial_data_maps.R)     | [solutions](https://katrienantonio.github.io/PE-pricing-analytics/scripts) |
| Model building                             | [sheets model building](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html#model-building)   | [script](https://katrienantonio.github.io/PE-pricing-analytics/scripts/3_basics_model_building.R) | [solutions](https://katrienantonio.github.io/PE-pricing-analytics/scripts) |
| From GAM to GLM                            | [sheets from GAM to GLM](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html#from-gam-to-glm) | [script](https://katrienantonio.github.io/PE-pricing-analytics/scripts/4_clustering_binning.R)    | [solutions](https://katrienantonio.github.io/PE-pricing-analytics/scripts) |

You can access the lecture sheets 9 (in html) via
[pricing-analytics-sheets](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_with_GAMs_and_GLMs_IABE.html)
and in pdf via
[pricing-analytics-sheets-pdf](https://katrienantonio.github.io/PE-pricing-analytics/sheets/pricing_analytics_lecture_sheets_in_pdf.pdf).

An older documentation of the same workshop is
[here](https://katrienantonio.github.io/PE-pricing-analytics/background/2019_04_APC_Pricing_analytics_in_R.html).

## Software requirements

Please bring a laptop with a recent version of R and RStudio installed.
Make sure you can connect your laptop to the internet (or download the
course material one day before the start of the workshop). You will
need:

  - R (at least 3.5.2 <https://cloud.r-project.org/bin/windows/base/> )
  - RStudio (
    <https://www.rstudio.com/products/rstudio/download/#download> )

You should install and load the packages that will be used throughout
the workshop. You can use the following instructions to install (if
necessary) and load the packages. These instructions are also available
in `prework_installation_packages.R` from the `scripts` folder.

``` r
packages <- c("tidyverse", "mgcv", "evtree", "classInt", "rgdal", "RColorBrewer", "grid", "gridExtra", "visreg", "sf", "tmap", "rgeos", "mapview", "leaflet")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
```

## RStudio Cloud

We provide a back-up plan in case your local installation of R (and the
necessary packages) is not working properly. Please join our project on
RStudio Cloud via the link posted on TOLEDO.

After creating an account for RStudio you will be able to work with the
scripts and data sets in the cloud.

## Instructor

<img src="img/Katrien.jpg" width="110"/>

<p align="justify">

[Katrien Antonio](https://katrienantonio.github.io/) is professor in
insurance data science at KU Leuven and associate professor at
University of Amsterdam. She teaches courses on data science for
insurance, life and non-life insurance mathematics and loss models.
Research-wise Katrien puts focus on pricing, reserving and fraud
analytics, as well as mortality dynamics.

## Let’s go\!

You are now ready to load the data and build predictive models.
